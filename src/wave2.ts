"use strict";

const SHARED_SIZE = 0x2000;
const NUM_TRI = 1024; // how many we can render at most
const TRIMEM_SIZE = 4 * NUM_TRI; // words
const CONNECT_TO = 'ws://127.0.0.1:23192/';

function hex(v:number) {
	return v.toString(16).padStart(2, '0');
}

const vert_source = `#version 300 es
precision highp float;
in vec4 position;
in uvec4 col;
out vec4 v_col;
out vec4 v_tex;
uniform vec4 u_xfm;
uniform vec4 u_screen;

vec2 apply_tx(vec2 on, vec4 by) {
	return on.xx * by.xy + on.yy * by.zw;
}
const float unspin = 0.006135924152;
void main() {
	uvec3 ic = uvec3((col.w >> 11) & 0x1fu, (col.w >> 5) & 0x3fu, col.w & 0x1fu);
	vec3 cv = vec3(ic.xyz) * vec3(0.03125, 0.015625, 0.03125);
	v_col = vec4(cv, max(max(cv.x, cv.y), cv.z));
	vec2 inst_pos = vec2(col.xy);
	vec2 spin = vec2(col.z & 0x3ffu, col.z & 0x3ffu) * vec2(unspin);
	vec2 rot = vec2(sin(spin.x), cos(spin.y));
	vec2 pix_pos = apply_tx(apply_tx(position.xy, u_xfm), vec4(rot.yx * vec2(1,-1), rot.xy)) + inst_pos;
	vec2 v_uv = (inst_pos.xy * u_screen.xy + vec2(-1,1) + u_screen.zw) * vec2(0.5,-0.5) + vec2(0.5, 0.5);
	v_tex = vec4(v_uv, 0, 0);
	gl_Position = vec4(pix_pos * u_screen.xy + vec2(-1,1) + u_screen.zw, 0, 1);
}
`;
const frag_source = `#version 300 es
precision mediump float;
in vec4 v_col;
in vec4 v_tex;
out vec4 color;
uniform sampler2D i_texture;
void main() {
	vec4 t_col = texture(i_texture, v_tex.xy);
	color = vec4(v_col.xyz, 1);
}`;
const vert_source_bg = `#version 300 es
precision highp float;
in vec4 position;
out vec2 v_uv;
uniform vec4 u_xfm;
uniform vec4 u_screen;

void main() {
	vec2 pix_pos = position.xy;
	vec2 screen_pos = pix_pos * u_screen.xy;
	v_uv = position.zw;
	gl_Position = vec4(screen_pos + vec2(-1,1) + u_screen.zw, 0, 1);
}`;
const frag_source_bg = `#version 300 es
precision mediump float;
in vec2 v_uv;
out vec4 color;
uniform sampler2D i_texture;
void main() {
	vec4 t_col = texture(i_texture, v_uv);
	color = vec4(t_col.xyz, max(max(t_col.x, t_col.y), t_col.z) * 0.8);
}`;
class WaveSys {
	connect() {
		let ws = this.ws = new WebSocket(CONNECT_TO);
		ws.binaryType = 'arraybuffer';
		ws.addEventListener("open", ()=>{ ws.send("vm"); });
		ws.addEventListener("close", ()=>{
			delete this.ws;
			this.reconnect_delay = 5;
		});
		ws.addEventListener("message", (e)=>this.onmessage(e));
	}
	ws: WebSocket | undefined;
	ctx: WebGL2RenderingContext;
	canvas: HTMLCanvasElement;
	backing_memory = new Uint8Array(SHARED_SIZE * 2 + TRIMEM_SIZE * 2);
	tri_memory = new Int16Array(this.backing_memory.buffer, SHARED_SIZE*2, TRIMEM_SIZE);
	memory = new Uint16Array(this.backing_memory.buffer, 0, SHARED_SIZE + TRIMEM_SIZE);
	test_delay = 0;
	reconnect_delay = 0;

	u_screen_id: WebGLUniformLocation | null = null;
	bg_u_screen_id: WebGLUniformLocation | null = null;
	u_xfm_id: WebGLUniformLocation | null = null;
	bg_texture: WebGLTexture;
	vertex_buffer: WebGLBuffer;
	instance_buffer: WebGLBuffer;
	index_buffer: WebGLBuffer;
	prog: WebGLProgram;
	bg_prog: WebGLProgram;
	rb_color: WebGLRenderbuffer;
	rb_depth: WebGLRenderbuffer;
	onmessage(e: MessageEvent<ArrayBuffer>) {
		let msg = new Uint8Array(e.data);
		let offset = 0;
		let copy = 0;
		let i = 0;
		let meme = this.memory;
		let meme_limit = meme.length;
		uh: while((i+1) < msg.length) {
			offset += msg[i];
			copy = msg[i+1];
			i += 2;
			while((i+1) < msg.length && copy > 0) {
				let v = msg[i] | (msg[i+1] << 8);
				meme[offset] = v;
				i+=2; offset++; copy--;
				if(offset >= meme_limit) break uh;
			}
		}
		const ctx = this.ctx;
		ctx.texSubImage2D(ctx.TEXTURE_2D, 0, 0, 0, 256, 32, ctx.RGB, ctx.UNSIGNED_SHORT_5_6_5, meme);
		ctx.bufferSubData(ctx.ARRAY_BUFFER, 0, this.backing_memory, SHARED_SIZE*2, 8*NUM_TRI);
		this.gl_frame();
	}
	gl_frame() {
		const ctx = this.ctx;
		ctx.clearColor(0, 0, 0, 0);
		ctx.clear(ctx.COLOR_BUFFER_BIT | ctx.DEPTH_BUFFER_BIT);
		ctx.useProgram(this.bg_prog);
		ctx.drawElements(ctx.TRIANGLES, 6, ctx.UNSIGNED_INT, 4*3); // "background"
		ctx.useProgram(this.prog);
		ctx.uniform4f(this.u_xfm_id, 1, 0, 0, 1);
		ctx.drawElementsInstanced(ctx.TRIANGLES, 3, ctx.UNSIGNED_INT, 0, 1024); // foreground triangles
	}
	interval() {
		if(this.ws) {
		} else if(this.reconnect_delay <= 0) {
			this.connect();
		} else {
			this.reconnect_delay--;
		}
		this.gl_frame();
	}
	make_shader_pls(what: number, thing: string): WebGLShader {
		let s = this.ctx.createShader(what); // fragment or vertex shader
		if(!s) throw Error('eek! create Shader');
		this.ctx.shaderSource(s, thing);
		this.ctx.compileShader(s);
		let status = this.ctx.getShaderParameter(s, this.ctx.COMPILE_STATUS) as boolean;
		if(!status) {
			let info = this.ctx.getShaderInfoLog(s);
			console.error('build shader: ', info);
			throw Error('eek! compile Shader');
		}
		return s;
	}
	link_shader_pls(vert: string, frag: string) {
		let vert_shader = this.make_shader_pls(this.ctx.VERTEX_SHADER, vert);
		let frag_shader = this.make_shader_pls(this.ctx.FRAGMENT_SHADER, frag);
		let p = this.ctx.createProgram();
		if(!p) throw Error('eek! create program');
		this.ctx.attachShader(p, vert_shader);
		this.ctx.attachShader(p, frag_shader);
		this.ctx.linkProgram(p);
		if(!(this.ctx.getProgramParameter(p, this.ctx.LINK_STATUS) as boolean)) {
			let info = this.ctx.getProgramInfoLog(p);
			console.error('link shaders:', info);
			throw Error('eek! link shaders');
		}
		this.ctx.detachShader(p, vert_shader);
		this.ctx.detachShader(p, frag_shader);
		this.ctx.deleteShader(vert_shader);
		this.ctx.deleteShader(frag_shader);
		return p;
	}

	constructor(canvas: HTMLCanvasElement, context: WebGL2RenderingContext) {
		this.canvas = canvas;
		const ctx = this.ctx = context;
		{
			let r = ctx.createRenderbuffer() || (()=>{throw Error('createRenderBuffer');})();
			ctx.bindRenderbuffer(ctx.RENDERBUFFER, r);
			ctx.renderbufferStorage(ctx.RENDERBUFFER, ctx.RGBA8, this.canvas.width, this.canvas.height);
			this.rb_color = r;
		}
		ctx.depthFunc(ctx.LEQUAL);
		ctx.blendFunc(ctx.SRC_ALPHA, ctx.ONE_MINUS_SRC_ALPHA);
		ctx.enable(ctx.BLEND);
		this.prog = this.link_shader_pls(vert_source, frag_source);
		this.bg_prog = this.link_shader_pls(vert_source_bg, frag_source_bg);
		this.u_screen_id = ctx.getUniformLocation(this.prog, "u_screen");
		this.bg_u_screen_id = ctx.getUniformLocation(this.bg_prog, "u_screen");
		this.u_xfm_id = ctx.getUniformLocation(this.prog, "u_xfm");
		let w = 2 / this.canvas.width;
		let h = -2 / this.canvas.height;
		ctx.useProgram(this.bg_prog);
		ctx.uniform4f(this.bg_u_screen_id, w, h, 0, 0); // size and position of the objects
		ctx.uniform1i(ctx.getUniformLocation(this.bg_prog, "i_texture"), 0);
		ctx.useProgram(this.prog);
		ctx.uniform4f(this.u_screen_id, w, h, -16 * w, h * -16); // size and position of the objects
		ctx.uniform4f(this.u_xfm_id, 1, 0, 0, 1);
		ctx.enableVertexAttribArray(0);
		ctx.enableVertexAttribArray(1);
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.vertex_buffer = buf;
			ctx.bindBuffer(ctx.ARRAY_BUFFER, buf);
			const box_top = 80;
			let vertex_data = new Float32Array([
				// the Triangle
				0,-12,0,0,
				7,12,0,0,
				-7,12,0,0,
				// the box
				0,box_top+0,0,0,
				0,box_top+240,0,1,
				1920,box_top+0,1,0,
				1920,box_top+240,1,1,
				//
			]);
			ctx.bufferData(ctx.ARRAY_BUFFER, vertex_data, ctx.STATIC_DRAW);
			ctx.vertexAttribPointer(0, 4, ctx.FLOAT, false, 4*4, 0);
		}
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.vertex_buffer = buf;
			ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, buf);
			let vertex_data = new Uint32Array([ 0,2,1, 3,4,5, 5,4,6, ]);
			ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, vertex_data, ctx.STATIC_DRAW);
		}
		{
			this.bg_texture = ctx.createTexture() || (()=>{throw Error('eek! texture');})();
			ctx.activeTexture(ctx.TEXTURE0);
			ctx.bindTexture(ctx.TEXTURE_2D, this.bg_texture);
			ctx.texParameteri(ctx.TEXTURE_2D, ctx.TEXTURE_MIN_FILTER, ctx.NEAREST);
			ctx.texParameteri(ctx.TEXTURE_2D, ctx.TEXTURE_MAG_FILTER, ctx.NEAREST);
			ctx.texStorage2D(ctx.TEXTURE_2D, 1, ctx.RGB565, 256, 32);
		}
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.instance_buffer = buf;
			ctx.bindBuffer(ctx.ARRAY_BUFFER, buf);
			ctx.bufferData(ctx.ARRAY_BUFFER, TRIMEM_SIZE * 2, ctx.DYNAMIC_DRAW);
			ctx.vertexAttribIPointer(1, 4, ctx.UNSIGNED_SHORT, 4*2, 0);
		}
		ctx.vertexAttribDivisor(1, 1);
	}
};
let sys: WaveSys;

function overlay_main() {
	let canvas = document.getElementById('render') as HTMLCanvasElement;
	let ctx = canvas.getContext('webgl2');
	if(!ctx) throw Error("OOF");
	sys = new WaveSys(canvas, ctx);
	setInterval(function() { sys.interval() }, 1000);
}

