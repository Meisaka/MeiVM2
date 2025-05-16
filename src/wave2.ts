"use strict";

const SHARED_SIZE = 0x2000;
const NUM_TRI = 1024; // how many we can render at most
const TRIMEM_SIZE = 4 * NUM_TRI; // words
const TRIMEM_OFFSET = SHARED_SIZE * 2;
const IDENTMEM_OFFSET = TRIMEM_OFFSET + TRIMEM_SIZE * 2;
const MEM_BUFFER_SIZE = IDENTMEM_OFFSET + TRIMEM_SIZE * 2;

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
const float unspin = 1.917475984857e-4;
void main() {
	uvec3 ic = uvec3((col.w >> 11) & 0x1fu, (col.w >> 5) & 0x3fu, col.w & 0x1fu);
	vec3 cv = vec3(ic.xyz) * vec3(0.03125, 0.015625, 0.03125);
	v_col = vec4(cv, max(max(cv.x, cv.y), cv.z));
	vec2 inst_pos = vec2(col.xy);
	float spin = float(col.z & 0x7fffu) * unspin;
	vec2 rot = vec2(-sin(spin), cos(spin));
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
const vert_ident_source = `#version 300 es
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
void main() {
	uvec3 ic = uvec3((col.w >> 11) & 0x1fu, (col.w >> 5) & 0x3fu, col.w & 0x1fu);
	vec3 cv = vec3(ic.xyz) * vec3(0.03125, 0.015625, 0.03125);
	v_col = vec4(vec3(0.5) + cv * 0.50, max(max(cv.x, cv.y), cv.z));
	vec2 inst_pos = vec2(col.xy);
	vec2 pix_pos = apply_tx(position.xy, u_xfm) + inst_pos;
	vec2 v_uv = (inst_pos.xy * u_screen.xy + vec2(-1,1) + u_screen.zw) * vec2(0.5,-0.5) + vec2(0.5, 0.5);
	v_tex = vec4(v_uv, position.zw);
	gl_Position = vec4(pix_pos * u_screen.xy + vec2(-1,1) + u_screen.zw, 0, 1);
}
`;
const frag_ident_source = `#version 300 es
precision mediump float;
in vec4 v_col;
in vec4 v_tex;
out vec4 color;
uniform sampler2D i_texture;
void main() {
	//vec4 t_col = texture(i_texture, v_tex.xy);
	vec2 rel_coords = smoothstep(0.0, 0.5, abs(v_tex.zw - vec2(0.5)));
	vec2 alpha = step(0.95, rel_coords);
	float f_alpha = min(1.0, dot(alpha, alpha));
	vec2 inner = step(0.7, rel_coords);
	f_alpha = f_alpha * inner.x * inner.y;
	color = vec4(v_col.xyz, f_alpha);
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
	float intensity = max(max(t_col.x, t_col.y), t_col.z);
	color = vec4(t_col.xyz, intensity * 0.8);
}`;
class WaveSys {
	connect() {
		let ws = this.ws = new WebSocket(CONNECT_TO);
		ws.binaryType = 'arraybuffer';
		ws.addEventListener("open", ()=>this.onopen());
		ws.addEventListener("close", ()=>this.onclose());
		ws.addEventListener("error", ()=>this.onerror());
		ws.addEventListener("message", (e)=>this.onmessage(e));
	}
	ws: WebSocket | undefined;
	ctx: WebGL2RenderingContext;
	canvas: HTMLCanvasElement;
	backing_memory = new Uint8Array(MEM_BUFFER_SIZE);
	tri_memory = new Int16Array(this.backing_memory.buffer, TRIMEM_OFFSET, TRIMEM_SIZE);
	ident_memory = new Int16Array(this.backing_memory.buffer, IDENTMEM_OFFSET, TRIMEM_SIZE);
	memory = new Uint16Array(this.backing_memory.buffer, 0, SHARED_SIZE + TRIMEM_SIZE);
	test_delay = 0;
	reconnect_delay = 0;
	number_tri = 0;
	number_ident = 0;

	u_screen_id: WebGLUniformLocation | null = null;
	u_screen_ident_id: WebGLUniformLocation | null = null;
	bg_u_screen_id: WebGLUniformLocation | null = null;
	u_xfm_id: WebGLUniformLocation | null = null;
	u_xfm_ident_id: WebGLUniformLocation | null = null;
	bg_texture: WebGLTexture;
	vertex_buffer: WebGLBuffer;
	instance_buffer: WebGLBuffer;
	instance_ident_buffer: WebGLBuffer;
	//@ts-ignore
	index_buffer: WebGLBuffer;
	prog: WebGLProgram;
	prog_bg: WebGLProgram;
	prog_ident: WebGLProgram;
	//@ts-ignore
	rb_color: WebGLRenderbuffer;
	//@ts-ignore
	rb_depth: WebGLRenderbuffer;
	onopen() {
		this.ws!.send("vm");
	}
	onclose() {
		delete this.ws;
		this.reconnect_delay = 5;
	}
	onerror() {
		this.reconnect_delay = 5;
	}
	onmessage(e: MessageEvent<ArrayBuffer>) {
		let msg = new Uint8Array(e.data);
		let offset = 0;
		let copy = 0;
		let i = 0;
		let meme = this.memory;
		let ident_offset = 0;
		let ident_meme = this.ident_memory;
		let meme_limit = meme.length;
		uh: while((i+1) < msg.length) {
			let m = msg[i]; i++;
			switch(m) {
			case 1: // copy n words
				copy = msg[i];
				i += 1;
				break;
			case 2: // offset n words, copy n words
				offset += msg[i];
				copy = msg[i+1];
				i += 2;
				break;
			case 3: // offset nn words
				offset += msg[i] | msg[i+1];
				i += 2;
				break;
			case 4: // beginning of ship updates
				offset = SHARED_SIZE;
				this.number_tri = 0;
				this.number_ident = 0;
				break;
			case 5: // simple ship update
				meme[offset  ] = msg[i  ] | (msg[i+1] << 8);
				meme[offset+1] = msg[i+2] | (msg[i+3] << 8);
				meme[offset+2] = msg[i+4] | (msg[i+5] << 8);
				meme[offset+3] = msg[i+6] | (msg[i+7] << 8);
				this.number_tri++;
				if(this.number_tri == -1) {
					ident_meme[ident_offset  ] = meme[offset  ]
					ident_meme[ident_offset+1] = meme[offset+1]
					ident_meme[ident_offset+2] = 0;
					ident_meme[ident_offset+3] = meme[offset+3]
					this.number_ident++;
					ident_offset += 4;
				}
				i += 8;
				offset += 4;
				break;
			case 6: // ident ship update
				ident_meme[ident_offset  ] = meme[offset  ] = msg[i  ] | (msg[i+1] << 8);
				ident_meme[ident_offset+1] = meme[offset+1] = msg[i+2] | (msg[i+3] << 8);
				meme[offset+2] = msg[i+4] | (msg[i+5] << 8);
				meme[offset+3] = msg[i+6] | (msg[i+7] << 8);
				ident_meme[ident_offset+2] = 0;
				ident_meme[ident_offset+3] = meme[offset+3];
				this.number_tri++;
				this.number_ident++;
				i += 8;
				offset += 4;
				ident_offset += 4;
				i += msg[i]; // skip the user name string
				break;
			}
			while((i+1) < msg.length && copy > 0) {
				let v = msg[i] | (msg[i+1] << 8);
				meme[offset] = v;
				i+=2; offset++; copy--;
				if(offset >= meme_limit) break uh;
			}
		}
		const ctx = this.ctx;
		ctx.texSubImage2D(ctx.TEXTURE_2D, 0, 0, 0, 256, 32, ctx.RGB, ctx.UNSIGNED_SHORT_5_6_5, meme);
		ctx.bindBuffer(ctx.ARRAY_BUFFER, this.instance_buffer);
		ctx.bufferSubData(ctx.ARRAY_BUFFER, 0, this.backing_memory, TRIMEM_OFFSET, 8*this.number_tri);
		ctx.bindBuffer(ctx.ARRAY_BUFFER, this.instance_ident_buffer);
		ctx.bufferSubData(ctx.ARRAY_BUFFER, 0, this.backing_memory, IDENTMEM_OFFSET, 8*this.number_ident);
		this.gl_frame();
	}
	gl_frame() {
		const ctx = this.ctx;
		ctx.clearColor(0, 0, 0, 0);
		ctx.clear(ctx.COLOR_BUFFER_BIT /*| ctx.DEPTH_BUFFER_BIT */);
		ctx.useProgram(this.prog_bg);
		ctx.drawElements(ctx.TRIANGLES, 6, ctx.UNSIGNED_INT, 4*3); // "background"
		ctx.useProgram(this.prog);
		ctx.uniform4f(this.u_xfm_id, 1, 0, 0, 1);
		// foreground triangles
		if(this.number_tri > 0) {
			ctx.bindBuffer(ctx.ARRAY_BUFFER, this.instance_buffer);
			ctx.vertexAttribIPointer(1, 4, ctx.UNSIGNED_SHORT, 4*2, 0);
			ctx.drawElementsInstanced(ctx.TRIANGLES, 3, ctx.UNSIGNED_INT, 0, this.number_tri);
			if(this.number_ident > 0) {
				ctx.useProgram(this.prog_ident);
				ctx.bindBuffer(ctx.ARRAY_BUFFER, this.instance_ident_buffer);
				ctx.vertexAttribIPointer(1, 4, ctx.UNSIGNED_SHORT, 4*2, 0);
				ctx.drawElementsInstanced(ctx.TRIANGLES, 6, ctx.UNSIGNED_INT, 9 * 4, this.number_ident);
			}
		}
	}
	interval() {
		if(this.ws) {
		} else if(this.reconnect_delay <= 0) {
			this.connect();
		} else {
			this.reconnect_delay--;
		}
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
		this.prog_bg = this.link_shader_pls(vert_source_bg, frag_source_bg);
		this.prog_ident = this.link_shader_pls(vert_ident_source, frag_ident_source);
		this.u_screen_id = ctx.getUniformLocation(this.prog, "u_screen");
		this.u_screen_ident_id = ctx.getUniformLocation(this.prog_ident, "u_screen");
		this.bg_u_screen_id = ctx.getUniformLocation(this.prog_bg, "u_screen");
		this.u_xfm_id = ctx.getUniformLocation(this.prog, "u_xfm");
		this.u_xfm_ident_id = ctx.getUniformLocation(this.prog_ident, "u_xfm");
		let w = 2 / this.canvas.width;
		let h = -2 / this.canvas.height;
		ctx.useProgram(this.prog_bg);
		ctx.uniform4f(this.bg_u_screen_id, w, h, 0, 0); // size and position of the objects
		ctx.uniform1i(ctx.getUniformLocation(this.prog_bg, "i_texture"), 0);
		ctx.useProgram(this.prog);
		ctx.uniform4f(this.u_screen_id, w, h, -16 * w, h * -16); // size and position of the objects
		ctx.uniform4f(this.u_xfm_id, 1, 0, 0, 1);
		ctx.useProgram(this.prog_ident);
		ctx.uniform4f(this.u_screen_ident_id, w, h, -16 * w, h * -16); // size and position of the objects
		ctx.uniform4f(this.u_xfm_ident_id, 1, 0, 0, 1);
		ctx.enableVertexAttribArray(0);
		ctx.enableVertexAttribArray(1);
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.vertex_buffer = buf;
			ctx.bindBuffer(ctx.ARRAY_BUFFER, buf);
			const box_top = 80;
			let vertex_data = new Float32Array([
				// the Triangle
				/* 00 */ 0,-12,0,0,
				/* 01 */ 7,12,0,0,
				/* 02 */ -7,12,0,0,
				// the box
				/* 03 */ 0,box_top+0,0,0,
				/* 04 */ 0,box_top+240,0,1,
				/* 05 */ 1920,box_top+0,1,0,
				/* 06 */ 1920,box_top+240,1,1,
				// the ident shape
				/* 07 */ -30,-25,0,0,
				/* 08 */ -30, 25,0,1,
				/* 09 */  30,-25,1,0,
				/* 10 */  30, 25,1,1,
			]);
			ctx.bufferData(ctx.ARRAY_BUFFER, vertex_data, ctx.STATIC_DRAW);
			ctx.vertexAttribPointer(0, 4, ctx.FLOAT, false, 4*4, 0);
		}
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.vertex_buffer = buf;
			ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, buf);
			let vertex_data = new Uint32Array([
				/* tri */ 0,2,1, /* box */ 3,4,5, 5,4,6,
				/* ident */
				7,8,9, 9,8,10,
			]);
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
		{
			let buf = ctx.createBuffer() || (()=>{throw Error('eek! buffer');})();
			this.instance_ident_buffer = buf;
			ctx.bindBuffer(ctx.ARRAY_BUFFER, buf);
			ctx.bufferData(ctx.ARRAY_BUFFER, TRIMEM_SIZE * 2, ctx.DYNAMIC_DRAW);
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

