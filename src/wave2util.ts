import * as waveasm from '../waveasm_thing/pkg/waveasm';
import * as monaco from 'monaco-editor';

const CONNECT_TO = 'wss://eciv.net/wave_api'

function hex(v:number, l:number=4) {
	return v.toString(16).padStart(l, '0');
}

let editor_state: {
	editor?: undefined,
	model: undefined,
} | {
	editor: monaco.editor.IStandaloneCodeEditor,
	model: monaco.editor.ITextModel,
} = {} as any
class WaveUtil {
	div_status0: HTMLDivElement
	div_mem0: HTMLDivElement
	div_run0: HTMLDivElement
	cv0: HTMLCanvasElement
	cv1: HTMLCanvasElement
	ct0: CanvasRenderingContext2D
	ct1: CanvasRenderingContext2D
	status_run = false
	refresh_mode = false
	token_sent = false
	editor_div: HTMLDivElement
	resize_sent = false
	resize_size = 0
	div_regs0: HTMLInputElement[] = []
	wr_data0: HTMLTextAreaElement
	rd_data0: HTMLTextAreaElement
	wr_button0: HTMLButtonElement
	wc_button0: HTMLButtonElement
	ws: WebSocket | undefined
	user_id = ''
	reconnect_delay = 0
	constructor() {
		this.div_run0 = document.getElementById('run0') as HTMLDivElement
		this.div_mem0 = document.getElementById('mem0') as HTMLDivElement
		this.cv0 = document.getElementById('tel0') as HTMLCanvasElement
		this.ct0 = this.cv0.getContext('2d')!
		this.ct0.imageSmoothingEnabled = false
		this.cv1 = document.getElementById('tel1') as HTMLCanvasElement
		this.ct1 = this.cv1.getContext('2d')!
		this.ct1.imageSmoothingEnabled = false
		this.editor_div = document.getElementById('editor0') as HTMLDivElement
		this.wr_data0 = document.getElementById('writedata0') as HTMLTextAreaElement
		this.rd_data0 = document.getElementById('redata0') as HTMLTextAreaElement
		let regbox = [
			'c0r0', 'c0r1', 'c0r2', 'c0r3', 'c0r4', 'c0r5', 'c0r6', 'c0r7',
			'r0r0', 'r0r1', 'r0r2', 'r0r3', 'r0r4', 'r0r5', 'r0r6', 'r0r7',
			'r0rm0', 'r0rm1', 'r0rs', 'r0rm2', 'r0rm3', 'r0rm4',
		]
		for(let i = 0; i < regbox.length; i++) {
			let elem = document.getElementById(regbox[i]) as HTMLInputElement
			this.div_regs0.push(elem)
			if(i >= 16) { continue }
			const reg_index = i * 4
			//elem.contentEditable = 'plaintext-only'
			const sel_info = {
				end: elem.selectionEnd ?? 0,
				start: elem.selectionStart ?? 0,
				type: elem.selectionDirection,
			}
			elem.addEventListener('selectionchange', (ev) => {
				//console.log(document.getSelection())
				sel_info.end = elem.selectionEnd ?? 0
				sel_info.start = elem.selectionStart ?? 0
				sel_info.type = elem.selectionDirection
			})
			elem.addEventListener('change', () => {
				console.log('changed', elem.value)
				if(this.ws && this.user_id.length > 0) {
					if(reg_index == 0) {
						this.ws.send(`w 0 ${elem.value}`)
					} else {
						this.ws.send(`w 0 ${reg_index.toString(16)}ᚢ${elem.value}`)
					}
					this.ws.send('sta 0')
				}
			})
			window.addEventListener('resize', () => {
				this.resize_sent = true
			})
			//@ts-ignore
			elem.addEventListener('input', (ev:InputEvent) => {
				//console.log(sel)
				let the_crazy_text = elem.value
				let old_start = sel_info.start
				let old_end = sel_info.end
				let new_text
				sel_info.end = elem.selectionEnd ?? 0
				sel_info.start = elem.selectionStart ?? 0
				sel_info.type = elem.selectionDirection
				let start_text = the_crazy_text.substring(0, sel_info.start)
				let end_text = the_crazy_text.substring(sel_info.end)
				ev.preventDefault()
				ev.stopPropagation()
				if(ev.inputType == 'insertText') {
					// if(!sel) {
					// 	console.log('Null select')
					// 	return
					// }
					if(old_start == old_end) {
						//console.warn('one inserted', sel_info.start, sel_info.end)
						if(ev.data == null || !ev.data.match(/[0-9a-fA-F]/)) {
							new_text = start_text.substring(0, sel_info.start - 1) + end_text.substring(0)
							elem.value = new_text
							//sel.setPosition(elem.firstChild, sel_info.start - 1)
							elem.setSelectionRange(sel_info.start - 1, sel_info.start - 1, 'forward')
						} else {
							if(old_start == 4 || old_start == 9 || old_start == 14) {
								new_text = start_text.substring(0, sel_info.start - 1) + ' ' + ev.data + end_text.substring(2)
								sel_info.start += 1
							} else {
								new_text = start_text + end_text.substring(1)
							}
							//console.log(new_text, sel_info.start)
							if(new_text.length > 19) {
								new_text = new_text.substring(0,19)
							}
							elem.value = new_text
							elem.setSelectionRange(sel_info.start, sel_info.start, 'forward')
							//sel.setPosition(elem.firstChild, sel_info.start)
						}
					} else {
						//console.warn('range inserted', old_start, old_end)
						let in_text = ev.data ?? '0'
						if(ev.data == null || !ev.data.match(/[0-9a-fA-F]/)) {
							in_text = '0'
						}
						let template_string = '0000 0000 0000 0000'.substring(old_start, old_end).replaceAll('0', in_text)
						//console.log(start_text, '||', end_text)
						start_text = start_text.substring(0, sel_info.start - 1)
						new_text = start_text + template_string + end_text
						if(new_text.length > 19) {
							new_text = new_text.substring(0,19)
						}
						elem.value = new_text
						elem.setSelectionRange(sel_info.start, sel_info.start, 'forward')
					}
				} else if(ev.inputType == 'deleteContentBackward') {
					let del_text = '0'
					if(old_start != old_end) {
						let template_string = '0000 0000 0000 0000'.substring(old_start, old_end).replaceAll('0', del_text)
						//console.log(start_text, '||', end_text)
						start_text = start_text.substring(0, sel_info.start)
						new_text = start_text + template_string + end_text
					} else {
						if(sel_info.start == 4 || sel_info.start == 9 || sel_info.start == 14) {
							del_text = ' '
						}
						new_text = start_text + del_text + end_text
					}
					//console.log(ev)
					//console.log(sel_info.start, start_text, end_text)
					elem.value = new_text
					elem.setSelectionRange(sel_info.start, sel_info.start, 'forward')
				} else {
					console.warn('todo', ev.inputType)
					//console.log(sel)
				}
				sel_info.end = elem.selectionEnd ?? 0
				sel_info.start = elem.selectionStart ?? 0
				sel_info.type = elem.selectionDirection
				//console.log('new sel', sel_info.start, sel_info.end)
			})
		}
		let addButton = (l:string, f: (ev: MouseEvent) => void) => {
			let b = document.getElementById(l) as HTMLButtonElement
			if(!b) return;
			b.addEventListener('click', f)
		}
		this.div_status0 = document.getElementById('status0') as HTMLDivElement
		this.wr_button0 = document.getElementById('write0') as HTMLButtonElement
		this.wc_button0 = document.getElementById('code0') as HTMLButtonElement
		this.wr_button0.addEventListener('click', () => {
			if(this.ws) {
				this.ws.send(`w 0 ${this.wr_data0.value}`)
				this.wr_data0.select()
				this.refresh_all()
			}
		})
		this.wc_button0.addEventListener('click', () => {
			if(this.ws) {
				this.ws.send(`w 0 40ᚢ${this.wr_data0.value}`)
				this.wr_data0.select()
				this.refresh_all()
			}
		});
		(document.getElementById('asm0') as HTMLButtonElement).addEventListener('click', () => {
			let source = this.wr_data0.value
			if(editor_state.model) {
				source = editor_state.model.getValue()
				if(source == '' && this.wr_data0.value != '') {
					source = this.wr_data0.value
					editor_state.model.setValue(source)
				}
				localStorage.setItem('wave2_program0', source)
				this.wr_data0.value = source
			}
			using thing = waveasm.do_the_thing(source, 'wavecode')

			if(editor_state.model) {
				let out = thing.output
				let out_items = out.split('\n')
				let decor_items: monaco.editor.IMarkerData[] = []
				for(let item of out_items) {
					if(item == '') continue
					console.log(item)
					if(item.startsWith('[')) {
						try {
							let better_item = JSON.parse(item)
							decor_items.push({
								message: better_item[5],
								severity: monaco.MarkerSeverity.Error,
								code: 'OOF',
								startLineNumber: better_item[0],
								endLineNumber: better_item[1],
								startColumn: 1 + better_item[2],
								endColumn: 1 + better_item[3],
							})
						} catch(ohnoitded) {}
					}
				}
				decor_items.push({
					message: 'oof',
					severity: monaco.MarkerSeverity.Hint,
					code: 'OOF',
					startLineNumber: 1,
					endLineNumber: 1,
					startColumn: 2,
					endColumn: 3,
				})
				if(decor_items.length > 0) {
					monaco.editor.setModelMarkers(editor_state.model, "waveasm", decor_items)
				}
			}
			this.rd_data0.value = JSON.stringify([thing.status, thing.output, thing.memory, thing.code])
			//thing.free()
		});
		(document.getElementById('asml0') as HTMLButtonElement).addEventListener('click', () => {
			let source = this.wr_data0.value
			if(editor_state.model) {
				source = editor_state.model.getValue()
				if(source == '' && this.wr_data0.value != '') {
					source = this.wr_data0.value
					editor_state.model.setValue(source)
				}
				localStorage.setItem('wave2_program0', source)
				this.wr_data0.value = source
			}
			using thing = waveasm.do_the_thing(source, 'wavecode')
			let status = thing.status, memory = thing.memory, code = thing.code
			if(status && memory != undefined && code != undefined && this.ws && this.user_id) {
				if(memory != '') {
					this.ws.send(`w 0 ${memory.replace(' ','').replace('\n','')}`)
				}
				if(code != '') {
					this.ws.send(`w 0 40ᚢ${code.replace(' ','').replace('\n','')}`)
				}
				this.refresh_all()
			}
			//thing.free()
		})
		addButton('b_run0', () => {
			if(this.ws) {
				this.ws.send('run 0')
				setTimeout(() => this.refresh_all(), 200)
			}
		})
		addButton('b_halt0', () => {
			if(this.ws) {
				this.ws.send('halt 0')
				this.refresh_all()
			}
		})
		addButton('b_ident', () => {
			if(this.ws && this.user_id) {
				this.ws.send('ident')
			}
		})
		addButton('b_status0', () => {
			this.refresh_all()
		})
		addButton('b_stauto0', () => {
			this.refresh_mode = !this.refresh_mode
			if(!this.ws || this.user_id == '') { return }
			if(this.refresh_mode) {
				this.ws.send('sts 0')
			} else {
				this.ws.send('nosts')
			}
		})
		addButton('b_res0', () => {
			if(this.ws) {
				this.ws.send('restart 0')
				this.refresh_all()
			}
		})
		addButton('b_clear0', () => {
			if(this.ws) {
				this.ws.send('clear 0')
				this.refresh_all()
			}
		})
	}
	fast_tick() {
		let econ = this.editor_div
		let container_height = (econ.parentElement?.clientHeight ?? 0)
		if(container_height != this.resize_size) {
			this.resize_size = container_height
			this.resize_sent = true
		}
		if(this.resize_sent && editor_state.editor) {
			this.resize_sent = false
			let width = Math.max(570, econ.clientWidth ?? 0)
			let sibling_height = (econ.nextElementSibling?.clientHeight ?? 0)
			let evil_height = container_height - sibling_height
			let height = 400
			if(evil_height < 400) {
				height = Math.max(400, sibling_height)
			}
			editor_state.editor.layout({width, height}, true)
		}
	}
	refresh_all() {
		if(!this.ws || this.user_id == '') { return }
		this.ws.send('sta 0')
		this.ws.send('r 0 40 C0')
	}
	connect() {
		let ws = this.ws = new WebSocket(CONNECT_TO)
		ws.binaryType = 'arraybuffer'
		ws.addEventListener("open", ()=>this.onopen())
		ws.addEventListener("close", ()=>this.onclose())
		ws.addEventListener("error", ()=>this.onerror())
		ws.addEventListener("message", (e)=>this.onmessage(e))
	}
	onopen() {
		//this.ws!.send("vm");
		console.log('connected')
		this.status_run = false
		if(!this.ws) return;
		this.div_status0.innerText = 'connected';
		let the_token = localStorage.getItem('wave2_session_token')
		if(the_token == null || the_token == '') {
			this.ws.send('auth')
		} else {
			this.ws.send(`token ${the_token}`)
			this.token_sent = true
		}
	}
	onclose() {
		this.user_id = ''
		this.status_run = false
		this.token_sent = false
		delete this.ws
		this.reconnect_delay = 10
	}
	onerror() {
		this.reconnect_delay = 5
	}
	onmessage(e: MessageEvent<ArrayBuffer|string>) {
		if(typeof e.data == 'string') {
			let msg = e.data.split(' ')
			if(msg[0] === 'authed') {
				this.user_id = msg[1]
				let the_token = msg[2]
				localStorage.setItem('wave2_session_token', the_token)
				this.token_sent = false
				this.div_status0.innerText = `logged in ${this.user_id}`
				this.refresh_all()
				return
			}
			if(msg[0] == 'OK>') {
				this.div_status0.innerText = `login with: ${e.data.substring(4)}`
				localStorage.removeItem('wave2_session_token')
				this.token_sent = false
			} else if(msg[0] == 'ACK') {
			} else {
				if(this.token_sent) {
					console.warn('token cleared')
					// expired!
					localStorage.removeItem('wave2_session_token')
				}
				console.warn('unknown control message', e.data)
				this.div_status0.innerText = e.data
			}
		} else {
			let v = new DataView(e.data)
			let where_at = 1
			let net_op = v.getUint8(0)
			switch(net_op) {
			case 8: case 10: {
				let sleep_text = 'Unknown'
				if(v.getUint8(2) == 82 /* R */) {
					sleep_text = 'Running'
					this.status_run = true
				} else if(v.getUint8(2) == 72 /* H */) {
					sleep_text = 'Halted'
					this.status_run = false
				}
				let where_at = 3
				let div_index = 0
				if(net_op == 10) {
					// thread state update without const registers
					// so skip them
					div_index = 8
					if(!this.refresh_mode) {
						this.ws!.send('nosts')
					}
				}
				while(where_at < v.byteLength) {
					if(div_index < 18) {
						this.div_regs0[div_index].value =
							`${hex(v.getUint16(where_at, true))
							} ${hex(v.getUint16(where_at+2, true))
							} ${hex(v.getUint16(where_at+4, true))
							} ${hex(v.getUint16(where_at+6, true))}`
						where_at += 8
						div_index += 1
					} else if(div_index == 18) {
						let sleep_time = v.getUint32(where_at, true)
						if(this.status_run && sleep_time > 0) {
							sleep_text = 'Run/Sleep'
						}
						this.div_regs0[div_index].value = hex(sleep_time, 8)
						where_at += 4
						div_index += 1
					} else if(div_index == 19) {
						this.div_regs0[div_index].value =
							hex(v.getUint16(where_at, true))
						where_at += 2
						div_index += 1
					} else if(div_index == 20) {
						let modsel = hex(v.getUint16(where_at, true))
						where_at += 2
						for(let n = 0; n < 6; n++) {
							modsel += ' ' + hex(v.getUint16(where_at, true))
							where_at += 2
						}
						this.div_regs0[div_index].value = modsel
						div_index += 1
					} else if(div_index == 21) {
						let vel_x = v.getInt16(where_at, true) * 0.00390625
						let vel_y = v.getInt16(where_at+2, true) * 0.00390625
						let compass = v.getUint16(where_at+4, true)
						let color_val = v.getUint16(where_at+6, true)
						let info = vel_x.toFixed(2).padStart(7,' ')
						info += ' ' + vel_y.toFixed(2).padStart(7,' ')
						info += ' ' + hex(compass)
						info += ' ' + hex(color_val)
						where_at += 8
						let cl_r = (color_val >> 11) & 0x1f,
							cl_g = (color_val >> 5) & 0x3f,
							cl_b = color_val & 0x1f
						cl_r = (cl_r << 3) | (cl_r >> 2)
						cl_g = (cl_g << 2) | (cl_g >> 4)
						cl_b = (cl_b << 3) | (cl_b >> 2)
						this.div_regs0[div_index].value = info
						let compass_angle = compass * (2.0 / 32768.0) * Math.PI;
						let head_x = Math.sin(compass_angle)
						let head_y = -Math.cos(compass_angle)
						let head_xx = -head_y
						let head_xy = head_x
						let head_yx = head_x
						let head_yy = head_y
						let ct = this.ct0
						ct.clearRect(0,0,200,200)
						ct.fillStyle = `#${hex(cl_r,2)}${hex(cl_g,2)}${hex(cl_b,2)}`
						ct.fillRect(0, 0, 20, 10)
						ct.strokeStyle = '#777'
						ct.beginPath()
						ct.moveTo(50, 50 + -30)
						ct.lineTo(50 + 15, 50 + 30)
						ct.lineTo(50 + -15, 50 + 30)
						ct.lineTo(50, 50 + -30)
						ct.stroke()
						ct.beginPath()
						ct.moveTo(50 + head_xy * -45, 50 + -head_yy * -45)
						ct.lineTo(50 + head_xx * 5 + head_xy * -40, 50 + -head_yx * 5 + -head_yy * -40)
						ct.lineTo(50 + head_xx * -5 + head_xy * -40, 50 + -head_yx * -5 + -head_yy * -40)
						ct.lineTo(50 + head_xy * -45, 50 + -head_yy * -45)
						ct.stroke()
						ct.strokeStyle = '#f22'
						ct.beginPath()
						ct.moveTo(50, 50)
						ct.lineTo(50 + vel_x, 50)
						ct.stroke()
						ct.strokeStyle = '#6f6'
						ct.beginPath()
						ct.moveTo(50, 50)
						ct.lineTo(50, 50 - vel_y)
						ct.stroke()
						ct.strokeStyle = '#fc6'
						ct.beginPath()
						ct.moveTo(50, 50)
						ct.lineTo(50 + vel_x, 50 - vel_y)
						ct.stroke()
						//
						ct = this.ct1
						ct.clearRect(0,0,200,200)
						ct.strokeStyle = '#fff'
						ct.beginPath()
						ct.moveTo(50, 50)
						ct.lineTo(50 + head_x * 30, 50 + head_y * 30)
						ct.stroke()
						ct.strokeStyle = '#777'
						ct.beginPath()
						ct.moveTo(50 + head_x * 30, 50 + head_y * 30)
						ct.lineTo(50 + head_xx * 15 + head_xy * -30, 50 + head_yx * 15 + head_yy * -30)
						ct.lineTo(50 + head_xx * -15 + head_xy * -30, 50 + head_yx * -15 + head_yy * -30)
						ct.lineTo(50 + head_x * 30, 50 + head_y * 30)
						ct.stroke()
						ct.strokeStyle = '#fc6'
						ct.beginPath()
						ct.moveTo(50, 50)
						ct.lineTo(50 + head_xx * vel_x + head_xy * vel_y, 50 + head_yx * vel_x + head_yy * vel_y)
						ct.stroke()
						div_index += 1
					} else {
						where_at += 2
					}
				}
				this.div_run0.innerText = sleep_text
			} break;
			case 9: {
				let big_string_of_words = ''
				where_at = 2
				if(where_at + 1 < v.byteLength) {
					big_string_of_words = v.getUint16(where_at, true).toString(16).padStart(4,'0')
				}
				where_at += 2
				while(where_at + 1 < v.byteLength) {
					big_string_of_words += ' ' + v.getUint16(where_at, true).toString(16).padStart(4,'0')
					where_at += 2
				}
				this.div_mem0.innerText = big_string_of_words
			} break;
			}
		}
	}
	interval() {
		if(this.ws) {
		} else if(this.reconnect_delay <= 0) {
			this.connect()
		} else {
			this.reconnect_delay--
		}
	}
}

import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
self.MonacoEnvironment = {
	getWorker: async function(workerId, label) {
		// const getWorkerModule =
		// 	(modulePath:string) => new Worker(self.MonacoEnvironment!.getWorkerUrl(
		// 		`/monaco-editor/esm/vs/${modulePath}`, label),
		// 		{ name: label, type: 'module' })
		const getWorkerModule =
			async (modulePath:string) => await
				import(`monaco-editor/esm/vs/${modulePath}`)
				
		switch(label) {
			case 'json':
				return getWorkerModule('language/json/json.worker?worker')
			case 'css':
			case 'scss':
			case 'less':
				return getWorkerModule('language/css/css.worker?worker')
			case 'html':
				return getWorkerModule('language/html/html.worker?worker')
			case 'typescript':
			case 'javascript':
				return getWorkerModule('language/typescript/ts.worker?worker')
			default:
				return new editorWorker()
		}
	}
}

let sys: WaveUtil;
async function editor_init() {
	monaco.languages.register({
		id: 'wave2asm',
	})
	interface InstructionInfo {
		i: string[],
		s?: string[],
		doc?: string,
	}
	let instructions: InstructionInfo[] = [
		{ i:["move", "mov"], doc: 'Move values between registers' },
		{ i:["swizzle", "swi"] },
		{ i:["wmove", "wmov"] },
		{ i:["wswap"] },
		{ i:["wadd"] },
		{ i:["wsub"] },
		// math ops
		{ i:["add"], s:['w','b'], },
		{ i:["adds"], s:['w','b'], },
		{ i:["sub"], s:['w','b'], },
		{ i:["subs"], s:['w','b'], },
		{ i:["eq"], s:['w','b'], },
		{ i:["equ"], s:['w','b'], },
		{ i:["ne"], s:['w','b'], },
		{ i:["neq"], s:['w','b'], },
		{ i:["carry"], s:['w','b'], },
		{ i:["lt"], s:['w','b'], },
		{ i:["gt"], s:['w','b'], },
		{ i:["le"], s:['w','b'], },
		{ i:["lte"], s:['w','b'], },
		{ i:["ge"], s:['w','b'], },
		{ i:["gte"], s:['w','b'], },
		{ i:["addover", "ado", "addo"], s:['w','b'], },
		{ i:["subover", "suo", "subo"], s:['w','b'], },
		{ i:["rsubover", "rso", "rsuo", "rsubo"], s:['w','b'], },
		// shift ops
		{ i:["lsl"], s:['w','b'], },
		{ i:["asl"], s:['w','b'], },
		{ i:["rol"], s:['w','b'], },
		{ i:["asr"], s:['w','b'], },
		{ i:["lsr"], s:['w','b'], },
		{ i:["ror"], s:['w','b'], },
		// bitwise ops
		{ i:["all"] },
		{ i:["zero"] },
		{ i:["one"] },
		{ i:["swap", "swp"] },
		{ i:["notsrc", "nsrc"] },
		{ i:["notdest", "ndst", "notdst"] },
		{ i:["srcandnotdst", "sand"] },
		{ i:["notsrcanddst", "nsad"] },
		{ i:["srcornotdst" , "sond"] },
		{ i:["notsrcordst" , "nsod"] },
		{ i:["and"] },
		{ i:["or"] },
		{ i:["xor"] },
		{ i:["nand"] },
		{ i:["nor"] },
		{ i:["xnor"] },
		// special ops
		{ i:["hadd"] },
		{ i:["multisat", "mul", "mults"] },
		{ i:["multlow" , "mlo", "multl"] },
		{ i:["multhigh", "mhi", "multh"] },
		{ i:["divide", "div"] },
		{ i:["rdivide", "rdiv"] },
		// system ops
		{ i:["nop"] },
		{ i:["halt", "hlt"] },
		{ i:["sleep", "slp"], s:['', 'l','h','w'], },
		// shorthand ops
		{ i:["skip", "skip1"] },
		{ i:["skip2"] },
		{ i:["skip3"] },
		{ i:["skip4"] },
		{ i:["set", "set1"] },
		{ i:["set2"] },
		{ i:["set3"] },
		{ i:["set4"] },
		{ i:["jump", "jmp"] },
		{ i:["je"] },
		{ i:["jeq"] },
		{ i:["jc"] },
		{ i:["jcp"] },
		{ i:["jne"] },
		{ i:["jnc"] },
		{ i:["jcc"] },
	];
	let registers = [
		{r:'c0', doc: 'constant'},
		{r:'c1', doc: 'constant'},
		{r:'c2', doc: 'constant'},
		{r:'c3', doc: 'constant'},
		{r:'c4', doc: 'constant'},
		{r:'c5', doc: 'constant'},
		{r:'c6', doc: 'constant'},
		{r:'c7', doc: 'constant'},
		{r:'r0', doc: 'general'},
		{r:'r1', doc: 'general'},
		{r:'r2', doc: 'general'},
		{r:'r3', doc: 'general'},
		{r:'r4', doc: 'general'},
		{r:'r5', doc: 'general'},
		{r:'r6', doc: 'general'},
		{r:'r7', doc: 'instruction'},
		{r:'ri', doc: 'instruction'},
	]
	monaco.languages.setMonarchTokensProvider('wave2asm', {
		// Set defaultToken to invalid to see what you do not tokenize yet
		defaultToken: 'invalid',

		registers: registers.map((r) => r.r),
		keywords: (()=>{
			let out = []
			for(let inst of instructions) {
				for(let alias of inst.i) {
					out.push(alias)
				}
			}
			return out
		})(),
		// The main tokenizer for our languages
		tokenizer: {
			root: [
				{ include: 'whitespace' },
				[/:\w+/, 'number'],
				[/^\.memory\s*/, { token: 'keyword', switchTo:'memory' }],
				[/^\.\w+/, 'type'],
				[/(\w+)(\.[bwhl])/, {
					cases: {
						'$1@keywords': [{token: 'type', switchTo:'params'}, 'keyword'],
						'@default': 'invalid',
					},
				}],
				[/\w+/, {
					cases: {
						'@keywords': {token: 'type', switchTo:'params'},
						'@default': 'invalid',
					},
				}],
				[/!(\w+)/, {
					cases: {
						'$1~[0-9a-fA-F]{4}': 'number',
						'@default': 'invalid',
					}
				}],
			],
			params: [
				{ include: 'whitespace' },
				[/^./, { token:'@rematch', switchTo: 'root' }],
				[/:\w+/, 'number'], // labels
				[/\w+/, {
					cases: {
						'@registers': 'register',
						'@default': 'ident',
					}
				}],
				[/[+]/, 'operator'],
				[/[\[\],.+]/, 'delimiter'],
				// numbers
				[/\$[0-9a-fA-F]+/, 'number.hex'],
				[/\d+/, 'number'],
				[/./, 'invalid'],
			],
			memory: [
				[/^\.code/, { token: 'keyword', switchTo: 'root' }],
				[/\w+/, {
					cases: {
						'[0-9a-fA-F]{4}': 'number',
						'@default': 'invalid',
					},
				}],
				{ include: 'whitespace' },
			],
			whitespace: [
				[/[ \t\r\n]+/, 'white'],
				[/;.*$/, 'comment'],
				[/#.*$/, 'comment'],
			],
		},
	})
	monaco.languages.registerCompletionItemProvider('wave2asm', {
		provideCompletionItems: (model, position, context, token) => {
			let line = model.getLineContent(position.lineNumber)
			let maybe_word = model.getWordAtPosition(position)
			let suggestions: monaco.languages.CompletionItem[] = []
			let line_before = line.substring(0, position.column - 1)
			let line_after = line.substring(position.column - 1)
			let word_before = ''
			if(maybe_word) {
				word_before = line.substring(maybe_word.startColumn - 1, position.column - 1)
				sys.rd_data0.value = `word:${position.column}:${line_before}<${word_before}:${maybe_word.word}:${maybe_word.startColumn}-${maybe_word.endColumn}>${line_after}`
			} else {
				sys.rd_data0.value = `line:${position.column}:${line_before}<>${line_after}:`
			}
			let at_cursor = {
				startLineNumber: position.lineNumber,
				startColumn: position.column,
				endLineNumber: position.lineNumber,
				endColumn: position.column,
			} satisfies monaco.IRange
			let some_text = (word: string | InstructionInfo, kind: monaco.languages.CompletionItemKind = monaco.languages.CompletionItemKind.Keyword, alias_or_cb?: number | ((item:monaco.languages.CompletionItem) => void)) => {
				let inst: undefined | InstructionInfo
				if(typeof word != 'string') {
					if(typeof alias_or_cb !== 'number')
						throw new Error('called some_text wrong')
					inst = word
					word = inst.i[alias_or_cb]
				}
				if(maybe_word) {
					if(!word.includes(maybe_word.word)) {
						return;
					}
				}
				let item = {
					label: word,
					insertText: word,
					kind,
					range: at_cursor,
				} satisfies monaco.languages.CompletionItem;
				let full_item = item as monaco.languages.CompletionItem;
				if(inst) {
					if(inst.s == undefined) {
					} else {
						full_item.commitCharacters = ['.']
					}
					if(inst.doc) {
						full_item.detail = inst.doc
					}
				} else if(typeof alias_or_cb == 'function') {
					alias_or_cb(full_item)
				}
				if(maybe_word) {
					if(maybe_word.startColumn < position.column) {
						item.range.startColumn = maybe_word.startColumn
						if(!word.startsWith(maybe_word.word)) {
							let where_at = word.indexOf(maybe_word.word)
							if(where_at > -1) {
								full_item.filterText = word.substring(where_at) + word.substring(0, where_at)
								full_item.sortText = word_before + 'z' + word.substring(0, where_at)
							} else {
								full_item.sortText = word_before + 'z' + word
							}
							full_item.range = {
								insert: {
									startLineNumber: item.range.startLineNumber,
									endLineNumber: item.range.endLineNumber,
									startColumn: maybe_word.startColumn,
									endColumn: maybe_word.startColumn,
								},
								replace: item.range
							}
						}
					}
				}
				suggestions.push(item)
			}
			let give_dot = false
			let give_inst = false
			let give_labels = false
			let give_params = false
			if(context.triggerKind == monaco.languages.CompletionTriggerKind.TriggerCharacter) {
				if(context.triggerCharacter == '.') {
					give_dot = true
				} else if(context.triggerCharacter == ':') {
					give_labels = true
				} else {
					give_inst = true
				}
			} else {
				give_inst = true
				if(line.startsWith('.')) {
					give_dot = true
					give_inst = false
				} else if(line == '') {
					give_dot = true
				}
			}
			if(/^\s*(:\w+\s*)?(\w+(\.\w)?\s)/.test(line)) {
				give_inst = false
				give_params = true
			}
			if(give_dot) {
				if(word_before == '') {
					let m = /(\w+)\.$/.exec(line_before)
					if(m) {
						word_before = m[1] + '.'
						let found_thing =
							instructions.find((inst) => inst.i.includes(m[1]))
						if(found_thing && found_thing.s) {
							for(let suffix_i = 0; suffix_i < found_thing.s.length; suffix_i++) {
								let suffix = found_thing.s[suffix_i]
								if(suffix == '') continue;
								suggestions.push({
									label: suffix,
									insertText: suffix,
									sortText: suffix_i.toString(),
									commitCharacters: ['r', found_thing.i[0].startsWith('mov') ? '[':'c'],
									preselect: suffix == 'w',
									kind: monaco.languages.CompletionItemKind.Keyword,
									range: at_cursor,
								} satisfies monaco.languages.CompletionItem);
							}
						}
					}
				}
				if('.code'.includes(word_before) && position.column < 6) {
					some_text('.code')
				}
				if('.memory'.includes(word_before) && position.column < 8) {
					some_text('.memory')
				}
			}
			if(give_inst) {
				for(let inst of instructions) {
					for(let alias_index = 0; alias_index < inst.i.length; alias_index++) {
						some_text(inst, monaco.languages.CompletionItemKind.Function, alias_index)
					}
				}
			}
			if(give_params) {
				for(let reg of registers) {
					some_text(reg.r, monaco.languages.CompletionItemKind.Variable,
						(item) => {
							item.commitCharacters = [',','.',']','\r','\n',';']
							item.detail = `${reg.doc} register`
						})
				}
			}
			return { suggestions }
		},
		triggerCharacters: [
			':',',','[','.',
			'a', 'c', 'd', 'e',
			'g', 'h', 'j', 'l',
			'm', 'n', 'o', 'r',
			's', 'w', 'x', 'z',
		],
		// this is only if we want to fill in extra details that might be more expensive
		//resolveCompletionItem: (item, token) => { return item },
	})
	let init_source = localStorage.getItem('wave2_program0') ?? ''
	editor_state.editor = monaco.editor.create(document.getElementById('editor0') as HTMLElement, {
		value: init_source,
		roundedSelection: false,
		theme: 'vs-dark',
		fontSize: 16,
		scrollBeyondLastLine: false,
		placeholder: '; wave2 assembly code here\n:label jmp :label\n',
		language: 'wave2asm',
	})
	editor_state.model = editor_state.editor.getModel()!
}

async function util_main() {
	try {
		waveasm.oof_two_point_ohno()
	} catch(e) { console.error(e) }
	sys = new WaveUtil()
	// @ts-ignore
	window.sys = sys
	await editor_init()
	setInterval(function() { sys.interval() }, 1000)
	setInterval(function() { sys.fast_tick() }, 100)
}

if(document.readyState === 'loading') {
	document.addEventListener('DOMContentLoaded', async() => {
		util_main()
	})
} else {
	util_main();
}

