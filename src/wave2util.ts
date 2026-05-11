(function(){
const CONNECT_TO = 'wss://eciv.net/wave_api'

class WaveUtil {
	div_status0: HTMLDivElement
	div_mem0: HTMLDivElement
	div_run0: HTMLDivElement
	div_regs0: HTMLInputElement[] = []
	wr_data0: HTMLTextAreaElement
	wr_button0: HTMLButtonElement
	wc_button0: HTMLButtonElement
	ws: WebSocket | undefined
	user_id = ''
	reconnect_delay = 0
	constructor() {
		let container0 = document.getElementById('container0') as HTMLDivElement
		let buttons0 = document.getElementById('buttons0') as HTMLDivElement
		this.div_run0 = document.getElementById('run0') as HTMLDivElement
		this.div_mem0 = document.getElementById('mem0') as HTMLDivElement
		this.wr_data0 = document.getElementById('writedata0') as HTMLTextAreaElement
		for(let i = 0; i < 16; i++) {
			let id = i < 8 ? `c0r${i}` : `r0r${i-8}`
			let elem = document.getElementById(id) as HTMLInputElement
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
			this.div_regs0.push(elem)
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
		addButton('b_status0', () => this.refresh_all())
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
		if(!this.ws) return;
		this.div_status0.innerText = 'connected';
		let the_token = localStorage.getItem('wave2_session_token')
		if(the_token == null || the_token == '') {
			this.ws.send('auth')
		} else {
			this.ws.send(`token ${the_token}`)
		}
	}
	onclose() {
		this.user_id = ''
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
				this.div_status0.innerText = `logged in ${this.user_id}`
				this.refresh_all()
			} else if(msg[0] == 'OK>') {
				this.div_status0.innerText = `login with: ${e.data.substring(4)}`
			} else if(msg[0] == 'ACK') {
				let show_reg = false
				if(msg[1].startsWith('R')) {
					msg[1] = msg[1].substring(1)
					this.div_run0.innerText = 'Running'
					show_reg = true
				} else if(msg[1].startsWith('H')) {
					msg[1] = msg[1].substring(1)
					this.div_run0.innerText = 'Halted'
					show_reg = true
				}
				let where_at = 0
				if(show_reg) {
					let div_index = 0
					let big_string = ''
					while(where_at < msg[1].length) {
						if(div_index < 16) {
							this.div_regs0[div_index].value =
								msg[1].substring(where_at, where_at + 4)
								+' '+msg[1].substring(where_at+4, where_at + 8)
								+' '+msg[1].substring(where_at+8, where_at + 12)
								+' '+msg[1].substring(where_at+12, where_at + 16)
							div_index += 1
						} else {
							big_string += msg[1].substring(where_at, where_at + 16)
						}
						where_at += 16
					}
					if(big_string != '') {
						this.div_mem0.innerText = big_string
					}
				} else {
					let big_string = ''
					while(where_at < msg[1].length) {
						if(big_string !== '') {
							big_string += ' '
						}
						big_string += msg[1].substring(where_at, where_at + 4)
						where_at += 4
					}
					this.div_mem0.innerText = big_string
				}
			} else {
				console.log(e.data)
				this.div_status0.innerText = e.data
			}
		} else {
			let v = new DataView(e.data)
			let where_at = 1
			switch(v.getUint8(0)) {
			case 8: {
				if(v.getUint8(2) == 82 /* R */) {
					if(v.getUint32(3+144, true) > 0) {
						this.div_run0.innerText = 'Run/Sleep'
					} else {
						this.div_run0.innerText = 'Running'
					}
				} else if(v.getUint8(2) == 72 /* H */) {
					this.div_run0.innerText = 'Halted'
				}
				let where_at = 3
				let div_index = 0
				while(where_at < v.byteLength) {
					if(div_index < 16) {
						this.div_regs0[div_index].value =
							v.getUint16(where_at, true).toString(16).padStart(4,'0')
							+' '+v.getUint16(where_at+2, true).toString(16).padStart(4,'0')
							+' '+v.getUint16(where_at+4, true).toString(16).padStart(4,'0')
							+' '+v.getUint16(where_at+6, true).toString(16).padStart(4,'0')
						where_at += 8
						div_index += 1
					} else {
						where_at += 2
					}
				}
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

let sys: WaveUtil;
function util_main() {
	sys = new WaveUtil()
	// @ts-ignore
	window.sys = sys
	setInterval(function() { sys.interval() }, 1000)
}
// @ts-ignore
window.util_main = util_main

})() // scope thingy
