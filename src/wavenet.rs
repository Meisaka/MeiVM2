
use serde::Serialize;
use std::net::SocketAddr;
use std::collections::HashMap;
use futures_util::{ StreamExt, SinkExt };
use std::sync::Arc;
use tokio::net::{
    TcpListener,
    TcpStream
};
use rand::random;
use serde_json::Value as JSValue;
use tokio::{
    fs,
    runtime,
    sync::{
        mpsc,
        oneshot,
        watch,
    },
    time::{ self, Duration, timeout }
};
use tokio_tungstenite::{
    WebSocketStream,
    tungstenite::protocol::Message as WSMessage,
};
use meivm2::{SimulationVM, vm_write, write_persist, read_persist};

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
enum ClientCommand {
    ChatMessage(String),
}

#[derive(Debug)]
pub enum SimError {
    SendError,
    Timeout,
    CloseMessage,
    InvalidFrame,
    CloseError,
    IOError(std::io::Error),
    WSError(tokio_tungstenite::tungstenite::Error),
}
impl From<std::io::Error> for SimError {
    fn from(value: std::io::Error) -> Self {
        SimError::IOError(value)
    }
}
impl From<tokio_tungstenite::tungstenite::Error> for SimError {
    fn from(value: tokio_tungstenite::tungstenite::Error) -> Self {
        SimError::WSError(value)
    }
}
impl std::fmt::Display for SimError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

async fn sim_parse_chat(
    settings: &'static ServerState, reply: &mpsc::Sender<WSData>,
    sim_vm: &mut SimulationVM,
    user_id: u64, user_login: String, user_name: String, user_color_str: String,
    message: String) -> Option<String> {
    //eprintln!("sim got chat: {}:{}", user, message);
    let user_color = if let (Some('#'), Some(r), Some(g), Some(b)) = (
        user_color_str.chars().next(),
        user_color_str.get(1..3),
        user_color_str.get(3..5),
        user_color_str.get(5..7)) {
        (u32::from_str_radix(r, 16).unwrap_or(0) << 16)
        | (u32::from_str_radix(g, 16).unwrap_or(0) << 8)
        | u32::from_str_radix(b, 16).unwrap_or(0)
    } else { 0xffffff };
    let mut user_params = Some((
        user_login, user_name, user_color));
    let mut split = message.split_whitespace();
    let command = split.next()?;
    if !command.starts_with("!vm") { None? }
    while let Some(command) = split.next() {
        if let Some((user_login, user_name, user_color))
            = user_params.take() {
            sim_vm.user_new_with_update(user_id, user_name, user_login, user_color);
        }
        match command {
            "run"| "go" | "start" | "begin"
                | "commence" | "launch" | "execute" => {
                sim_vm.user_run(user_id);
            }
            "write" => {
                let vmproc = &mut sim_vm.make_user(user_id).proc;
                vm_write(&mut split, vmproc.as_mut(), 0);
            }
            "code" => {
                let vmproc = &mut sim_vm.make_user(user_id).proc;
                vm_write(&mut split, vmproc.as_mut(), 0x40);
            }
            "stop" | "halt" | "crash" | "lunch" => {
                sim_vm.user_halt(user_id);
            }
            "ident" => {
                let vmuser = sim_vm.user_new(user_id);
                vmuser.context.ident_time = 10000;
            }
            "reset" | "clear" => {
                sim_vm.user_reset(user_id);
            }
            "restart" => {
                sim_vm.user_restart(user_id);
                sim_vm.user_run(user_id);
            }
            "color" | "colour" => {
                let vmuser = sim_vm.make_user(user_id);
                let param = split.next()?;
                if let Some(color) = settings.parse_color_string(param) {
                    vmuser.context.user_color_loaded = true;
                    vmuser.context.ship.set_color(color);
                }
            }
            "dump" => { sim_vm.user_dump(user_id); }
            "summon" => {
                let vmuser = sim_vm.user_new(user_id);
                vmuser.context.user_color_loaded = true;
                vmuser.context.ship.set_color(user_color);
            }
            "auth" | "login" | "logon" => {
                let id = (split.next()?, split.next()?, split.next()?, split.next()?);
                return Some(format!("{} {} {} {}", id.0, id.1, id.2, id.3));
            }
            "help" | "commands" => {
                let s = serde_json::to_string(&settings.help_message).ok()?;
                reply.send(WSData::Text(s)).await.ok()?;
            }
            _ => {}
        }
    }
    None
}

const RANDOM_WORDS: &[&str] = &[
    /* 0x00    0x01       0x02      0x03*/
    "eek",    "meh",     "ohmega", "nyan",
    /* 0x04    0x05       0x06      0x07*/
    "fops",   "ehlo",    "fimsh",  "yeet",
    /* 0x08    0x09       0x0a      0x0b*/
    "cooking", "cookie",  "lisp",   "brainrot",
    /* 0x0c    0x0d       0x0e      0x0f*/
    "sigma",  "spam",    "eepy",   "mood",
    /* 0x10    0x11       0x12      0x13*/
    "memory", "cursed",  "thing",  "word",
    /* 0x14    0x15       0x16      0x17*/
    "vampire", "bird",  "clearly", "foxgoddess",
    /* 0x18    0x19       0x1a      0x1b*/
    "evil",   "factory", "chaos",  "cute",
    /* 0x1c    0x1d       0x1e      0x1f */
    "stare",  "song",    "maybe",  "instruction",
    /* 0x20    0x21       0x22      0x23 */
    "e",      "reverse", "unique", "ultimate",
    /* 0x24    0x25       0x26      0x27 */
    "earth",  "neovim",  "emacs",  "vector",
    /* 0x28    0x29       0x2a      0x2b */
    "list",   "holee",   "sheesh", "cheese",
    /* 0x2c    0x2d       0x2e      0x2f */
    "anime",  "vm",      "wave",   "jinx",
    /* 0x30    0x31       0x32      0x33  */
    "docker", "coupon", "compile", "redundant",
    /* 0x34    0x35       0x36      0x37 */
    "music","repeating", "mecha",  "better",
    /* 0x38    0x39       0x3a      0x3b */
    "future", "lore",    "frog",   "rust",
    /* 0x3c    0x3d       0x3e      0x3f */
    "arch",   "btw",   "hydrated", "kitty",
];

const CSS_COLOR_NAMES: &[(&str, u32)] = &[
    ("black", 0x000000), ("silver", 0xc0c0c0), ("gray", 0x808080), ("white", 0xffffff),
    ("maroon", 0x800000), ("red", 0xff0000), ("purple", 0x800080), ("fuchsia", 0xff00ff),
    ("green", 0x008000), ("lime", 0x00ff00), ("olive", 0x808000), ("yellow", 0xffff00),
    ("navy", 0x000080), ("blue", 0x0000ff), ("teal", 0x008080), ("aqua", 0x00ffff),
    ("aliceblue", 0xf0f8ff), ("antiquewhite", 0xfaebd7), ("aquamarine", 0x7fffd4),
    ("aqua", 0x00ffff), ("azure", 0xf0ffff), ("beige", 0xf5f5dc), ("bisque", 0xffe4c4),
    ("black", 0x000000), ("blue", 0x0000ff), ("brown", 0xa52a2a), ("coral", 0xff7f50),
    ("cadetblue", 0x5f9ea0), ("chartreuse", 0x7fff00), ("chocolate", 0xd2691e),
    ("blueviolet", 0x8a2be2), ("blanchedalmond", 0xffebcd), ("cornflowerblue", 0x6495ed),
    ("burlywood", 0xdeb887), ("cornsilk", 0xfff8dc), ("crimson", 0xdc143c),
    ("cyan", 0x00ffff), ("darkblue", 0x00008b), ("darkcyan", 0x008b8b),
    ("darkgray", 0xa9a9a9), ("darkgreen", 0x006400), ("darkgrey", 0xa9a9a9),
    ("darkkhaki", 0xbdb76b), ("darkgoldenrod", 0xb8860b), ("darkmagenta", 0x8b008b),
    ("darkolivegreen", 0x556b2f), ("darkorange", 0xff8c00), ("darkorchid", 0x9932cc),
    ("darkred", 0x8b0000), ("darksalmon", 0xe9967a), ("darkseagreen", 0x8fbc8f),
    ("darkslateblue", 0x483d8b), ("darkslategray", 0x2f4f4f), ("darkslategrey", 0x2f4f4f),
    ("darkturquoise", 0x00ced1), ("darkviolet", 0x9400d3), ("deeppink", 0xff1493),
    ("deepskyblue", 0x00bfff), ("dimgray", 0x696969), ("dimgrey", 0x696969),
    ("dodgerblue", 0x1e90ff), ("firebrick", 0xb22222), ("floralwhite", 0xfffaf0),
    ("forestgreen", 0x228b22), ("fuchsia", 0xff00ff), ("gainsboro", 0xdcdcdc),
    ("ghostwhite", 0xf8f8ff), ("gold", 0xffd700), ("goldenrod", 0xdaa520),
    ("gray", 0x808080), ("green", 0x008000), ("greenyellow", 0xadff2f),
    ("grey", 0x808080), ("honeydew", 0xf0fff0), ("hotpink", 0xff69b4),
    ("indianred", 0xcd5c5c), ("indigo", 0x4b0082), ("ivory", 0xfffff0),
    ("khaki", 0xf0e68c), ("lavender", 0xe6e6fa), ("lavenderblush", 0xfff0f5),
    ("lawngreen", 0x7cfc00), ("lemonchiffon", 0xfffacd), ("lightblue", 0xadd8e6),
    ("lightcoral", 0xf08080), ("lightcyan", 0xe0ffff), ("lightgoldenrodyellow", 0xfafad2),
    ("lightgray", 0xd3d3d3), ("lightgreen", 0x90ee90), ("lightgrey", 0xd3d3d3),
    ("lightpink", 0xffb6c1), ("lightsalmon", 0xffa07a), ("lightseagreen", 0x20b2aa),
    ("lightskyblue", 0x87cefa), ("lightslategray", 0x778899), ("lightslategrey", 0x778899),
    ("lightsteelblue", 0xb0c4de), ("lightyellow", 0xffffe0), ("lime", 0x00ff00),
    ("limegreen", 0x32cd32), ("linen", 0xfaf0e6), ("magenta", 0xff00ff),
    ("maroon", 0x800000), ("mediumaquamarine", 0x66cdaa), ("mediumblue", 0x0000cd),
    ("mediumorchid", 0xba55d3), ("mediumpurple", 0x9370db), ("mediumseagreen", 0x3cb371),
    ("mediumslateblue", 0x7b68ee), ("mediumspringgreen", 0x00fa9a), ("mediumturquoise", 0x48d1cc),
    ("mediumvioletred", 0xc71585), ("midnightblue", 0x191970), ("mintcream", 0xf5fffa),
    ("mistyrose", 0xffe4e1), ("moccasin", 0xffe4b5), ("navajowhite", 0xffdead),
    ("navy", 0x000080), ("oldlace", 0xfdf5e6), ("olive", 0x808000),
    ("olivedrab", 0x6b8e23), ("orange", 0xffa500), ("orangered", 0xff4500),
    ("orchid", 0xda70d6), ("palegoldenrod", 0xeee8aa), ("palegreen", 0x98fb98),
    ("paleturquoise", 0xafeeee), ("palevioletred", 0xdb7093), ("papayawhip", 0xffefd5),
    ("peachpuff", 0xffdab9), ("peru", 0xcd853f), ("pink", 0xffc0cb),
    ("plum", 0xdda0dd), ("powderblue", 0xb0e0e6), ("purple", 0x800080),
    ("rebeccapurple", 0x663399), ("red", 0xff0000), ("rosybrown", 0xbc8f8f),
    ("royalblue", 0x4169e1), ("saddlebrown", 0x8b4513), ("salmon", 0xfa8072),
    ("sandybrown", 0xf4a460), ("seagreen", 0x2e8b57), ("seashell", 0xfff5ee),
    ("sienna", 0xa0522d), ("silver", 0xc0c0c0), ("skyblue", 0x87ceeb),
    ("slateblue", 0x6a5acd), ("slategray", 0x708090), ("slategrey", 0x708090),
    ("snow", 0xfffafa), ("springgreen", 0x00ff7f), ("steelblue", 0x4682b4),
    ("tan", 0xd2b48c), ("teal", 0x008080), ("thistle", 0xd8bfd8), ("tomato", 0xff6347),
    ("turquoise", 0x40e0d0), ("violet", 0xee82ee), ("wheat", 0xf5deb3), ("white", 0xffffff),
    ("whitesmoke", 0xf5f5f5), ("yellow", 0xffff00), ("yellowgreen", 0x9acd32),
];

async fn on_wsmessage(ws: &mut WebSocketStream<tokio_tungstenite::MaybeTlsStream<TcpStream>>) -> Result<WSData, SimError> {
    let mut wait_pong: Option<u32> = None;
    loop {
        let message =
            match timeout(Duration::from_secs(60), ws.next()).await
        {
            Ok(message) => message,
            Err(_) => {
                if wait_pong.is_some() {
                    eprintln!("connection timeout error");
                    Err(SimError::Timeout)?
                }
                if ws.send(WSMessage::Ping(vec![b'h', b'i', b' ', 0])).await.is_err() {
                    eprintln!("connection ping send error");
                    Err(SimError::SendError)?
                }
                wait_pong = Some(0);
                continue
            }
        };
        break Ok(match message {
            Some(Ok(WSMessage::Text(s))) => {
                if s.is_empty() { continue }
                WSData::Text(s)
            }
            Some(Ok(WSMessage::Binary(msg))) => {
                if msg.is_empty() { continue }
                WSData::Binary(msg)
            }
            Some(Ok(WSMessage::Ping(msg))) => {
                match ws.send(WSMessage::Pong(msg)).await {
                    Ok(()) => continue,
                    Err(_) => break Err(SimError::SendError),
                }
            }
            Some(Ok(WSMessage::Pong(_))) => {
                wait_pong = None;
                continue
            }
            Some(Ok(WSMessage::Close(Some(e)))) => {
                eprintln!("connection close: {}", e);
                Err(SimError::CloseMessage)?
            }
            Some(Ok(WSMessage::Close(None))) => Err(SimError::CloseMessage)?,
            Some(Ok(WSMessage::Frame{..})) => { Err(SimError::InvalidFrame)? }
            Some(Err(e)) => {
                eprintln!("connection error {}", e);
                Err(SimError::CloseError)?
            }
            None => Err(SimError::CloseError)?
        })
    }
}

#[derive(Debug)]
enum ExtCommand {
    LoadString(String),
    ThreadRun,
    ThreadRestart,
    ThreadHalt,
    ThreadClear,
    ThreadGetState,
}
struct ExtMessage {
    user: u64,
    cmd: ExtCommand,
}
type ExtClientNew = (mpsc::Sender<ClientData>, oneshot::Sender<u64>, oneshot::Sender<(String, mpsc::Sender<ExtMessage>)>);
struct ExtClient {
    to_socket: mpsc::Sender<ClientData>,
}
enum WSData {
    Text(String),
    Binary(Vec<u8>),
}

#[derive(Clone)]
enum ClientData {
    Binary(Arc<Vec<u8>>),
    Text(Arc<String>),
}
type ClientSend = mpsc::Sender<ClientData>;
enum ClientChannelMessage {
    NewClient(ClientSend),
    JsonMessage(JSValue),
}

async fn command_socket_task(mut command_rx: mpsc::Receiver<WSData>, command_tx: mpsc::Sender<JSValue>) {
    let mut error_was_display = false;
    loop {
        let mut stream = match tokio_tungstenite::connect_async("ws://127.0.0.1:23190").await {
            Ok((stream, _)) => stream,
            Err(e) => {
                if !error_was_display {
                    error_was_display = true;
                    eprintln!("command channel connect error {e}");
                }
                continue;
            }
        };
        if stream.send(WSMessage::text("command vm")).await.is_err() {
            continue;
        };
        error_was_display = false;
        println!("command channel connected");
        loop {
            let msg = tokio::select! { biased;
                res = command_rx.recv() => {
                    match match res.expect("simulation to command") {
                        WSData::Binary(v) => stream.send(WSMessage::Binary(v)).await,
                        WSData::Text(v) => stream.send(WSMessage::Text(v)).await,
                    } { Ok(_) => continue, Err(_) => break }
                }
                ws = on_wsmessage(&mut stream) => {
                    match ws { Ok(msg) => msg, Err(_) => break }
                }
            };
            if let WSData::Text(cmd) = msg {
                match serde_json::from_str::<JSValue>(&cmd) {
                    Ok(v) => command_tx.send(v).await.expect("command to simulation"),
                    Err(_) => break,
                }
            }
        }
        println!("command channel closed");
    }
}

async fn simulation(settings: &'static ServerState,
    mut sim_rx: mpsc::Receiver<ClientChannelMessage>,
    mut sim_ext_rx: mpsc::Receiver<ExtClientNew>,
    ) {
    let mut interval = time::interval(time::Duration::from_millis(18));
    let mut cull_interval = time::interval(time::Duration::from_millis(1000));
    let mut clients: Vec<ClientSend> = vec![];
    let mut ext_clients: Vec<(String, mpsc::Sender<ClientData>, oneshot::Sender<u64>)> = vec![];
    let mut ext_users: HashMap<u64, ExtClient> = HashMap::with_capacity(128);
    let (ext_tx, mut ext_rx) = mpsc::channel(256);
    let (cmd_rep_tx, cmd_rep_rx) = mpsc::channel(16);
    let (cmd_tx, mut cmd_rx) = mpsc::channel(16);
    let mut sim_vm = fs::read("wave.state").await.ok()
        .and_then(|state| read_persist(&state).ok()).unwrap_or_else(SimulationVM::new);
    tokio::spawn(command_socket_task(cmd_rep_rx, cmd_tx));
    enum SimulationSelect {
        None,
        Cull,
        Tick,
        ExtMessage(ExtMessage),
        Command(JSValue),
        FrontCommand(JSValue),
    }
    let mut should_run = settings.should_run_rx.clone();
    loop {
        match tokio::select! { biased;
            r = should_run.changed() => {
                if r.is_ok() { continue }
                if let Ok(state) = write_persist(sim_vm.as_ref()) {
                    if let Err(e) = fs::write("wave.state", &state).await {
                        eprintln!("shutdown: saving vm state failed: {e}");
                    } else {
                        eprintln!("shutdown: vm state saved");
                    }
                } else {
                    eprintln!("shutdown: serialize vm state failed");
                }
                return
            }
            resp = sim_rx.recv() => match resp {
                Some(ClientChannelMessage::NewClient(client)) => {
                    clients.push(client);
                    eprintln!("new client");
                    sim_vm.memory_invalidate();
                    SimulationSelect::None
                }
                Some(ClientChannelMessage::JsonMessage(message)) => {
                    SimulationSelect::FrontCommand(message)
                }
                None => { eprintln!("simulation is ending, clients channel closed"); return }
            },
            _ = cull_interval.tick() => SimulationSelect::Cull,
            _ = interval.tick() => SimulationSelect::Tick,
            resp = cmd_rx.recv() => match resp {
                Some(cmd) => SimulationSelect::Command(cmd),
                None => { eprintln!("command task ended, simulation abort"); return }
            },
            resp = ext_rx.recv() => match resp {
                Some(msg) => SimulationSelect::ExtMessage(msg),
                None => { eprintln!("simulation task is ending, ext command channel is closed"); return }
            },
            resp = sim_ext_rx.recv() => match resp {
                None => { eprintln!("simulation is ending, ext clients channel closed"); return }
                Some((data_tx, auth_tx, ack_tx)) => {
                    let auth_id: u32 = random();
                    let id_0 = (auth_id & 0x3f) as usize;
                    let id_1 = ((auth_id >> 6) & 0x3f) as usize;
                    let id_2 = ((auth_id >> 12) & 0x3f) as usize;
                    let id_3 = ((auth_id >> 18) & 0x3f) as usize;
                    let auth_token = format!("{} {} {} {}",
                        RANDOM_WORDS[id_0], RANDOM_WORDS[id_1],
                        RANDOM_WORDS[id_2], RANDOM_WORDS[id_3]);
                    if ack_tx.send((auth_token.clone(), ext_tx.clone())).is_err() {
                        continue // this should drop the connection
                    }
                    ext_clients.push((auth_token, data_tx, auth_tx));
                    SimulationSelect::None
                }
            },
        } { 
            SimulationSelect::None => {}
            SimulationSelect::Cull => {
                let mut index = ext_clients.len();
                while index > 0 {
                    index -= 1;
                    let (_, send_tx, auth_tx) = &ext_clients[index];
                    if send_tx.is_closed() || auth_tx.is_closed() {
                        ext_clients.swap_remove(index);
                    }
                }
                for (k,v) in ext_users.iter() {
                    if v.to_socket.is_closed() {
                        let id = *k;
                        ext_users.remove(&id);
                        break
                    }
                }
            }
            SimulationSelect::Tick => {
                sim_vm.tick(168);
                let msg = sim_vm.delta_encode();
                let mut client_index = 0;
                while client_index < clients.len() {
                    let client = &clients[client_index];
                    match client.try_send(ClientData::Binary(msg.clone())) {
                        Ok(()) => (),
                        Err(mpsc::error::TrySendError::Closed(_)) => {
                            clients.swap_remove(client_index);
                            eprintln!("del client");
                            continue;
                        }
                        Err(mpsc::error::TrySendError::Full(_)) => { eprintln!("output full"); }
                    }
                    client_index += 1;
                }
            }
            SimulationSelect::ExtMessage(ExtMessage { user, cmd }) => {
                eprintln!("sim ext command channel recv message {user}, {cmd:?}")
            }
            SimulationSelect::FrontCommand(message) => {
                let JSValue::Array(v) = message else { continue };
                let mut args = v.into_iter();
                fn get_me_the_next_i64_please(args: &mut std::vec::IntoIter<JSValue>) -> Option<i64> {
                    let JSValue::Number(value) = args.next()? else { return None };
                    value.as_i64()
                }
                let Some(command_code) = get_me_the_next_i64_please(&mut args) else { continue };
                match command_code {
                    1 => {
                        let Some(eid) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(x) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(y) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(user) = sim_vm.find_user_eid(eid as u32) else { continue };
                        user.context.ship.x = x as f32;
                        user.context.ship.y = y as f32;
                    }
                    2 => {
                        let Some(eid) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(x) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(y) = get_me_the_next_i64_please(&mut args) else { continue };
                        let Some(user) = sim_vm.find_user_eid(eid as u32) else { continue };
                        user.context.ship.vel_x = x as f32;
                        user.context.ship.vel_y = y as f32;
                    }
                    _ => {}
                }
            }
            SimulationSelect::Command(message) => {
                let JSValue::Object(mut outer) = message else { continue };
                let Some(JSValue::Object(mut inner)) = outer.remove("Command") else { continue };
                let Some(JSValue::Array(v)) = inner.remove("data") else { continue };
                println!("command: {v:?}");
                let mut args = v.into_iter();
                let Some(JSValue::String(s)) = args.next() else { continue };
                match s.as_str() {
                    "resetall" => { sim_vm.sys_halt_all(); }
                    "reload" => {
                        if let Ok(state) = fs::read("wave.state").await {
                            if let Ok(vm) = read_persist(&state) {
                                eprintln!("loaded vm state from file");
                                *sim_vm.as_mut() = vm;
                            }
                        }
                    }
                    "save" => {
                        if let Ok(state) = write_persist(sim_vm.as_ref()) {
                            if let Err(e) = fs::write("wave.state", &state).await {
                                eprintln!("saving vm state failed: {e}");
                            } else {
                                eprintln!("vm state saved");
                            }
                        } else {
                            eprintln!("serialize vm state failed");
                        }
                    }
                    "input" => {
                        let Some(JSValue::String(u)) = args.next() else { continue };
                        let Ok(user_id) = u.parse::<u64>() else { continue };
                        let Some(JSValue::String(user_login)) = args.next() else { continue };
                        let Some(JSValue::String(user_name)) = args.next() else { continue };
                        let Some(JSValue::String(user_color_str)) = args.next() else { continue };
                        let Some(JSValue::String(message)) = args.next() else { continue };
                        if let Some(auth) =
                            sim_parse_chat(settings,
                                &cmd_rep_tx,
                                &mut sim_vm,
                                user_id, user_login, user_name, user_color_str,
                                message
                                ).await {
                            for (index, (id, _, _)) in ext_clients.iter().enumerate() {
                                if auth == *id {
                                    let (_, client_tx, auth_tx) = ext_clients.swap_remove(index);
                                    if auth_tx.send(user_id).is_err() { break }
                                    ext_users.insert(user_id, ExtClient{to_socket: client_tx});
                                    break
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

async fn ws_listener_task(settings: &'static ServerState) -> Result<(), SimError> {
    let vmio_bind =
        format!("{}:{}", &settings.vmio_listen_address, &settings.vmio_listen_port);
    let ext_bind =
        format!("{}:{}", &settings.ext_listen_address, &settings.ext_listen_port);
    let vmio_listen = TcpListener::bind(&vmio_bind).await?;
    let ext_listen = TcpListener::bind(&ext_bind).await?;
    loop {
        tokio::select! {
            _ = settings.simulation_vmio_tx.closed() => break,
            Ok((stream, addr)) = vmio_listen.accept() => {
                tokio::spawn(vmio_handler(settings, stream, addr));
            }
            Ok((stream, addr)) = ext_listen.accept() => {
                tokio::spawn(ext_handler(settings, stream, addr));
            }
            _ = tokio::signal::ctrl_c() => break,
        }
    }
    Ok(())
}

async fn ext_handler(
    settings: &'static ServerState,
    stream: TcpStream, addr: SocketAddr
) {
    println!("Ext connection from {}", addr);
    let res: Result<(), SimError> = 'WSErr: {
        if let Err(e) = stream.set_nodelay(true) {
            break 'WSErr Err(e.into());
        }
        let mut stream =
            match tokio_tungstenite::accept_async(
                tokio_tungstenite::MaybeTlsStream::Plain(stream)).await {
                Ok(v) => v,
                Err(e) => break 'WSErr Err(e.into()),
            };
        let (chan_tx, mut chan_rx) = mpsc::channel::<ClientData>(16);
        let (ack_tx, ack_rx) = oneshot::channel();
        let (auth_tx, mut auth_rx) = oneshot::channel();
        if settings.simulation_ext_client_tx.send((chan_tx, auth_tx, ack_tx)).await.is_err() {
            break 'WSErr Err(SimError::SendError)
        }
        let (auth_token, reply_chan) = match ack_rx.await {
            Ok(v) => v, Err(_) => break 'WSErr Err(SimError::CloseError)
        };
        let auth_id = loop {
            let msg = tokio::select! { biased;
                res = &mut auth_rx => {
                    match res {
                        Ok(v) => break v,
                        Err(_) => break 'WSErr Err(SimError::CloseError)
                    }
                }
                ws = on_wsmessage(&mut stream) => {
                    match ws {
                        Ok(msg) => Some(msg),
                        Err(_) => break 'WSErr Err(SimError::CloseError)
                    }
                }
            };
            match msg {
                None => {}
                Some(WSData::Binary(_)) => {}
                Some(WSData::Text(s)) => {
                    let mut tokens = s.split_whitespace();
                    if let Some("auth") = tokens.next() {
                        if let Err(e) = stream.send(WSMessage::Text(auth_token.clone())).await {
                            break 'WSErr Err(e.into())
                        }
                    }
                }
            }
        };
        if let Err(e) = stream.send(WSMessage::Text(format!("authed {auth_id}"))).await {
            break 'WSErr Err(e.into())
        }
        loop {
            let result = loop {
                tokio::select! {
                    value = chan_rx.recv() => {
                        if let Some(value) = value {
                            if let Err(e) = stream.send(match value {
                                ClientData::Binary(value) => {
                                    WSMessage::Binary(Arc::unwrap_or_clone(value))
                                }
                                ClientData::Text(value) => WSMessage::Text(value.to_string()),
                            }).await {
                                break Err(e.into())
                            }
                        } else {
                            break Err(SimError::CloseError)
                        }
                    }
                    ws = on_wsmessage(&mut stream) => break ws,
                }
            };
            match result {
                Err(e) => {
                    println!("disconnection from {} via {:#?}", addr, e);
                    break 'WSErr Err(e)
                }
                Ok(WSData::Text(s)) => {
                    let mut tokens = s.split_whitespace();
                    match tokens.next() {
                        Some("halt") => {
                            if reply_chan.send(ExtMessage { user: auth_id, cmd: ExtCommand::ThreadHalt }).await.is_err() {
                                break 'WSErr Err(SimError::SendError)
                            }
                        }
                        Some("run") => {
                            if reply_chan.send(ExtMessage { user: auth_id, cmd: ExtCommand::ThreadRun }).await.is_err() {
                                break 'WSErr Err(SimError::SendError)
                            }
                        }
                        Some("restart") => {
                            if reply_chan.send(ExtMessage { user: auth_id, cmd: ExtCommand::ThreadRestart }).await.is_err() {
                                break 'WSErr Err(SimError::SendError)
                            }
                        }
                        Some("clear") => {
                            if reply_chan.send(ExtMessage { user: auth_id, cmd: ExtCommand::ThreadClear }).await.is_err() {
                                break 'WSErr Err(SimError::SendError)
                            }
                        }
                        Some("get") => {
                            if reply_chan.send(ExtMessage { user: auth_id, cmd: ExtCommand::ThreadGetState }).await.is_err() {
                                break 'WSErr Err(SimError::SendError)
                            }
                        }
                        _ => {}
                    }
                }
                Ok(_) => { }
            }
        }
    };
    match res {
        Ok(()) => { println!("closed {}", addr); }
        Err(e) => { println!("closed {} error: {}", addr, e); }
    }
}

async fn vmio_handler(
    settings: &'static ServerState,
    stream: TcpStream, addr: SocketAddr
) {
    println!("VMIO connection from {}", addr);
    let res: Result<(), SimError> = 'WSErr: {
        if let Err(e) = stream.set_nodelay(true) {
            break 'WSErr Err(e.into());
        }
        let mut stream =
            match tokio_tungstenite::accept_async(
                tokio_tungstenite::MaybeTlsStream::Plain(stream)).await {
                Ok(v) => v,
                Err(e) => break 'WSErr Err(e.into()),
            };
        let (chan_tx, mut chan_rx) = mpsc::channel::<ClientData>(16);
        if settings.simulation_vmio_tx.send(ClientChannelMessage::NewClient(chan_tx)).await.is_err() {
            break 'WSErr Err(SimError::SendError)
        }
        loop {
            let result = tokio::select! {
                value = chan_rx.recv() => {
                    let Some(value) = value else {
                        break Err(SimError::CloseError)
                    };
                    if let Err(e) = stream.send(match value {
                        ClientData::Binary(value) => {
                            WSMessage::Binary(Arc::unwrap_or_clone(value))
                        }
                        ClientData::Text(value) => WSMessage::Text(value.to_string()),
                    }).await {
                        break Err(e.into())
                    }
                    None
                }
                ws = on_wsmessage(&mut stream) => {
                    Some(ws)
                }
            };
            match result {
                Some(Err(e)) => {
                    println!("disconnection from {} via {:#?}", addr, e);
                    break 'WSErr Err(e)
                }
                Some(Ok(WSData::Text(msg))) => {
                    let Ok(message) = serde_json::from_str::<serde_json::Value>(&msg) else {
                        continue;
                    };
                    settings.simulation_vmio_tx.try_send(ClientChannelMessage::JsonMessage(message)).ok();
                }
                Some(Ok(_)) => { }
                None => {}
            }
        }
    };
    match res {
        Ok(()) => { println!("closed {}", addr); }
        Err(e) => { println!("closed {} error: {}", addr, e); }
    }
}

struct ServerState {
    css_names: HashMap<&'static str, u32>,
    vmio_listen_port: u16,
    vmio_listen_address: String,
    ext_listen_port: u16,
    ext_listen_address: String,
    should_run_rx: watch::Receiver<()>,
    simulation_vmio_tx: mpsc::Sender<ClientChannelMessage>,
    simulation_ext_client_tx: mpsc::Sender<ExtClientNew>,
    help_message: ClientCommand,
}
impl ServerState {
    fn parse_color_string(&self, color: &str) -> Option<u32> {
        if let Some(color) = self.css_names.get(color) {
            return Some(*color)
        }
        if !color.starts_with('#') { return None }
        match color.len() {
            4 => if let (Some(r), Some(g), Some(b)) = (
                color.get(1..2).and_then(|v| u32::from_str_radix(v, 16).ok()),
                color.get(2..3).and_then(|v| u32::from_str_radix(v, 16).ok()),
                color.get(3..4).and_then(|v| u32::from_str_radix(v, 16).ok())) {
                Some( (r << 20) | (r << 16)
                    | (g << 12) | (g << 8)
                    | (b << 4) | b)
            } else { None }
            7 => if let (Some(r), Some(g), Some(b)) = (
                color.get(1..3).and_then(|v| u32::from_str_radix(v, 16).ok()),
                color.get(3..5).and_then(|v| u32::from_str_radix(v, 16).ok()),
                color.get(5..7).and_then(|v| u32::from_str_radix(v, 16).ok())) {
                Some((r << 16) | (g << 8) | b)
            } else { None }
            _ => None
        }
    }
}

fn main() -> Result<(), SimError> {
    let (simulation_vmio_tx, sim_rx) = mpsc::channel(16);
    let (simulation_ext_client_tx, sim_ext_rx) = mpsc::channel(16);
    let (should_run_tx, should_run_rx) = watch::channel(());
    let run = Box::new(runtime::Builder::new_multi_thread().enable_all().build()?);
    let run: &'static runtime::Runtime = Box::leak(run);
    let help_message: ClientCommand = ClientCommand::ChatMessage(
        String::from(r"how to use the overlay VM: github.com/Meisaka/MeiVM2/blob/main/vm.txt")
    );
    let mut css_names = HashMap::with_capacity(CSS_COLOR_NAMES.len());
    for (name, color_code) in CSS_COLOR_NAMES {
        css_names.insert(*name, *color_code);
    }
    let settings = Box::leak(Box::new(ServerState {
        css_names,
        vmio_listen_port: 23192,
        vmio_listen_address: String::from("0.0.0.0"),
        ext_listen_port: 23191,
        ext_listen_address: String::from("0.0.0.0"),
        should_run_rx,
        simulation_vmio_tx,
        simulation_ext_client_tx,
        help_message,
    }));
    run.block_on(async {
        tokio::spawn(simulation(settings, sim_rx, sim_ext_rx));
        ws_listener_task(settings).await?;
        should_run_tx.send(()).ok();
        drop(should_run_tx);
        Ok(())
    })
}

