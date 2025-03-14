
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
        if command.starts_with("run") || command.starts_with("go")
            || command.starts_with("start") || command.starts_with("commence")
            || command.starts_with("begin") || command.starts_with("launch")
            || command.starts_with("execute")
        {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_run(user_id);
        } else if command.starts_with("write") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            let vmproc = &mut sim_vm.make_user(user_id).proc;
            vm_write(&mut split, vmproc.as_mut(), 0);
        } else if command.starts_with("code") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            let vmproc = &mut sim_vm.make_user(user_id).proc;
            vm_write(&mut split, vmproc.as_mut(), 0x40);
        } else if command.starts_with("stop") || command.starts_with("halt") || command.starts_with("crash") || command.starts_with("lunch") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_halt(user_id);
        } else if command.starts_with("reset") || command.starts_with("clear") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_reset(user_id);
        } else if command.starts_with("restart") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_restart(user_id);
            sim_vm.user_run(user_id);
        } else if command.starts_with("dump") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_dump(user_id);
        } else if command.starts_with("summon") {
            user_params.take().map(|(user_login, user_name, user_color)|
                sim_vm.user_new_with_update(user_id, user_name, user_login, user_color));
            sim_vm.user_new(user_id);
        } else if command == "auth" || command == "login" || command == "logon" {
            let id = (split.next()?, split.next()?, split.next()?, split.next()?);
            return Some(format!("{} {} {} {}", id.0, id.1, id.2, id.3));
        } else if command.starts_with("help") || command.starts_with("commands") {
            let s = serde_json::to_string(&settings.help_message).ok()?;
            reply.send(WSData::Text(s)).await.ok()?;
        }
    }
    None
}

const RANDOM_WORDS: &[&str] = &[
    /* 0x00 */ "eek",
    /* 0x01 */ "meh",
    /* 0x02 */ "ohmega",
    /* 0x03 */ "nyan",
    /* 0x04 */ "fops",
    /* 0x05 */ "ehlo",
    /* 0x06 */ "fimsh",
    /* 0x07 */ "yeet",
    /* 0x08 */ "cooking",
    /* 0x09 */ "cookie",
    /* 0x0a */ "lisp",
    /* 0x0b */ "brainrot",
    /* 0x0c */ "sigma",
    /* 0x0d */ "spam",
    /* 0x0e */ "eepy",
    /* 0x0f */ "mood",
    /* 0x10 */ "memory",
    /* 0x11 */ "cursed",
    /* 0x12 */ "thing",
    /* 0x13 */ "word",
    /* 0x14 */ "vampire",
    /* 0x15 */ "bird",
    /* 0x16 */ "foxgoddess",
    /* 0x17 */ "clearly",
    /* 0x18 */ "evil",
    /* 0x19 */ "factory",
    /* 0x1a */ "chaos",
    /* 0x1b */ "cute",
    /* 0x1c */ "stare",
    /* 0x1d */ "song",
    /* 0x1e */ "maybe",
    /* 0x1f */ "reverse",
    /* 0x20 */ "instruction",
    /* 0x21 */ "unique",
    /* 0x22 */ "e",
    /* 0x23 */ "ultimate",
    /* 0x24 */ "earth",
    /* 0x25 */ "neovim",
    /* 0x26 */ "emacs",
    /* 0x27 */ "vector",
    /* 0x28 */ "list",
    /* 0x29 */ "holee",
    /* 0x2a */ "sheesh",
    /* 0x2b */ "cheese",
    /* 0x2c */ "anime",
    /* 0x2d */ "vm",
    /* 0x2e */ "wave",
    /* 0x2f */ "jinx",
    /* 0x30 */ "docker",
    /* 0x31 */ "coupon",
    /* 0x32 */ "compile",
    /* 0x33 */ "redundant",
    /* 0x34 */ "music",
    /* 0x35 */ "repeating",
    /* 0x36 */ "mecha",
    /* 0x37 */ "better",
    /* 0x38 */ "future",
    /* 0x39 */ "lore",
    /* 0x3a */ "frog",
    /* 0x3b */ "rust",
    /* 0x3c */ "arch",
    /* 0x3d */ "btw",
    /* 0x3e */ "hydrated",
    /* 0x3f */ "kitty",
];

async fn on_wsmessage(ws: &mut WebSocketStream<tokio_tungstenite::MaybeTlsStream<TcpStream>>) -> Result<WSData, SimError> {
    let mut wait_pong: Option<u32> = None;
    loop {
        let message =
            match timeout(Duration::from_secs(60), ws.next()).await
        {
            Ok(message) => message,
            Err(_) => {
                if let Some(_) = &wait_pong {
                    eprintln!("connection timeout error");
                    Err(SimError::Timeout)?
                }
                if let Err(_) = ws.send(WSMessage::Ping(vec![b'h', b'i', b' ', 0])).await {
                    eprintln!("connection ping send error");
                    Err(SimError::SendError)?
                }
                wait_pong = Some(0);
                continue
            }
        };
        break Ok(match message {
            Some(Ok(WSMessage::Text(s))) => {
                if s.len() == 0 { continue }
                WSData::Text(s)
            }
            Some(Ok(WSMessage::Binary(msg))) => {
                if msg.len() == 0 { continue }
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
        if let Err(_) = stream.send(WSMessage::text("command vm")).await {
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
                    Ok(v) => command_tx.send(v).await.ok().expect("command to simulation"),
                    Err(_) => break,
                }
            }
        }
        println!("command channel closed");
    }
}

async fn simulation(settings: &'static ServerState,
    mut sim_rx: mpsc::Receiver<ClientSend>,
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
        .and_then(|state| read_persist(&state).ok()).unwrap_or_else(|| SimulationVM::new());
    tokio::spawn(command_socket_task(cmd_rep_rx, cmd_tx));
    enum SimulationSelect {
        None,
        Cull,
        Tick,
        ExtMessage(ExtMessage),
        Command(JSValue),
    }
    let mut should_run = settings.should_run_rx.clone();
    loop {
        match tokio::select! { biased;
            r = should_run.changed() => {
                if r.is_ok() { continue }
                if let Some(state) = write_persist(sim_vm.as_ref()).ok() {
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
                Some(client) => {
                    clients.push(client);
                    eprintln!("new client");
                    sim_vm.memory_invalidate();
                    SimulationSelect::None
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
                let msg = sim_vm.tick(42);
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
                        if let Some(state) = fs::read("wave.state").await.ok() {
                            if let Some(vm) = read_persist(&state).ok() {
                                eprintln!("loaded vm state from file");
                                *sim_vm.as_mut() = vm;
                            }
                        }
                    }
                    "save" => {
                        if let Some(state) = write_persist(sim_vm.as_ref()).ok() {
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
) -> () {
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
        if let Err(_) =
            settings.simulation_ext_client_tx.send((chan_tx, auth_tx, ack_tx)).await {
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
                    match tokens.next() {
                        Some("auth") => {
                            if let Err(e) = stream.send(WSMessage::Text(auth_token.clone())).await {
                                break 'WSErr Err(e.into())
                            }
                        }
                        _ => {}
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
                                    if Arc::strong_count(&value) > 1 { println!("vm update is cloning"); }
                                    WSMessage::Binary(Arc::unwrap_or_clone(value).into())
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
) -> () {
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
        if let Err(_) = settings.simulation_vmio_tx.send(chan_tx).await {
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
                            if Arc::strong_count(&value) > 1 { println!("vm update is cloning"); }
                            WSMessage::Binary(Arc::unwrap_or_clone(value).into())
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
    vmio_listen_port: u16,
    vmio_listen_address: String,
    ext_listen_port: u16,
    ext_listen_address: String,
    should_run_rx: watch::Receiver<()>,
    simulation_vmio_tx: mpsc::Sender<ClientSend>,
    simulation_ext_client_tx: mpsc::Sender<ExtClientNew>,
    help_message: ClientCommand,
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
    let settings = Box::leak(Box::new(ServerState {
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

