
use std::process::ExitCode;
use std::time::Duration;
use meivm2::{SimulationVM, vm_write, write_persist, read_persist};
use std::sync::mpsc;

fn parse_command(sim_vm: &mut SimulationVM, user_id: &mut u64, run_all: &mut bool, message: &str) -> Option<()> {
    //eprintln!("sim got chat: {}:{}", user, message);
    let mut split = message.split_whitespace();
    while let Some(command) = split.next() {
        match command {
        "run" | "go" | "start" => {
            sim_vm.user_run(*user_id);
        } "write" => {
            let vmproc = &mut sim_vm.make_user(*user_id).proc;
            vm_write(&mut split, vmproc.as_mut(), 0);
        } "code" => {
            let vmproc = &mut sim_vm.make_user(*user_id).proc;
            vm_write(&mut split, vmproc.as_mut(), 0x40);
        } "stop" | "halt" | "crash" => {
            sim_vm.user_halt(*user_id);
        } "reset" | "clear" => {
            sim_vm.user_reset(*user_id);
        } "restart" => {
            sim_vm.user_restart(*user_id);
            sim_vm.user_run(*user_id);
        } "load" => {
            match std::fs::read("wave.state") {
                Ok(buf) => match read_persist::<SimulationVM>(&buf) {
                    Ok(vm) => {
                        *sim_vm = vm;
                        eprintln!("load tested successfully");
                    }
                    Err(e) => {
                        eprintln!("load parse error: {e:?}");
                    }
                }
                Err(e) => {
                    eprintln!("load file error: {e}");
                }
            }
        } "save" => {
            if let Ok(save) = write_persist(sim_vm) {
                match std::fs::write("wave.state", save.as_slice()) {
                    Ok(()) => eprintln!("success"),
                    Err(e) => eprintln!("save error: {e}"),
                }
            }
        } "dump" => {
            sim_vm.user_dump(*user_id);
        } "help" | "commands" | "?" => {
            eprintln!(r"how to use the overlay VM: github.com/Meisaka/MeiVM2/blob/main/vm.txt
help  (link to the info page, and this list of commands)
asm <program>  (tiny in-chat assembler: TODO)
halt      (stop your thread)
run       (default the instruction pointer and start your thread)
restart   (defaults the instruction register, without directly affecting any other registers)
clear     (resets all thread memory and registers to their defaults)
dump      (prints the current state of the VM to the terminal, not very useful remotely)
write <data>
code <data>
user <user_number>  (CLI: switch to a different user context)
load                (CLI: restore all state from save file)
save                (CLI: save state of all threads and memory)
exit, quit, q       (leave the emulator CLI)
threads             (CLI: display running threads)
debug               (CLI: stop processing ticks)
resume              (CLI: enable processing ticks)
s, step, tick       (CLI: run a single tick)
"
                );
        } "user" => { *user_id = split.next()?.parse().ok()?; }
        "debug" => { *run_all = false; }
        "resume" => { *run_all = true; }
        "s" | "step" | "tick" => {
            sim_vm.tick(1);
            if *run_all == false {
                for vm_thread in sim_vm.threads() {
                    eprintln!("{}", vm_thread);
                }
            }
        }
        "threads" => {
            for vm_thread in sim_vm.threads() {
                eprintln!("{}", vm_thread);
            }
        }
        _ => {}
        }
    }
    None
}

fn simulation(sim_rx: mpsc::Receiver<String>) {
    let tick_size = Duration::from_millis(50);
    let mut sim_vm = SimulationVM::new();
    let mut active_user: u64 = 0;
    let mut run_all: bool = true;
    loop {
        match sim_rx.recv_timeout(tick_size) {
            Ok(v) => {
                parse_command(&mut sim_vm, &mut active_user, &mut run_all, &v);
                // TODO
                // new "users"
            }
            Err(mpsc::RecvTimeoutError::Timeout) => (),
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                eprintln!("simulation task is ending, data channel is closed");
                return
            }
        }
        if run_all {
            sim_vm.tick(20);
        }
        // TODO console display of state maybe
    }
}

fn main() -> ExitCode {
    let (sim_channel_tx, sim_channel_rx) = mpsc::sync_channel(16);
    std::thread::spawn(|| {
        simulation(sim_channel_rx)
    });
    use std::io::Read;
    let cin = std::io::stdin();
    let mut buf = [0u8; 1024];
    let mut unacceptable = 0;
    let mut last_command: Option<String> = None;
    loop {
        if let Some(mut amount) = {
            let mut lock_cin = cin.lock();
            match lock_cin.read(&mut buf) {
                Ok(amount) => Some(amount),
                Err(e) => {
                    eprintln!("oh noes {}", e);
                    unacceptable += 1;
                    if unacceptable > 10 { break }
                    None
                }
            }
        } {
            unacceptable = 0;
            eprintln!("Input: {} bytes", amount);
            if amount == 0 { break }
            if amount > 0 && buf[amount - 1] == b'\n' { amount -= 1; }
            let (pre, _) = buf.split_at(amount);
            if pre.is_empty() {
                if let Some(s) = &last_command {
                    sim_channel_tx.try_send(s.clone()).ok();
                }
            } else if let Ok(s) = String::from_utf8(pre.to_vec()) {
                last_command = Some(s.clone());
                if s == "exit" || s == "quit" || s == "q" {
                    break
                }
                sim_channel_tx.try_send(s).ok();
            }
        }
    }
    ExitCode::SUCCESS
}

