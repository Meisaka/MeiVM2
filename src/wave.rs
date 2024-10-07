
use std::process::ExitCode;
use std::time::{self, Duration};
use meivm2::SimulationVM;
use std::sync::mpsc;

fn vm_write(split: &mut std::str::SplitWhitespace, sim_vm: &mut SimulationVM, user_id: u64, start_addr: u16) -> () {
    let mut val: u16 = 0;
    let mut addr = start_addr;
    let mut order = [12,8,4,0].into_iter().enumerate().cycle();
    let mut ofs_index = 0;
    let mut command_break = false;
    for omnom in split {
        for bite in omnom.as_bytes().iter() {
            match bite {
                b'0'..=b'9' => {
                    let (index, ofs) = order.next().unwrap();
                    val |= ((bite - b'0') as u16) << ofs;
                    ofs_index = index;
                },
                b'A'..=b'F' => {
                    let (index, ofs) = order.next().unwrap();
                    val |= ((bite - b'A' + 10) as u16) << ofs;
                    ofs_index = index;
                },
                b'a'..=b'f' => {
                    let (index, ofs) = order.next().unwrap();
                    val |= ((bite - b'a' + 10) as u16) << ofs;
                    ofs_index = index;
                },
                b'!' => { command_break = true; }
                _ => ()
            }
            if ofs_index == 3 {
                sim_vm.user_write(user_id, addr, val);
                val = 0;
                addr += 1;
            }
        }
        if command_break { break }
    }
    if ofs_index != 3 {
        sim_vm.user_write(user_id, addr, val);
    }
}

fn parse_command(sim_vm: &mut SimulationVM, user_id: &mut u64, run_all: &mut bool, message: &str) -> Option<()> {
    //eprintln!("sim got chat: {}:{}", user, message);
    let mut split = message.split_whitespace();
    while let Some(command) = split.next() {
        match command {
        "run" | "go" | "start" => {
            sim_vm.user_run(*user_id);
        } "write" => {
            vm_write(&mut split, sim_vm, *user_id, 0);
        } "code" => {
            vm_write(&mut split, sim_vm, *user_id, 0x40);
        } "stop" | "halt" | "crash" => {
            sim_vm.user_halt(*user_id);
        } "reset" | "clear" => {
            sim_vm.user_reset(*user_id);
        } "restart" => {
            sim_vm.user_restart(*user_id);
            sim_vm.user_run(*user_id);
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
            let msg = sim_vm.tick(20);
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
            if amount > 0 && buf[amount - 1] == b'\n' { amount -= 1; }
            let (pre, _) = buf.split_at(amount);
            if let Some(s) = String::from_utf8(pre.to_vec()).ok() {
                if s == "exit" || s == "quit" || s == "q" {
                    break
                }
                sim_channel_tx.try_send(s).ok();
            }
        }
    }
    ExitCode::SUCCESS
}

