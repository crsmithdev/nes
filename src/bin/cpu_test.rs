#[macro_use]
extern crate log;

use env_logger::fmt::Color;
use log::Level;
use nes::nes::cpu;
use std::io::Write;

const TEST_ROM: &'static [u8] = include_bytes!("../../data/6502_functional_test.bin");
const TEST_START: u16 = 0x400;

fn main() {
    env_logger::builder()
        .format(|buf, record| {
            let mut level_style = buf.style();
            level_style.set_color(match record.level() {
                Level::Debug => Color::Cyan,
                Level::Trace => Color::Rgb(0xCC, 0xCC, 0xCC),
                Level::Warn => Color::Yellow,
                Level::Error => Color::Red,
                _ => Color::White,
            });
            let path = match record.module_path() {
                Some(path) => format!("[{}]", path),
                None => "".to_owned(),
            };
            writeln!(
                buf,
                "[{}]{} {}",
                level_style.value(record.level()),
                path,
                record.args()
            )
        })
        .init();

    let mut vm = cpu::VM::default();
    vm.load_program(&*TEST_ROM, TEST_START);
    let mut trap_addr = 0;
    let mut trap_counter = 0;

    loop {
        vm.update();
        if let Some(err) = vm.error {
            error!("cpu error: {}", err);
            break;
        }
        if vm.cpu.ir_addr == trap_addr {
            trap_counter += 1;
            if trap_counter > 20 {
                error!(
                    "trap detected: cycle {} @ {:#04X} -> {} ({:?})",
                    vm.cpu.cycles, vm.cpu.ir_addr, vm.cpu.ir, vm.cpu
                );
                break;
            }
        } else {
            trap_addr = vm.cpu.ir_addr;
            trap_counter = 0;
        }
    }
}
