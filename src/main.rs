extern crate gfx_backend_metal as back;
#[macro_use]
extern crate objc;
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate maplit;

use chrono::Duration;
use env_logger::fmt::Color;
use log::Level;
use std::io::Write;
use std::time::SystemTime;
use winit::{
    event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};

#[macro_use]
mod base;
mod cpu;
mod graphics;
mod platform;
mod ui;

const TEST_ROM: &'static [u8] = include_bytes!("../data/6502_functional_test.bin");
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

    let event_loop = EventLoop::new();
    let mut graphics = ui::Graphics::new(&event_loop);
    let mut instructions = ui::InstructionsWindow::new(&event_loop);
    let mut registers = ui::InternalsWindow::new(&event_loop);
    let _ = ui::MainMenu::new();

    let mut vm = cpu::VM::default();
    vm.load_program(&*TEST_ROM, TEST_START);
    let proxy = event_loop.create_proxy();
    let timer = timer::Timer::new();
    let _guard = timer.schedule_repeating(Duration::milliseconds(2), move || {
        proxy.send_event(()).unwrap();
    });

    graphics.resize(1024, 768); // TODO
    instructions.load(&vm);
    registers.update(&vm.cpu);
    ui::activate();
    let mut paused = false;
    let mut ui_updated = SystemTime::now();

    event_loop.run(move |event, _, control| {
        *control = ControlFlow::Wait;

        match event {
            Event::WindowEvent { event, window_id } => match event {
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::Resized(dims) => {
                    if window_id == graphics.id() {
                        graphics.resize(dims.width, dims.height);
                    }
                }
                WindowEvent::KeyboardInput { input, .. } => match input {
                    KeyboardInput {
                        virtual_keycode: Some(key),
                        state: ElementState::Pressed,
                        ..
                    } => match key {
                        VirtualKeyCode::Right => {
                            if paused {
                                vm.update();
                            }
                        }
                        VirtualKeyCode::Space => {
                            paused = !paused;
                        }
                        _ => (),
                    },
                    _ => (),
                },
                _ => {}
            },
            Event::RedrawEventsCleared => (), // TODO
            Event::UserEvent(_) => {
                if !paused {
                    vm.update();
                }
                // graphics.update();
                let now = SystemTime::now();
                let elapsed = now.duration_since(ui_updated).unwrap();
                let ms = elapsed.as_millis();
                if ms > 33 {
                    instructions.update2(&vm);
                    registers.update(&vm.cpu);
                    ui_updated = now;
                }
            }
            _ => {}
        }
    });
}
