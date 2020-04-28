extern crate gfx_backend_metal as back;
#[macro_use]
extern crate objc;
extern crate log;
#[macro_use]
extern crate lazy_static;

mod base;
mod cpu;
mod graphics;
mod logger;
mod platform;
mod rom;
mod ui;

use crate::base::Rect;
use crate::graphics::Renderer;
use chrono::Duration;
use gfx_hal::{prelude::*, window::Extent2D};
use winit::{
    dpi::{LogicalPosition, LogicalSize},
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

const RECT_GRAPHICS: Rect = Rect {
    x: 200.,
    y: 100.,
    width: 1024.,
    height: 768.,
};
const RECT_INSTRUCTIONS: Rect = Rect {
    x: RECT_GRAPHICS.x + RECT_GRAPHICS.width + 20.,
    y: RECT_GRAPHICS.y,
    width: 200.,
    height: 527.,
};
const RECT_REGISTERS: Rect = Rect {
    x: RECT_INSTRUCTIONS.x,
    y: RECT_INSTRUCTIONS.y + RECT_INSTRUCTIONS.height + 40.,
    width: 200.,
    height: 200.,
};
const CRATE_NAME: &'static str = env!("CARGO_PKG_NAME");
const CRATE_VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn build_window(title: &str, event_loop: &EventLoop<()>, rect: Rect) -> Window {
    let builder = WindowBuilder::new()
        .with_inner_size(LogicalSize::new(rect.width, rect.height))
        .with_title(title);
    let window = builder.build(&event_loop).unwrap();
    window.set_outer_position(LogicalPosition::new(rect.x, rect.y));
    window.request_redraw();
    window
}

fn main() {
    let _ = &logger::init();
    let name = format!("{} v{}", CRATE_NAME, CRATE_VERSION);

    let event_loop = EventLoop::new();
    let main_window = build_window(&name, &event_loop, RECT_GRAPHICS);
    let instructions_window = build_window(&"instructions", &event_loop, RECT_INSTRUCTIONS);
    let registers_window = build_window(&"registers", &event_loop, RECT_REGISTERS);
    let main_window_id = main_window.id();
    let size = main_window.inner_size();

    let instance = back::Instance::create(&name, 1).unwrap();
    let mut renderer = Renderer::<back::Backend>::new(Some(instance), main_window);
    let mut instruction_view = ui::InstructionView::new(instructions_window);
    let mut registers_view = ui::RegisterFlagView::new(registers_window);
    let _menu = ui::MainMenu::new();

    let bytes = include_bytes!("../data/nestest.nes");
    let mut memory = rom::ROM::empty();
    memory.load_program(bytes);

    let mut m6502 = cpu::CPU::default();
    let mut pins = cpu::Pins::default();
    let proxy = event_loop.create_proxy();

    let timer = timer::Timer::new();
    let _guard = timer.schedule_repeating(Duration::seconds(1), move || {
        proxy.send_event(()).unwrap();
    });

    ui::activate();
    renderer.dimensions = Extent2D {
        width: size.width,
        height: size.height,
    };
    instruction_view.update(&memory.bytes, 0x8000);
    renderer.recreate_swapchain();
    renderer.render();

    event_loop.run(move |event, _, control| {
        *control = ControlFlow::Wait;

        match event {
            Event::WindowEvent { event, window_id } => match event {
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::Resized(dims) => {
                    if window_id == main_window_id {
                        renderer.dimensions = Extent2D {
                            width: dims.width,
                            height: dims.height,
                        };
                        renderer.recreate_swapchain();
                    }
                }
                _ => {}
            },
            Event::RedrawEventsCleared => (), // TODO
            Event::UserEvent(_) => {
                m6502.cycle(&mut pins);
                renderer.render();
                instruction_view.update(&memory.bytes, 0x8000);
                registers_view.update(&m6502);
            }
            _ => {}
        }
    });
}
