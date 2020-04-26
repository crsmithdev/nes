extern crate cocoa;
extern crate gfx_backend_metal as back;
#[macro_use]
extern crate objc;
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate nfd;

use gfx_hal::{prelude::*, window::Extent2D};

mod graphics;
mod logger;
mod rom;
mod ui;

use crate::graphics::Renderer;
use winit::{
    dpi::{PhysicalSize, Size},
    event::{Event, KeyboardInput, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

const DIMS_GRAPHICS: Extent2D = Extent2D {
    width: 1024,
    height: 768,
};
const DIMS_INSTRUCTIONS: Extent2D = Extent2D {
    width: 640,
    height: 768,
};
const CRATE_NAME: &'static str = env!("CARGO_PKG_NAME");
const CRATE_VERSION: &'static str = env!("CARGO_PKG_VERSION");

type Id = *mut objc::runtime::Object;

fn main() {
    let _ = &logger::init();
    let name = format!("{} v{}", CRATE_NAME, CRATE_VERSION);

    let event_loop = EventLoop::new();
    let builder = WindowBuilder::new()
        .with_inner_size(Size::Physical(PhysicalSize::new(
            DIMS_GRAPHICS.width,
            DIMS_GRAPHICS.height,
        )))
        .with_title(&name);
    let main_window = builder.build(&event_loop).unwrap();

    let instance = back::Instance::create(&name, 1).unwrap();
    let mut renderer = Renderer::<back::Backend>::new(Some(instance), main_window);
    let _menu = ui::Menu::new();

    let builder = WindowBuilder::new()
        .with_inner_size(Size::Physical(PhysicalSize::new(
            DIMS_INSTRUCTIONS.width,
            DIMS_INSTRUCTIONS.height,
        )))
        .with_title("instructions".to_string());
    let instructions_window = builder.build(&event_loop).unwrap();
    instructions_window.set_outer_position({
        let mut position = instructions_window.outer_position().unwrap();
        position.x += 900;
        position
    });
    let mut instruction_view = ui::InstructionView::new(instructions_window);
    let temp_rom = rom::temp_rom();
    instruction_view.update(&temp_rom.bytes[0..60]);

    ui::activate();
    renderer.render();
    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode: Some(VirtualKeyCode::Escape),
                            ..
                        },
                    ..
                } => *control_flow = ControlFlow::Exit,
                WindowEvent::Resized(dims) => {
                    renderer.dimensions = Extent2D {
                        width: dims.width,
                        height: dims.height,
                    };
                    renderer.recreate_swapchain();
                    instruction_view.update(&temp_rom.bytes[0..60]);
                }
                _ => {}
            },
            Event::RedrawEventsCleared => {
                renderer.render();
            }
            _ => {}
        }
    });
}
