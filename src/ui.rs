use crate::base::{Container, Label, Menu, Rect};
use crate::cpu::{decode_bytes, disassemble};
use crate::cpu::{Instruction, CPU};
use crate::graphics::Renderer;
pub use crate::platform::activate;
use crate::platform::{OSLabel, OSMenu, OSView};
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use winit::{
    dpi::{LogicalPosition, LogicalSize},
    event_loop::EventLoop,
    window::{Window, WindowBuilder, WindowId},
};

use gfx_hal::{prelude::*, window::Extent2D};

macro_rules! label {
    ($x:expr, $y:expr, $width:expr, $height:expr) => {{
        let rect = rect! {$x, $y, $width, $height};
        let mut label = OSLabel::new();
        label.set_rect(rect);
        label
    }};
    ($text:expr; $($arg:tt)*) => {{
        let mut label = label! {$($arg)*};
        label.set_text($text);
        label
    }};
}

const MAX_LINES: usize = 50;
const INSTRUCTION_PAD: usize = 4;
const CRATE_NAME: &'static str = env!("CARGO_PKG_NAME");
const CRATE_VERSION: &'static str = env!("CARGO_PKG_VERSION");
const LINE_HEIGHT: f64 = 20.;
const RECT_GRAPHICS: Rect = Rect {
    x: 200.,
    y: 100.,
    width: 640.,
    height: 480.,
};
const RECT_INSTRUCTIONS: Rect = Rect {
    x: RECT_GRAPHICS.x + RECT_GRAPHICS.width + 20.,
    y: RECT_GRAPHICS.y,
    width: 200.,
    height: RECT_GRAPHICS.height,
};
const RECT_REGISTERS: Rect = Rect {
    x: RECT_INSTRUCTIONS.x,
    y: RECT_INSTRUCTIONS.y + RECT_INSTRUCTIONS.height + 40.,
    width: 200.,
    height: 200.,
};

fn build_window<T>(title: &str, event_loop: &EventLoop<T>, rect: Rect) -> Window {
    let builder = WindowBuilder::new()
        .with_inner_size(LogicalSize::new(rect.width, rect.height))
        .with_title(title);
    let window = builder.build(&event_loop).unwrap();
    window.set_outer_position(LogicalPosition::new(rect.x, rect.y));
    window.request_redraw();
    window
}

pub struct Graphics {
    renderer: Renderer<back::Backend>,
}

impl Graphics {
    pub fn new<T>(event_loop: &EventLoop<T>) -> Self {
        let instance = back::Instance::create(&"nes", 1).unwrap();
        let name = format!("{} v{}", CRATE_NAME, CRATE_VERSION);
        let window = build_window(&name, &event_loop, RECT_GRAPHICS);
        let renderer = Renderer::<back::Backend>::new(Some(instance), window);
        Self { renderer: renderer }
    }

    pub fn id(&self) -> WindowId {
        self.renderer.window.id()
    }

    pub fn update(&mut self) {
        self.renderer.render();
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        self.renderer.dimensions = Extent2D {
            width: width,
            height: height,
        };
        self.renderer.recreate_swapchain();
        self.renderer.render();
    }
}

#[allow(dead_code)]
pub struct InternalsWindow {
    window: Window,
    view: OSView,
    statics: HashMap<&'static str, OSLabel>,
    values: HashMap<&'static str, OSLabel>,
}

impl InternalsWindow {
    const KEY_A: &'static str = &"A";
    const KEY_S: &'static str = &"S";
    const KEY_PC: &'static str = &"PC";
    const KEY_X: &'static str = &"X";
    const KEY_Y: &'static str = &"Y";
    const KEY_T: &'static str = &"T";
    const KEY_C: &'static str = &"C";
    const KEY_Z: &'static str = &"Z";
    const KEY_I: &'static str = &"I";
    const KEY_D: &'static str = &"D";
    const KEY_V: &'static str = &"V";
    const KEY_N: &'static str = &"N";

    pub fn new<T>(event_loop: &EventLoop<T>) -> Self {
        let window = build_window(&"internals", event_loop, RECT_REGISTERS);
        let top = window.inner_size().height as f64 / window.scale_factor() as f64;
        let height = LINE_HEIGHT;
        let width = 60.;

        let cols: [f64; 4] = [0., 35., 90., 115.];
        let rows: Vec<f64> = (0..=5).map(|i| top - height - i as f64 * height).collect();

        let values = hashmap! {
            Self::KEY_X => label! {cols[1], rows[0], width, height},
            Self::KEY_Y => label! {cols[1], rows[1], width, height},
            Self::KEY_A => label! {cols[1], rows[2], width, height},
            Self::KEY_PC => label! {cols[1], rows[3], width, height},
            Self::KEY_S => label! {cols[1], rows[4], width, height},
            Self::KEY_T => label! {cols[1], rows[5], width, height},
            Self::KEY_C => label! {cols[3], rows[0], width, height},
            Self::KEY_Z => label! {cols[3], rows[1], width, height},
            Self::KEY_I => label! {cols[3], rows[2], width, height},
            Self::KEY_D => label! {cols[3], rows[3], width, height},
            Self::KEY_V => label! {cols[3], rows[4], width, height},
            Self::KEY_N => label! {cols[3], rows[5], width, height},
        };

        let statics = hashmap! {
            Self::KEY_X => label! {" X:"; cols[0], rows[0], width, height},
            Self::KEY_Y => label! {" Y:"; cols[0], rows[1], width, height},
            Self::KEY_A => label! {" A:"; cols[0], rows[2], width, height},
            Self::KEY_PC => label! {"PC:"; cols[0], rows[3], width, height},
            Self::KEY_S => label! {" P:"; cols[0], rows[4], width, height},
            Self::KEY_T => label! {" T:"; cols[0], rows[5], width, height},
            Self::KEY_C => label! {"C:"; cols[2], rows[0], width, height},
            Self::KEY_Z => label! {"Z:"; cols[2], rows[1], width, height},
            Self::KEY_I => label! {"I:"; cols[2], rows[2], width, height},
            Self::KEY_D => label! {"D:"; cols[2], rows[3], width, height},
            Self::KEY_V => label! {"V:"; cols[2], rows[4], width, height},
            Self::KEY_N => label! {"N:"; cols[2], rows[5], width, height},
        };

        let mut view = OSView::from_window(&window);
        for key in statics.keys() {
            view.add_subview(&statics[key]);
            view.add_subview(&values[key]);
        }

        Self {
            window,
            view,
            statics,
            values,
        }
    }

    pub fn update_value(&mut self, key: &str, value: &str) {
        let label = &mut self.values.get_mut(key).unwrap();
        label.set_text(value);
    }

    pub fn update(&mut self, cpu: &CPU) {
        self.update_value(Self::KEY_X, &hex2f!(cpu.x));
        self.update_value(Self::KEY_Y, &hex2f!(cpu.y));
        self.update_value(Self::KEY_A, &hex2f!(cpu.a));
        self.update_value(Self::KEY_S, &hex2f!(cpu.s));
        self.update_value(Self::KEY_PC, &hex4f!(cpu.pc));
        self.update_value(Self::KEY_T, &format!("{}", cpu.t));
        self.update_value(Self::KEY_C, &bitf!(cpu.flags.c));
        self.update_value(Self::KEY_Z, &bitf!(cpu.flags.z));
        self.update_value(Self::KEY_I, &bitf!(cpu.flags.i));
        self.update_value(Self::KEY_D, &bitf!(cpu.flags.d));
        self.update_value(Self::KEY_V, &bitf!(cpu.flags.v));
        self.update_value(Self::KEY_N, &bitf!(cpu.flags.n));

        self.window.request_redraw();
    }
}

#[derive(Debug)]
struct InstructionSlice {
    addr_start: u16,
    addr_end: u16,
    hl_line: usize,
    hl_addr: u16,
    instructions: Vec<(u16, Instruction)>,
    indices: HashMap<u16, usize>,
}

impl Default for InstructionSlice {
    fn default() -> Self {
        Self {
            addr_start: 0,
            addr_end: 0,
            hl_line: 0,
            hl_addr: 0,
            instructions: vec![],
            indices: HashMap::new(),
        }
    }
}

impl fmt::Display for InstructionSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!(
            "{{start: {:#04X}, highlight: {:#04X}({}), end: {:#04X}({})}}",
            self.addr_start,
            self.hl_addr,
            self.hl_line,
            self.addr_end,
            self.instructions.len() - 1,
        ))
    }
}

pub struct Instructions {
    window: Window,
    decoded: BTreeMap<u16, Instruction>,
    displayed: InstructionSlice,
    lines: Vec<OSLabel>,
}

impl Instructions {
    // TODO variable # of allocated labels
    // TODO incremental decoding

    pub fn new<T>(event_loop: &EventLoop<T>) -> Self {
        let window = build_window(&"instructions", event_loop, RECT_INSTRUCTIONS);
        let mut view = OSView::from_window(&window);
        let lines = (0..MAX_LINES)
            .map(|_| OSLabel::from_container(&mut view))
            .collect();

        Self {
            window,
            lines,
            displayed: InstructionSlice::default(),
            decoded: BTreeMap::new(),
        }
    }

    fn get_extent(&self) -> (f64, f64) {
        let scale = self.window.scale_factor();
        let size = self.window.inner_size();
        let height = size.height as f64 / scale;
        let width = size.width as f64 / scale;
        (width, height)
    }

    pub fn preload(&mut self, _cpu: &CPU, memory: &[u8]) {
        let n_lines = (self.get_extent().1 / LINE_HEIGHT) as usize;
        self.decoded = Self::map_instructions(memory, 0);
        self.update_slice(n_lines, 0);
    }

    fn map_instructions(bytes: &[u8], start: usize) -> BTreeMap<u16, Instruction> {
        let mut map = BTreeMap::new();
        let mut address = start;

        while address < bytes.len() {
            match decode_bytes(bytes, address) {
                Ok(instruction) => {
                    let length = instruction.bytes as usize;
                    map.insert(address as u16, instruction);
                    address += length;
                }
                Err(_) => address += 1,
            }
        }

        debug!(
            "decoded {} instructions ({:#04X}-{:#04X}) from {} bytes",
            map.len(),
            start,
            address,
            bytes.len()
        );
        map
    }

    fn update_slice(&mut self, n: usize, addr: u16) {
        let current = &mut self.displayed;

        if let Some(&i) = current.indices.get(&addr) {
            if i >= INSTRUCTION_PAD && i < n - INSTRUCTION_PAD {
                current.hl_line = i;
                current.hl_addr = addr;
                return;
            }
        };

        let position = if addr < current.addr_start {
            n - INSTRUCTION_PAD
        } else {
            INSTRUCTION_PAD
        };

        let mut rev = self.decoded.range(0..addr);
        let mut instructions = vec![];

        while instructions.len() < position {
            match rev.next_back() {
                Some((a, i)) => instructions.push((*a, *i)),
                _ => break,
            }
        }

        instructions.reverse();
        let hl_line = instructions.len();
        let mut fwd = self.decoded.range(addr..addr + 100u16);

        while instructions.len() < n {
            match fwd.next() {
                Some(i) => instructions.push((*i.0, *i.1)),
                None => break,
            }
        }

        let indices: HashMap<_, _> = instructions
            .iter()
            .enumerate()
            .map(|(i, (a, _))| (*a, i))
            .collect();

        self.displayed = InstructionSlice {
            addr_start: instructions[0].0,
            addr_end: instructions[instructions.len() - 1].0,
            hl_line: hl_line,
            hl_addr: addr,
            instructions: instructions,
            indices: indices,
        };

        debug!("sliced instructions @ {:#04X}: {}", addr, self.displayed);
    }

    pub fn update(&mut self, cpu: &CPU) {
        let scale = self.window.scale_factor();
        let size = self.window.inner_size();
        let height = size.height as f64 / scale;
        let width = size.width as f64 / scale;
        let n_lines = (height / LINE_HEIGHT) as usize;
        let addr = cpu.pc;

        for l in &mut self.lines {
            l.highlight(false);
            l.hide();
            l.set_text(&"");
        }

        if cpu.t == 0 && addr != self.displayed.hl_addr {
            self.update_slice(n_lines, addr);
        }
        let slice = &self.displayed;

        for (addr, inst) in &slice.instructions {
            let i = slice.indices[&addr];
            let text_view = &mut self.lines[i];
            let rect = Rect {
                x: 0.,
                y: height - (i as f64) * LINE_HEIGHT - LINE_HEIGHT,
                width: width,
                height: LINE_HEIGHT,
            };

            if i == slice.hl_line {
                text_view.highlight(true);
            }

            let text1 = disassemble(&inst);
            let text2 = match inst.bytes {
                3 => format!("{:02X} {:02X}{:02X}", inst.opcode, inst.arg1, inst.arg2),
                2 => format!("{:02X} {:02X}  ", inst.opcode, inst.arg1),
                _ => format!("{:02X}     ", inst.opcode),
            };
            let text3 = format!("{:04X} {} {}", addr, text2, text1);

            text_view.show();
            text_view.set_rect(rect);
            text_view.set_text(&text3);
        }
    }
}

pub struct MainMenu {
    _menu: OSMenu,
}

impl MainMenu {
    pub fn new() -> Self {
        Self {
            _menu: OSMenu::new(),
        }
    }
}
