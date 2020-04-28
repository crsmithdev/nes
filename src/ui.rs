use crate::base::{Container, Label, Menu, Rect};
use crate::cpu::disassemble;
use crate::cpu::CPU;
pub use crate::platform::activate;
use crate::platform::{OSLabel, OSMenu, OSView};
use winit::window::Window;

const LINE_HEIGHT: f64 = 20.;

pub struct RegisterFlagView {
    _window: Window,
    inner: OSView,
    x: OSLabel,
    y: OSLabel,
    acc: OSLabel,
    pc: OSLabel,
    sp: OSLabel,
    c: OSLabel,
    z: OSLabel,
    i: OSLabel,
    d: OSLabel,
    b: OSLabel,
    v: OSLabel,
    s: OSLabel,
}

impl RegisterFlagView {
    pub fn new(window: Window) -> Self {
        let inner = OSView::from_window(&window);
        let size = window.inner_size();
        let scale = window.scale_factor() as f64;
        let height = size.height as f64 / scale;
        let width = size.width as f64 / scale;

        let mut view = Self {
            inner,
            x: OSLabel::new(),
            y: OSLabel::new(),
            acc: OSLabel::new(),
            pc: OSLabel::new(),
            sp: OSLabel::new(),
            c: OSLabel::new(),
            z: OSLabel::new(),
            i: OSLabel::new(),
            d: OSLabel::new(),
            b: OSLabel::new(),
            v: OSLabel::new(),
            s: OSLabel::new(),
            _window: window,
        };
        view.x = view.create_label("", Rect::new(35., height - 30., width, LINE_HEIGHT));
        view.y = view.create_label("", Rect::new(85., height - 30., width, LINE_HEIGHT));
        view.acc = view.create_label("", Rect::new(145., height - 30., width, LINE_HEIGHT));
        view.create_label(" X:", Rect::new(10., height - 30., width, LINE_HEIGHT));
        view.create_label(" Y:", Rect::new(60., height - 30., width, LINE_HEIGHT));
        view.create_label("ACC:", Rect::new(110., height - 30., width, LINE_HEIGHT));
        view.create_label("PC:", Rect::new(10., height - 60., width, LINE_HEIGHT));
        view.pc = view.create_label("", Rect::new(35., height - 60., width, LINE_HEIGHT));
        view.create_label("SP:", Rect::new(60., height - 60., width, LINE_HEIGHT));
        view.sp = view.create_label("", Rect::new(85., height - 60., width, LINE_HEIGHT));
        view.create_label("C:", Rect::new(10., height - 90., width, LINE_HEIGHT));
        view.c = view.create_label("", Rect::new(25., height - 90., width, LINE_HEIGHT));
        view.create_label("Z:", Rect::new(50., height - 90., width, LINE_HEIGHT));
        view.z = view.create_label("", Rect::new(65., height - 90., width, LINE_HEIGHT));
        view.create_label("I:", Rect::new(90., height - 90., width, LINE_HEIGHT));
        view.i = view.create_label("", Rect::new(105., height - 90., width, LINE_HEIGHT));
        view.create_label("D:", Rect::new(130., height - 90., width, LINE_HEIGHT));
        view.d = view.create_label("", Rect::new(145., height - 90., width, LINE_HEIGHT));
        view.create_label("B:", Rect::new(10., height - 120., width, LINE_HEIGHT));
        view.b = view.create_label("", Rect::new(25., height - 120., width, LINE_HEIGHT));
        view.create_label("V:", Rect::new(50., height - 120., width, LINE_HEIGHT));
        view.v = view.create_label("", Rect::new(65., height - 120., width, LINE_HEIGHT));
        view.create_label("S:", Rect::new(90., height - 120., width, LINE_HEIGHT));
        view.s = view.create_label("", Rect::new(105., height - 120., width, LINE_HEIGHT));
        view
    }

    fn create_label(&mut self, text: &str, rect: Rect) -> OSLabel {
        let mut label = OSLabel::new();
        label.set_rect(rect);
        label.set_text(text);
        self.inner.add_subview(&label);
        label
    }

    pub fn update(&mut self, cpu: &CPU) {
        self.x.set_text(&format!("{:02X}", cpu.x));
        self.y.set_text(&format!("{:02X}", cpu.y));
        self.acc.set_text(&format!("{:02X}", cpu.acc));
        self.pc.set_text(&format!("{:02X}", cpu.pc));
        self.sp.set_text(&format!("{:02X}", cpu.sp));
        self.c.set_text(&format!("{}", 0));
        self.z.set_text(&format!("{}", 0));
        self.i.set_text(&format!("{}", 0));
        self.d.set_text(&format!("{}", 0));
        self.b.set_text(&format!("{}", 0));
        self.v.set_text(&format!("{}", 0));
        self.s.set_text(&format!("{}", 0));
    }
}

pub struct InstructionView {
    window: Window,
    lines: Vec<OSLabel>,
}

impl InstructionView {
    pub fn new(window: Window) -> Self {
        let mut inner = OSView::from_window(&window);
        let lines = (0..50)
            .map(|_| {
                let label = OSLabel::new();
                inner.add_subview(&label);
                label
            })
            .collect();
        Self { window, lines }
    }

    pub fn update(&mut self, bytes: &[u8], offset: usize) {
        let scale = self.window.scale_factor();
        let size = self.window.inner_size();
        let height = size.height as f64 / scale;
        let width = size.width as f64 / scale;
        let n_lines = (height / LINE_HEIGHT) as usize;
        let mut address = offset;

        for i in 0..self.lines.len() {
            let text_view = &mut self.lines[i];
            let rect = Rect {
                x: 0.,
                y: (height as f64) - (i as f64) * LINE_HEIGHT - LINE_HEIGHT,
                width: width as f64,
                height: LINE_HEIGHT,
            };

            if i < n_lines {
                let decoded = disassemble(bytes, address);
                let op_string = match decoded.length {
                    3 => format!(
                        "{:02X} {:02X}{:02X}",
                        decoded.opcode, decoded.arg_1, decoded.arg_2
                    ),
                    2 => format!("{:02X} {:02X}  ", decoded.opcode, decoded.arg_1),
                    _ => format!("{:02X}     ", decoded.opcode),
                };
                let text = format!("{:04X} {} {}", address, op_string, decoded.string);
                address += decoded.length as usize;
                text_view.show();
                text_view.set_rect(rect);
                text_view.set_text(&text);
            } else {
                text_view.set_text(&"");
                text_view.hide();
            }
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
