pub type OSPtr = *mut std::ffi::c_void;

#[macro_export]
macro_rules! rect {
    ($x: expr, $y: expr, $width:expr, $height:expr) => {{
        Rect {
            x: $x,
            y: $y,
            width: $width,
            height: $height,
        }
    }};
}

macro_rules! hex2f {
    ($v:expr) => {
        &format!("{:02X}", $v)
    };
}

macro_rules! hex4f {
    ($v:expr) => {
        &format!("{:04X}", $v)
    };
}

macro_rules! bitf {
    ($v:expr) => {
        &format!("{}", $v as bool as u8)
    };
}

pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[derive(Clone, Copy)]
pub struct Color {
    pub r: f64,
    pub g: f64,
    pub b: f64,
    pub a: f64,
}

pub trait UIObject {
    fn from_ptr(ptr: OSPtr) -> Self;
    fn ptr(&self) -> OSPtr;
}

pub trait View: UIObject {}

pub trait Container: View {
    fn add_subview(&mut self, view: &impl View);
}

pub trait Menu {
    fn new() -> Self;
}

pub trait Label: UIObject {
    type F: Font;

    fn new() -> Self;
    fn from_container(container: &mut impl Container) -> Self;
    fn set_rect(&mut self, rect: Rect);
    fn hide(&mut self);
    fn show(&mut self);
    fn highlight(&mut self, value: bool);
    fn set_text(&mut self, text: &str);
}

pub trait Font {
    fn from_file(file: &str) -> Self;
}
