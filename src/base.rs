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

impl PartialEq for Rect {
    fn eq(&self, other: &Self) -> bool {
        (self.x - other.x).abs() <= 0.01
            && (self.y - other.y).abs() <= 0.01
            && (self.width - other.width).abs() <= 0.01
            && (self.height - other.height).abs() <= 0.01
    }
}

pub trait OSObject {
    fn ptr(&self) -> OSPtr;
}

#[derive(Clone, Copy)]
pub struct Color {
    pub r: f64,
    pub g: f64,
    pub b: f64,
    pub a: f64,
}

pub trait View: OSObject {}

pub trait Container: View {
    fn add_subview(&mut self, view: &impl View);
}

pub trait Menu {
    fn new() -> Self;
}

pub trait Label: OSObject {
    type F: Font;

    fn set_rect(&mut self, rect: Rect);
    fn set_visible(&mut self, value: bool);
    fn set_highlighted(&mut self, value: bool);
    fn set_text(&mut self, text: &str);
}

pub trait Font: OSObject {
    fn from_file(file: &str) -> Self;
}
