pub type OSPtr = *mut std::ffi::c_void;

pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

impl Rect {
    pub fn new(x: f64, y: f64, width: f64, height: f64) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
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

pub trait Label {
    type F: Font;

    fn new() -> Self;
    fn set_rect(&mut self, rect: Rect);
    fn hide(&mut self);
    fn show(&mut self);
    fn set_color(&mut self, color: Color);
    fn set_text(&mut self, text: &str);
    fn inner(&self) -> OSPtr;
}

pub trait Font {
    fn from_file(file: &str) -> Self;
}
