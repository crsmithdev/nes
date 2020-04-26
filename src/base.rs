pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

pub struct Color {
    pub r: f64,
    pub g: f64,
    pub b: f64,
    pub a: f64,
}

pub trait Label {
    type F: Font;

    fn new() -> Self;
    fn set_position(&mut self, rect: Rect);
    fn hide(&mut self);
    fn show(&mut self);
    fn set_color(&mut self, color: Color);
    fn set_text(&mut self, text: &str);
    fn set_font(&mut self, font: Self::F);
    fn inner(&self) -> *mut std::ffi::c_void;
}

pub trait Font {
    fn from_file(file: &str) -> Self;
}
