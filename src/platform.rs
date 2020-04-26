use crate::base::{Color, Font, Label, Rect};
use crate::Id;
use cocoa::{
    appkit::NSColor,
    base::{nil, BOOL, NO, YES},
    foundation::{NSPoint, NSRange, NSRect, NSSize, NSString},
};
use core_graphics::{data_provider::CGDataProvider, font::CGFont};
use std::{fs, fs::File, io::Read, sync::Arc};

use core_foundation::base::ToVoid;
use core_text::font::CTFont;

extern "C" {
    pub static NSFontAttributeName: Id;
    pub static NSForegroundColorAttributeName: Id;
    pub static NSZeroRect: NSRect;
}

macro_rules! msg_sendf {
    ($obj:expr, $name:ident) => ({
        let _: Id = msg_send!($obj, $name);
    });
    ($obj:expr, $($name:ident : $arg:expr)+) => ({
        let _: Id = msg_send!($obj, $($name : $arg)+);
    });
}

pub struct NSFont {
    font: CTFont,
}

impl Font for NSFont {
    fn from_file(_file: &str) -> NSFont {
        let filename = "./data/SourceCodePro-Regular.ttf";
        let font = {
            let mut f = File::open(&filename).expect("no file found");
            let metadata = fs::metadata(&filename).expect("unable to read metadata");
            let mut bytes = vec![0; metadata.len() as usize];
            f.read(&mut bytes).expect("buffer overflow");
            let provider = CGDataProvider::from_buffer(Arc::new(bytes.clone()));
            let cgfont = CGFont::from_data_provider(provider).unwrap();
            let font: CTFont = core_text::font::new_from_CGFont(&cgfont, 13.);
            font
        };
        NSFont { font }
    }
}

impl NSFont {
    pub fn inner(&self) -> *const std::ffi::c_void {
        self.font.to_void()
    }
}

pub struct NSTextViewLabel {
    view: NSTextView,
    color: Color,
    font: NSFont,
}

impl Label for NSTextViewLabel {
    type F = NSFont;

    fn new() -> Self {
        NSTextViewLabel {
            view: unsafe { NSTextView::init_with_frame(NSZeroRect) },
            color: Color {
                r: 1.,
                g: 1.,
                b: 1.,
                a: 1.,
            },
            font: NSFont::from_file("./data/SourceCodePro-Regular.ttf"),
        }
    }

    fn inner(&self) -> *mut std::ffi::c_void {
        unsafe { self.view.id() as *mut std::ffi::c_void }
    }

    fn set_position(&mut self, rect: Rect) {
        let nsrect = NSRect::new(
            NSPoint::new(rect.x, rect.y),
            NSSize::new(rect.width, rect.height),
        );
        unsafe {
            self.view.set_frame(nsrect);
        }
    }

    fn hide(&mut self) {
        unsafe {
            self.view.set_hidden(YES);
        }
    }

    fn show(&mut self) {
        unsafe {
            self.view.set_hidden(NO);
        }
    }

    fn set_color(&mut self, color: Color) {
        self.color = color;
    }

    fn set_text(&mut self, text: &str) {
        let range = NSRange::new(0, text.len() as u64);
        let font = self.font.inner();
        let color = unsafe {
            NSColor::colorWithRed_green_blue_alpha_(
                nil,
                self.color.r,
                self.color.g,
                self.color.b,
                self.color.a,
            )
        };
        unsafe {
            let mut string = NSMutableAttributedString::init_with_string(text);
            string.add_attribute(NSFontAttributeName, font, range);
            string.add_attribute(NSForegroundColorAttributeName, color, range);
        }
    }

    fn set_font(&mut self, font: Self::F) {
        self.font = font;
    }
}

pub struct NSMutableAttributedString {
    ptr: Id,
}

impl NSMutableAttributedString {
    pub unsafe fn init_with_string(string: impl Into<String>) -> Self {
        let ptr = {
            let s1: Id = NSString::alloc(nil).init_str(&string.into());
            let s2: Id = msg_send![class!(NSMutableAttributedString), alloc];
            msg_send![s2, initWithString: s1]
        };
        NSMutableAttributedString { ptr }
    }

    pub unsafe fn id(&self) -> Id {
        self.ptr
    }

    pub unsafe fn add_attribute<T>(&mut self, name: Id, value: T, range: NSRange) {
        msg_sendf![self.ptr, addAttribute:name value:value range:range];
    }
}

pub struct NSTextView {
    ptr: Id,
}

impl NSTextView {
    pub unsafe fn init_with_frame(rect: NSRect) -> Self {
        let ptr = {
            let alloc: Id = msg_send![class!(NSTextView), alloc];
            let view: Id = msg_send![alloc, initWithFrame: rect];
            msg_sendf![view, setBackgroundColor: NSColor::clearColor(nil)];
            view
        };
        NSTextView { ptr }
    }

    pub unsafe fn id(&self) -> Id {
        self.ptr
    }

    pub unsafe fn set_frame(&mut self, rect: NSRect) {
        msg_sendf![self.ptr, setFrame: rect];
    }

    pub unsafe fn set_hidden(&mut self, hidden: BOOL) {
        msg_sendf![self.ptr, setHidden: hidden];
    }

    pub unsafe fn set_attributed_string(&mut self, string: &NSMutableAttributedString) {
        let storage: Id = msg_send![self.id(), textStorage];
        msg_sendf![storage, setAttributedString: string.id()];
    }
}
