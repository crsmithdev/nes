use crate::{
    base::{Color, Container, Font, Label, Menu, OSPtr, Rect, UIObject, View},
    ui::ResourceManager,
};
use cocoa::{
    appkit::{NSApplication, NSApplicationActivateIgnoringOtherApps, NSColor, NSMenu, NSMenuItem},
    base::{nil, selector, NO, YES},
    foundation::{
        NSAutoreleasePool, NSPoint, NSProcessInfo, NSRange, NSRect, NSSize, NSString, NSUInteger,
    },
};
use core_foundation::base::ToVoid;
use core_graphics::{data_provider::CGDataProvider, font::CGFont};
use core_text::font::CTFont;
use std::{fs, fs::File, io::Read, sync::Arc};
use winit::{platform::macos::WindowExtMacOS, window::Window};

pub type Id = *mut objc::runtime::Object;
lazy_static! {
    static ref DEFAULT_FONT: OSFont = OSFont::from_file("./data/SourceCodePro-Regular.ttf");
    static ref DEFAULT_BACKGROUND_COLOR: Color = Color {
        r: 42. / 255.,
        g: 46. / 255.,
        b: 63. / 255.,
        a: 1.
    };
    static ref DEFAULT_TEXT_COLOR: Color = Color {
        r: 165. / 255.,
        g: 170. / 255.,
        b: 205. / 255.,
        a: 1.
    };
}

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

pub struct OSView {
    ptr: Id,
}

impl View for OSView {}

impl OSView {
    pub fn from_window(window: &Window) -> OSView {
        unsafe {
            let ptr = window.ns_view() as Id;
            let color = *DEFAULT_BACKGROUND_COLOR;
            let bg_color = NSColor::colorWithSRGBRed_green_blue_alpha_(
                nil, color.r, color.g, color.b, color.a,
            );
            msg_sendf![ptr, setBackgroundColor: bg_color];
            Self { ptr }
        }
    }
}

impl UIObject for OSView {
    fn from_ptr(ptr: OSPtr) -> Self {
        unsafe {
            let id = ptr as Id;
            let color = *DEFAULT_BACKGROUND_COLOR;
            let bg_color = NSColor::colorWithSRGBRed_green_blue_alpha_(
                nil, color.r, color.g, color.b, color.a,
            );
            msg_sendf![id, setBackgroundColor: bg_color];
            Self { ptr: id }
        }
    }

    fn ptr(&self) -> OSPtr {
        self.ptr as OSPtr
    }
}

impl Container for OSView {
    fn add_subview(&mut self, view: &impl View) {
        unsafe { msg_sendf!(self.ptr, addSubview: view.ptr()) }
    }
}

pub struct OSColor {
    color: std::sync::Arc<std::sync::Mutex<Id>>,
}

unsafe impl Send for OSColor {}

impl UIObject for OSColor {
    fn from_ptr(_ptr: OSPtr) -> Self {
        unimplemented!()
    }

    fn ptr(&self) -> OSPtr {
        *self.color.lock().unwrap() as OSPtr
    }
}

impl OSColor {
    pub fn new(r: f64, g: f64, b: f64, a: f64) -> Self {
        unsafe {
            let color = NSColor::colorWithSRGBRed_green_blue_alpha_(nil, r, g, b, a);
            let color = std::sync::Arc::new(std::sync::Mutex::new(color));
            Self { color }
        }
    }

    pub fn from_color(color: Color) -> Self {
        Self::new(color.r, color.g, color.b, color.a)
    }
}

pub struct OSFont {
    font: CTFont,
}

impl Font for OSFont {
    fn from_file(filename: &str) -> Self {
        let font = {
            let mut file = File::open(&filename).unwrap();
            let metadata = fs::metadata(&filename).unwrap();
            let mut bytes = vec![0; metadata.len() as usize];
            file.read(&mut bytes).expect("buffer overflow");

            let provider = CGDataProvider::from_buffer(Arc::new(bytes.clone()));
            let cgfont = CGFont::from_data_provider(provider).unwrap();
            let font = core_text::font::new_from_CGFont(&cgfont, 13.);
            font
        };
        Self { font }
    }
}

impl OSFont {
    pub fn inner(&self) -> *const std::ffi::c_void {
        self.font.to_void()
    }
}

pub struct OSLabel {
    ptr: Id,
    text: String,
    fg_color: Id,
    bg_color: Id,
    storage: Id,
    visible: bool,
    rect: Rect,
    range: NSRange,
    highlighted: bool,
    font: &'static OSFont,
}

impl View for OSLabel {}

impl UIObject for OSLabel {
    fn from_ptr(_ptr: OSPtr) -> Self {
        unimplemented!()
    }

    fn ptr(&self) -> OSPtr {
        self.ptr as OSPtr
    }
}

impl Label for OSLabel {
    type F = OSFont;

    fn set_rect(&mut self, rect: Rect) {
        let changed = rect != self.rect;
        self.rect = rect;

        if changed {
            let ns_rect = NSRect::new(
                NSPoint::new(self.rect.x, self.rect.y),
                NSSize::new(self.rect.width, self.rect.height),
            );
            unsafe {
                msg_sendf![self.ptr, setFrame: ns_rect];
            }
        }
    }

    fn set_visible(&mut self, value: bool) {
        let changed = value != self.visible;
        self.visible = value;

        if changed {
            let hidden = match value {
                true => NO,
                false => YES,
            };
            unsafe { msg_sendf![self.ptr, setHidden: hidden] }
        }
    }

    fn set_highlighted(&mut self, value: bool) {
        let changed = value != self.highlighted;
        self.highlighted = value;

        if changed {
            let range = NSRange::new(0, self.text.len() as u64);
            let (fg_color, bg_color) = match self.highlighted {
                true => (self.bg_color, self.fg_color),
                false => (self.fg_color, self.bg_color),
            };

            unsafe {
                let pool = NSAutoreleasePool::new(nil);
                msg_sendf![self.storage, addAttribute:NSForegroundColorAttributeName
                value:fg_color range:range];
                msg_sendf![self.ptr, setBackgroundColor: bg_color];
                pool.drain();
            }
        }
    }

    fn set_text(&mut self, text: &str) {
        let changed = text != self.text;
        self.text = text.to_owned();

        if changed {
            let new_range = NSRange::new(0, self.text.len() as u64);
            let old_range = self.range;
            self.range = new_range;

            unsafe {
                let pool = NSAutoreleasePool::new(nil);
                let new_text = NSString::alloc(nil).init_str(&self.text).autorelease();
                let (fg_color, bg_color) = match self.highlighted {
                    true => (self.bg_color, self.fg_color),
                    false => (self.fg_color, self.bg_color),
                };

                msg_sendf![self.storage, replaceCharactersInRange:old_range
                    withString: new_text];
                msg_sendf![self.storage, addAttribute:NSFontAttributeName
                    value:self.font.inner() range:new_range];
                msg_sendf![self.storage, addAttribute:NSForegroundColorAttributeName
                    value:fg_color range:new_range];
                msg_sendf![self.ptr, setBackgroundColor: bg_color];

                pool.drain();
            }
        }
    }
}

impl OSLabel {
    pub fn new(manager: &'static ResourceManager) -> Self {
        let ptr: Id = unsafe {
            let ptr: Id = msg_send![class!(NSTextView), alloc];
            let ptr: Id = msg_send![ptr, initWithFrame: NSZeroRect];
            ptr
        };
        let storage: Id = unsafe { msg_send![ptr, textStorage] };
        let mut label = OSLabel {
            ptr: ptr,
            text: "".to_owned(),
            highlighted: false,
            rect: rect!(0., 0., 0., 0.),
            storage: storage,
            range: NSRange::new(0, 0),
            visible: true,
            fg_color: manager.fg_color().ptr() as Id,
            bg_color: manager.bg_color().ptr() as Id,
            font: &*DEFAULT_FONT,
        };
        // unsafe { msg_sendf![ptr, setHidden: NO] };
        label.set_text(" ");
        label
    }

    // fn update_string(&mut self) {
    //     let new_range = NSRange::new(0, self.text.len() as u64);
    //     unsafe {
    //         let pool = NSAutoreleasePool::new(nil);
    //         let (fg_color, bg_color) = match self.highlighted {
    //             true => (self.bg_color, self.fg_color),
    //             false => (self.fg_color, self.bg_color),
    //         };
    //         let storage = self.storage;
    //         let new_text = NSString::alloc(nil).init_str(&self.text).autorelease();
    //         let old_len: u64 = msg_send![storage, length];
    //         let old_range = NSRange::new(0, old_len);
    //         msg_sendf![storage, replaceCharactersInRange:old_range withString: new_text];
    //         msg_sendf![storage, addAttribute:NSFontAttributeName value:self.font.inner() range:new_range];
    //         msg_sendf![storage, addAttribute:NSForegroundColorAttributeName value:fg_color range:new_range];
    //         msg_sendf![self.ptr, setBackgroundColor: bg_color];
    //         pool.drain();
    //     }
    //     self.changed = false;
    // }
}

pub struct OSMenu {}

impl Menu for OSMenu {
    fn new() -> Self {
        unsafe {
            let _pool = NSAutoreleasePool::new(nil);
            let app = cocoa::appkit::NSApp();
            let menubar = cocoa::appkit::NSMenu::new(nil).autorelease();
            let app_menu_item = NSMenuItem::new(nil).autorelease();
            let app_menu = NSMenu::new(nil).autorelease();
            let quit_prefix = NSString::alloc(nil).init_str("Quit ");
            let quit_title =
                quit_prefix.stringByAppendingString_(NSProcessInfo::processInfo(nil).processName());
            let quit_action = selector("terminate:");
            let quit_key = NSString::alloc(nil).init_str("q");
            let quit_item = NSMenuItem::alloc(nil)
                .initWithTitle_action_keyEquivalent_(quit_title, quit_action, quit_key)
                .autorelease();
            menubar.addItem_(app_menu_item);
            app_menu.addItem_(quit_item);
            app_menu_item.setSubmenu_(app_menu);
            app.setMainMenu_(menubar);
        }
        Self {}
    }
}

pub fn activate() {
    unsafe {
        let app: Id = msg_send![class!(NSRunningApplication), currentApplication];
        let dock_bundle_id = NSString::alloc(nil).init_str("com.apple.dock");
        let dock_array: Id = msg_send![
            class!(NSRunningApplication),
            runningApplicationsWithBundleIdentifier: dock_bundle_id
        ];
        let dock_array_len: NSUInteger = msg_send![dock_array, count];
        if dock_array_len == 0 {
            // TODO
        } else {
            let dock: Id = msg_send![dock_array, objectAtIndex: 0];
            msg_sendf![
                dock,
                activateWithOptions: NSApplicationActivateIgnoringOtherApps
            ];
            // TODO fail
        }
        msg_sendf![
            app,
            activateWithOptions: NSApplicationActivateIgnoringOtherApps
        ];
    }
}
