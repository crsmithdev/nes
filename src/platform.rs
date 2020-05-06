use crate::base::{Color, Container, Font, Label, Menu, OSPtr, Rect, UIObject, View};
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
// 42 46 63 165 170 205
// 0.132, 0.144, .148

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
    text_color: Color,
    background_color: Color,
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

    fn new() -> Self {
        let ptr = unsafe {
            let alloc: Id = msg_send![class!(NSTextView), alloc];
            let view: Id = msg_send![alloc, initWithFrame: NSZeroRect];
            msg_sendf![view, setBackgroundColor: NSColor::clearColor(nil)];
            view
        };
        let mut label = OSLabel {
            ptr: ptr,
            text: " ".to_owned(),
            text_color: *DEFAULT_TEXT_COLOR,
            background_color: *DEFAULT_BACKGROUND_COLOR,
            font: &*DEFAULT_FONT,
        };

        label.update_string();
        label
    }

    fn from_container(container: &mut impl Container) -> Self {
        let label = OSLabel::new();
        container.add_subview(&label);
        label
    }

    // fn inner(&self) -> OSPtr {
    //     self.ptr as OSPtr
    // }

    fn set_rect(&mut self, rect: Rect) {
        let ns_rect = NSRect::new(
            NSPoint::new(rect.x, rect.y),
            NSSize::new(rect.width, rect.height),
        );
        unsafe {
            msg_sendf![self.ptr, setFrame: ns_rect];
        }
    }

    fn hide(&mut self) {
        unsafe {
            msg_sendf![self.ptr, setHidden: YES];
        }
    }

    fn show(&mut self) {
        unsafe {
            msg_sendf![self.ptr, setHidden: NO];
        }
    }

    fn highlight(&mut self, value: bool) {
        if value {
            self.text_color = *DEFAULT_BACKGROUND_COLOR;
            self.background_color = *DEFAULT_TEXT_COLOR;
        } else {
            self.text_color = *DEFAULT_TEXT_COLOR;
            self.background_color = *DEFAULT_BACKGROUND_COLOR;
        }
        self.update_string();
    }

    fn set_text(&mut self, text: &str) {
        self.text = text.to_owned();
        self.update_string();
    }
}

impl OSLabel {
    fn update_string(&mut self) {
        let range = NSRange::new(0, self.text.len() as u64);
        unsafe {
            let fg_color = NSColor::colorWithSRGBRed_green_blue_alpha_(
                nil,
                self.text_color.r,
                self.text_color.g,
                self.text_color.b,
                self.text_color.a,
            );
            let bg_color = NSColor::colorWithSRGBRed_green_blue_alpha_(
                nil,
                self.background_color.r,
                self.background_color.g,
                self.background_color.b,
                self.background_color.a,
            );
            let string: Id = {
                let s1: Id = NSString::alloc(nil).init_str(&self.text);
                let s2: Id = msg_send![class!(NSMutableAttributedString), alloc];
                msg_send![s2, initWithString: s1]
            };
            msg_sendf![string, addAttribute:NSFontAttributeName value:self.font.inner() range:range];
            msg_sendf![string, addAttribute:NSForegroundColorAttributeName value:fg_color range:range];
            let storage: Id = msg_send![self.ptr, textStorage];
            msg_sendf![storage, setAttributedString: string];
            msg_sendf![self.ptr, setBackgroundColor: bg_color];
        }
    }
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
