use crate::Id;
use cocoa::{
    appkit::{NSApplication, NSApplicationActivateIgnoringOtherApps, NSColor, NSMenu, NSMenuItem},
    base::{nil, selector, BOOL, NO, YES},
    foundation::{
        NSAutoreleasePool, NSPoint, NSProcessInfo, NSRange, NSRect, NSSize, NSString, NSUInteger,
    },
};
use core_foundation::base::ToVoid;
use core_graphics::{color::CGColor, data_provider::CGDataProvider, font::CGFont};
use core_text::font::CTFont;
use std::{fs, fs::File, io::Read, sync::Arc};
use winit::{platform::macos::WindowExtMacOS, window::Window};

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

const LINE_HEIGHT: f64 = 20.;

pub struct Rect {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

pub struct Color {
    r: f64,
    g: f64,
    b: f64,
    a: f64,
}

pub trait Label {
    fn new() -> Self;
    fn set_position(&self, rect: Rect);
    fn hide(&self);
    fn show(&self);
    fn set_color(&self, color: Color);
    fn set_text(&self, string: impl Into<String>);
}

pub struct NSTextView {
    ptr: Id,
}

impl NSTextView {
    pub fn new() -> Self {
        let ptr = unsafe {
            let alloc: Id = msg_send![class!(NSTextView), alloc];
            let init: Id = msg_send![alloc, initWithFrame: NSZeroRect];
            init
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

    pub unsafe fn set_background_color(&mut self, color: impl NSColor) {
        msg_sendf![self.ptr, setBackgroundColor: color];
    }
}

pub struct InstructionView {
    window: Window,
    lines: Vec<Id>,
    font: CTFont,
}

impl InstructionView {
    pub fn new(window: Window) -> Self {
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

        println!("{:?}", window.scale_factor());

        let mut view = InstructionView {
            window,
            lines: vec![],
            font: font,
        };
        view.alloc_lines(50); // TODO
        view
    }

    fn alloc_lines(&mut self, n_lines: i32) {
        let view = self.window.ns_view() as Id;

        let lines = (0..n_lines)
            .map(|_| unsafe {
                let alloc: Id = msg_send![class!(NSTextView), alloc];
                let text_view: Id = msg_send![alloc, initWithFrame: NSZeroRect];
                msg_sendf![view, addSubview: text_view];
                text_view
            })
            .collect();

        self.lines = lines;
    }

    pub fn update(&mut self, bytes: &[u8]) {
        let scale = self.window.scale_factor();
        let size = self.window.inner_size();
        let height = size.height as f64 / scale;
        let width = size.width as f64 / scale;
        let n_lines = (height / LINE_HEIGHT) as usize;
        let color = unsafe { NSColor::colorWithRed_green_blue_alpha_(nil, 1., 1., 1., 1.) };

        for i in 0..self.lines.len() {
            let text_view = self.lines[i];

            if i < n_lines {
                let (inst_0, inst_1) = match i * 2 {
                    j if j < bytes.len() - 1 => (bytes[j], bytes[j + 1]),
                    j if j < bytes.len() => (bytes[j], 0u8),
                    _ => (0u8, 0u8),
                };
                let text = format!("{:04} {:02X?}{:02X?} TODO", i, inst_0, inst_1);
                let length = text.len() as u64;
                let range_all = NSRange::new(0, length);
                let font = self.font.to_void();
                let rect = NSRect::new(
                    NSPoint::new(0., (height as f64) - (i as f64) * LINE_HEIGHT - LINE_HEIGHT),
                    NSSize::new(width as f64, LINE_HEIGHT),
                );

                unsafe {
                    let storage: Id = msg_send![text_view, textStorage];
                    let string: Id = {
                        let s1: Id = NSString::alloc(nil).init_str(&text).autorelease();
                        let s2: Id = msg_send![class!(NSMutableAttributedString), alloc];
                        msg_send![s2, initWithString: s1]
                    };
                    msg_sendf![text_view, setFrame: rect];
                    msg_sendf![text_view, setHidden: NO];
                    msg_sendf![text_view, setBackgroundColor: NSColor::clearColor(nil)];
                    msg_sendf![string, addAttribute:NSFontAttributeName value:font range:range_all];
                    msg_sendf![string, addAttribute:NSForegroundColorAttributeName value:color range:range_all];
                    msg_sendf![storage, setAttributedString: string];
                }
            } else {
                unsafe {
                    msg_sendf![text_view, setHidden: YES];
                    msg_sendf![text_view, setFrame: NSZeroRect];
                }
            }
        }
    }
}

pub struct Menu {}

impl Menu {
    pub fn new() -> Self {
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
        Menu {}
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
