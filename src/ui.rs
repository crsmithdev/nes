use crate::Id;
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

//#[link(name = "AppKit", kind = "framework")]
extern "C" {
    pub static NSFontAttributeName: Id;
    pub static NSForegroundColorAttributeName: Id;
    pub static NSZeroRect: NSRect;
}

macro_rules! msg_yeet {
    ($obj:expr, $name:ident) => ({
        let _: Id = msg_send!($obj, $name);
    });
    ($obj:expr, $($name:ident : $arg:expr)+) => ({
        let _: Id = msg_send!($obj, $($name : $arg)+);
    });
}

const LINE_HEIGHT: f64 = 20.;

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

        let mut view = InstructionView {
            window,
            lines: vec![],
            font: font,
        };
        view.alloc_lines(50); // TODO
        view
    }

    fn alloc_lines(&mut self, n_lines: i32) {
        // TODO autorelease
        // TODO pool
        let view = self.window.ns_view() as Id;

        let lines = (0..n_lines)
            .map(|_| unsafe {
                let alloc: Id = msg_send![class!(NSTextView), alloc];
                let text_view: Id = msg_send![alloc, initWithFrame: NSZeroRect];
                msg_yeet![view, addSubview: text_view];
                text_view
            })
            .collect();

        self.lines = lines;
    }

    pub fn update(&mut self, bytes: &[u8]) {
        // TODO autorelease
        let height = self.window.inner_size().height / 2;
        let width = self.window.inner_size().width / 2;
        let n_lines = self.window.inner_size().height / (LINE_HEIGHT as u32) / 2; // TODO hidpi
        for i in 0..self.lines.len() {
            let text_view = self.lines[i];
            if i < (n_lines as usize) {
                let (inst_0, inst_1) = match i * 2 {
                    j if j < bytes.len() - 1 => (bytes[j], bytes[j + 1]),
                    j if j < bytes.len() => (bytes[j], 0u8),
                    _ => (0u8, 0u8),
                };
                let text = format!("{:04} {:02X?}{:02X?} TODO", i, inst_0, inst_1);
                let rect = NSRect::new(
                    NSPoint::new(0., (height as f64) - (i as f64) * LINE_HEIGHT - LINE_HEIGHT),
                    NSSize::new(width as f64, LINE_HEIGHT),
                );

                unsafe {
                    let _: Id = msg_send![text_view, setFrame: rect];
                    let _: Id = msg_send![text_view, setHidden: NO];
                    let _: Id = msg_send![text_view, setBackgroundColor: NSColor::clearColor(nil)];
                    let f = self.font.to_void();
                    let string: Id = {
                        let s1: Id = NSString::alloc(nil).init_str(&text).autorelease();
                        let s2: Id = msg_send![class!(NSMutableAttributedString), alloc];
                        msg_send![s2, initWithString: s1]
                    };
                    let length: u64 = msg_send![string, length];
                    let range_all = NSRange::new(0, length);
                    let storage: Id = msg_send![text_view, textStorage];
                    let _: Id =
                        msg_send![string, addAttribute:NSFontAttributeName value:f range:range_all];
                    let color = NSColor::colorWithRed_green_blue_alpha_(nil, 1., 1., 1., 1.);
                    let _: Id = msg_send![string, addAttribute:NSForegroundColorAttributeName value:color range:range_all];
                    let _: Id = msg_send![storage, setAttributedString: string];
                }
            } else {
                unsafe {
                    let _: Id = msg_send![text_view, setHidden: YES];
                    let _: Id = msg_send![text_view, setFrame: NSZeroRect];
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
            let _: Id = msg_send![
                dock,
                activateWithOptions: NSApplicationActivateIgnoringOtherApps
            ];
            // TODO fail
        }
        let _: Id = msg_send![
            app,
            activateWithOptions: NSApplicationActivateIgnoringOtherApps
        ];
    }
}
