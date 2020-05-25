use std::{cmp::min, collections::HashMap, fmt, num::Wrapping};

error_chain! {
    types {
        CPUError, CPUErrorKind, CPUResultExt, CPUResult;
    }

    errors {
        UnrecognizedOpcode(o: u8) { }
        InstructionTiming(i: Instruction, t: u8) {
            description("instruction timing error")
            display("exeuction timing error: {}, t={}", i, t)
        }
        InstructionExecution(i: Instruction) {
            description("instruction execution error")
            display("exeuction execution error: {}", i)
        }
        NotImplemented(i: Instruction) {
            description("instruction not implemented")
            display("instruction not implemented: {}", i)
         }
    }
}

macro_rules! instruction {
    ($opcode:expr, $name:expr, $bytes:expr, $cycles:expr, $mode:expr) => {
        Instruction {
            opcode: $opcode,
            name: $name,
            mode: $mode,
            low: 0,
            high: 0,
            bytes: $bytes,
            cycles: $cycles,
        }
    };
}

macro_rules! wrap_add {
    ($a:expr, $b:expr) => {
        (Wrapping($a) + Wrapping($b)).0
    };
}

lazy_static! {
    static ref OPCODES: HashMap<u8, Instruction> = hashmap! {
        0x69 => instruction!(0x69, &"ADC", 2, 2, AddressMode::Immediate),
        0x65 => instruction!(0x65, &"ADC", 2, 3, AddressMode::ZeroPage),
        0x75 => instruction!(0x75, &"ADC", 2, 4, AddressMode::ZeroPageX),
        0x6D => instruction!(0x6D, &"ADC", 3, 4, AddressMode::Absolute),
        0x7D => instruction!(0x7D, &"ADC", 3, 5, AddressMode::AbsoluteX),
        0x79 => instruction!(0x79, &"ADC", 3, 5, AddressMode::AbsoluteY),
        0x61 => instruction!(0x61, &"ADC", 2, 6, AddressMode::IndirectX),
        0x71 => instruction!(0x71, &"ADC", 2, 6, AddressMode::IndirectY),
        0x29 => instruction!(0x29, &"AND", 2, 2, AddressMode::Immediate),
        0x25 => instruction!(0x25, &"AND", 2, 3, AddressMode::ZeroPage),
        0x35 => instruction!(0x35, &"AND", 2, 4, AddressMode::ZeroPageX),
        0x2D => instruction!(0x2D, &"AND", 3, 4, AddressMode::Absolute),
        0x3D => instruction!(0x3D, &"AND", 3, 5, AddressMode::AbsoluteX),
        0x39 => instruction!(0x39, &"AND", 3, 5, AddressMode::AbsoluteY),
        0x21 => instruction!(0x21, &"AND", 2, 6, AddressMode::IndirectX),
        0x31 => instruction!(0x31, &"AND", 2, 6, AddressMode::IndirectY),
        0x0A => instruction!(0x0A, &"ASL", 1, 2, AddressMode::Accumulator),
        0x06 => instruction!(0x06, &"ASL", 2, 5, AddressMode::ZeroPage),
        0x16 => instruction!(0x16, &"ASL", 2, 6, AddressMode::ZeroPageX),
        0x0E => instruction!(0x0E, &"ASL", 3, 6, AddressMode::Absolute),
        0x1E => instruction!(0x1E, &"ASL", 3, 7, AddressMode::AbsoluteX),
        0x24 => instruction!(0x24, &"BIT", 2, 3, AddressMode::ZeroPage),
        0x2C => instruction!(0x2C, &"BIT", 3, 4, AddressMode::Absolute),
        0x10 => instruction!(0x10, &"BPL", 2, 4, AddressMode::Relative),
        0x30 => instruction!(0x30, &"BMI", 2, 4, AddressMode::Relative),
        0x50 => instruction!(0x50, &"BVC", 2, 4, AddressMode::Relative),
        0x70 => instruction!(0x70, &"BVS", 2, 4, AddressMode::Relative),
        0x90 => instruction!(0x90, &"BCC", 2, 4, AddressMode::Relative),
        0xB0 => instruction!(0xB0, &"BCS", 2, 4, AddressMode::Relative),
        0xD0 => instruction!(0xD0, &"BNE", 2, 4, AddressMode::Relative),
        0xF0 => instruction!(0xF0, &"BEQ", 2, 4, AddressMode::Relative),
        0x00 => instruction!(0x00, &"BRK", 2, 7, AddressMode::Immediate),
        0xC9 => instruction!(0xC9, &"CMP", 2, 2, AddressMode::Immediate),
        0xC5 => instruction!(0xC5, &"CMP", 2, 3, AddressMode::ZeroPage),
        0xD5 => instruction!(0xD5, &"CMP", 2, 4, AddressMode::ZeroPageX),
        0xCD => instruction!(0xCD, &"CMP", 3, 4, AddressMode::Absolute),
        0xDD => instruction!(0xDD, &"CMP", 3, 5, AddressMode::AbsoluteX),
        0xD9 => instruction!(0xD9, &"CMP", 3, 5, AddressMode::AbsoluteY),
        0xC1 => instruction!(0xC1, &"CMP", 2, 6, AddressMode::IndirectX),
        0xD1 => instruction!(0xD1, &"CMP", 2, 6, AddressMode::IndirectY),
        0xE0 => instruction!(0xE0, &"CPX", 2, 2, AddressMode::Immediate),
        0xE4 => instruction!(0xE4, &"CPX", 2, 3, AddressMode::ZeroPage),
        0xEC => instruction!(0xEC, &"CPX", 3, 4, AddressMode::Absolute),
        0xC0 => instruction!(0xC0, &"CPY", 2, 2, AddressMode::Immediate),
        0xC4 => instruction!(0xC4, &"CPY", 2, 3, AddressMode::ZeroPage),
        0xCC => instruction!(0xCC, &"CPY", 3, 4, AddressMode::Absolute),
        0xC6 => instruction!(0xC6, &"DEC", 2, 5, AddressMode::ZeroPage),
        0xD6 => instruction!(0xD6, &"DEC", 2, 6, AddressMode::ZeroPageX),
        0xCE => instruction!(0xCE, &"DEC", 3, 6, AddressMode::Absolute),
        0xDE => instruction!(0xDE, &"DEC", 3, 7, AddressMode::AbsoluteX),
        0x49 => instruction!(0x49, &"EOR", 2, 2, AddressMode::Immediate),
        0x45 => instruction!(0x45, &"EOR", 2, 3, AddressMode::ZeroPage),
        0x55 => instruction!(0x55, &"EOR", 2, 4, AddressMode::ZeroPageX),
        0x4D => instruction!(0x4D, &"EOR", 3, 4, AddressMode::Absolute),
        0x5D => instruction!(0x5D, &"EOR", 3, 5, AddressMode::AbsoluteX),
        0x59 => instruction!(0x59, &"EOR", 3, 5, AddressMode::AbsoluteY),
        0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
        0x51 => instruction!(0x51, &"EOR", 2, 6, AddressMode::IndirectY),
        0x18 => instruction!(0x18, &"CLC", 1, 2, AddressMode::Implicit),
        0x38 => instruction!(0x38, &"SEC", 1, 2, AddressMode::Implicit),
        0x58 => instruction!(0x58, &"CLI", 1, 2, AddressMode::Implicit),
        0x78 => instruction!(0x78, &"SEI", 1, 2, AddressMode::Implicit),
        0xB8 => instruction!(0xB8, &"CLV", 1, 2, AddressMode::Implicit),
        0xD8 => instruction!(0xD8, &"CLD", 1, 2, AddressMode::Implicit),
        0xF8 => instruction!(0xF8, &"SED", 1, 2, AddressMode::Implicit),
        0xE6 => instruction!(0xE6, &"INC", 2, 5, AddressMode::ZeroPage),
        0xF6 => instruction!(0xF6, &"INC", 2, 6, AddressMode::ZeroPageX),
        0xEE => instruction!(0xEE, &"INC", 3, 6, AddressMode::Absolute),
        0xFE => instruction!(0xFE, &"INC", 3, 7, AddressMode::AbsoluteX),
        0x4C => instruction!(0x4C, &"JMP", 3, 3, AddressMode::Absolute),
        0x6C => instruction!(0x6C, &"JMP", 3, 5, AddressMode::Indirect),
        0x20 => instruction!(0x20, &"JSR", 3, 6, AddressMode::Absolute),
        0xA9 => instruction!(0xA9, &"LDA", 2, 2, AddressMode::Immediate),
        0xA5 => instruction!(0xA5, &"LDA", 2, 3, AddressMode::ZeroPage),
        0xB5 => instruction!(0xB5, &"LDA", 2, 4, AddressMode::ZeroPageX),
        0xAD => instruction!(0xAD, &"LDA", 3, 4, AddressMode::Absolute),
        0xBD => instruction!(0xBD, &"LDA", 3, 5, AddressMode::AbsoluteX),
        0xB9 => instruction!(0xB9, &"LDA", 3, 5, AddressMode::AbsoluteY),
        0xA1 => instruction!(0xA1, &"LDA", 2, 6, AddressMode::IndirectX),
        0xB1 => instruction!(0xB1, &"LDA", 2, 6, AddressMode::IndirectY),
        0xA2 => instruction!(0xA2, &"LDX", 2, 2, AddressMode::Immediate),
        0xA6 => instruction!(0xA6, &"LDX", 2, 3, AddressMode::ZeroPage),
        0xB6 => instruction!(0xB6, &"LDX", 2, 4, AddressMode::ZeroPageY),
        0xAE => instruction!(0xAE, &"LDX", 3, 4, AddressMode::Absolute),
        0xBE => instruction!(0xBE, &"LDX", 3, 5, AddressMode::AbsoluteY),
        0xA0 => instruction!(0xA0, &"LDY", 2, 2, AddressMode::Immediate),
        0xA4 => instruction!(0xA4, &"LDY", 2, 3, AddressMode::ZeroPage),
        0xB4 => instruction!(0xB4, &"LDY", 2, 4, AddressMode::ZeroPageX),
        0xAC => instruction!(0xAC, &"LDY", 3, 4, AddressMode::Absolute),
        0xBC => instruction!(0xBC, &"LDY", 3, 5, AddressMode::AbsoluteX),
        0x4A => instruction!(0x4A, &"LSR", 1, 2, AddressMode::Accumulator),
        0x46 => instruction!(0x46, &"LSR", 2, 5, AddressMode::ZeroPage),
        0x56 => instruction!(0x56, &"LSR", 2, 6, AddressMode::ZeroPageX),
        0x4E => instruction!(0x4E, &"LSR", 3, 6, AddressMode::Absolute),
        0x5E => instruction!(0x5E, &"LSR", 3, 7, AddressMode::AbsoluteX),
        0xEA => instruction!(0xEA, &"NOP", 1, 2, AddressMode::Implicit),
        0x09 => instruction!(0x09, &"ORA", 2, 2, AddressMode::Immediate),
        0x05 => instruction!(0x05, &"ORA", 2, 3, AddressMode::ZeroPage),
        0x15 => instruction!(0x15, &"ORA", 2, 4, AddressMode::ZeroPageX),
        0x0D => instruction!(0x0D, &"ORA", 3, 4, AddressMode::Absolute),
        0x1D => instruction!(0x1D, &"ORA", 3, 5, AddressMode::AbsoluteX),
        0x19 => instruction!(0x19, &"ORA", 3, 5, AddressMode::AbsoluteY),
        0x01 => instruction!(0x01, &"ORA", 2, 6, AddressMode::IndirectX),
        0x11 => instruction!(0x11, &"ORA", 2, 6, AddressMode::IndirectY),
        0xAA => instruction!(0xAA, &"TAX", 1, 2, AddressMode::Implicit),
        0x8A => instruction!(0x8A, &"TXA", 1, 2, AddressMode::Implicit),
        0xCA => instruction!(0xCA, &"DEX", 1, 2, AddressMode::Implicit),
        0xE8 => instruction!(0xE8, &"tNX", 1, 2, AddressMode::Implicit),
        0xA8 => instruction!(0xA8, &"TAY", 1, 2, AddressMode::Implicit),
        0x98 => instruction!(0x98, &"TYA", 1, 2, AddressMode::Implicit),
        0x88 => instruction!(0x88, &"DEY", 1, 2, AddressMode::Implicit),
        0xC8 => instruction!(0xC8, &"INY", 1, 2, AddressMode::Implicit),
        0x2A => instruction!(0x2A, &"ROL", 1, 2, AddressMode::Accumulator),
        0x26 => instruction!(0x26, &"ROL", 2, 5, AddressMode::ZeroPage),
        0x36 => instruction!(0x36, &"ROL", 2, 6, AddressMode::ZeroPageX),
        0x2E => instruction!(0x2E, &"ROL", 3, 6, AddressMode::Absolute),
        0x3E => instruction!(0x3E, &"ROL", 3, 7, AddressMode::AbsoluteX),
        0x6A => instruction!(0x6A, &"ROR", 1, 2, AddressMode::Accumulator),
        0x66 => instruction!(0x66, &"ROR", 2, 5, AddressMode::ZeroPage),
        0x76 => instruction!(0x76, &"ROR", 2, 6, AddressMode::ZeroPageX),
        0x6E => instruction!(0x6E, &"ROR", 3, 6, AddressMode::Absolute),
        0x7E => instruction!(0x7E, &"ROR", 3, 7, AddressMode::AbsoluteX),
        0x40 => instruction!(0x40, &"RTI", 1, 6, AddressMode::Implicit),
        0x60 => instruction!(0x60, &"RTS", 1, 6, AddressMode::Implicit),
        0xE9 => instruction!(0xE9, &"SBC", 2, 2, AddressMode::Immediate),
        0xE5 => instruction!(0xE5, &"SBC", 2, 3, AddressMode::ZeroPage),
        0xF5 => instruction!(0xF5, &"SBC", 2, 4, AddressMode::ZeroPageX),
        0xED => instruction!(0xED, &"SBC", 3, 4, AddressMode::Absolute),
        0xFD => instruction!(0xFD, &"SBC", 3, 5, AddressMode::AbsoluteX),
        0xF9 => instruction!(0xF9, &"SBC", 3, 5, AddressMode::AbsoluteY),
        0xE1 => instruction!(0xE1, &"SBC", 2, 6, AddressMode::IndirectX),
        0xF1 => instruction!(0xF1, &"SBC", 2, 6, AddressMode::IndirectY),
        0x85 => instruction!(0x85, &"STA", 2, 3, AddressMode::ZeroPage),
        0x95 => instruction!(0x95, &"STA", 2, 4, AddressMode::ZeroPageX),
        0x8D => instruction!(0x8D, &"STA", 3, 4, AddressMode::Absolute),
        0x9D => instruction!(0x9D, &"STA", 3, 5, AddressMode::AbsoluteX),
        0x99 => instruction!(0x99, &"STA", 3, 5, AddressMode::AbsoluteY),
        0x81 => instruction!(0x81, &"STA", 2, 6, AddressMode::IndirectX),
        0x91 => instruction!(0x91, &"STA", 2, 6, AddressMode::IndirectY),
        0x9A => instruction!(0x9A, &"TXS", 1, 2, AddressMode::Implicit),
        0xBA => instruction!(0xBA, &"TSX", 1, 2, AddressMode::Implicit),
        0x48 => instruction!(0x48, &"PHA", 1, 3, AddressMode::Implicit),
        0x68 => instruction!(0x68, &"PLA", 1, 4, AddressMode::Implicit),
        0x08 => instruction!(0x08, &"PHP", 1, 3, AddressMode::Implicit),
        0x28 => instruction!(0x28, &"PLP", 1, 4, AddressMode::Implicit),
        0x86 => instruction!(0x86, &"STX", 2, 3, AddressMode::ZeroPage),
        0x96 => instruction!(0x96, &"STX", 2, 4, AddressMode::ZeroPageY),
        0x8E => instruction!(0x8E, &"STX", 3, 4, AddressMode::Absolute),
        0x84 => instruction!(0x84, &"STY", 2, 3, AddressMode::ZeroPage),
        0x94 => instruction!(0x94, &"STY", 2, 4, AddressMode::ZeroPageX),
        0x8C => instruction!(0x8C, &"STY", 3, 4, AddressMode::Absolute),
    };
}
#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Pins {
    pub addr: u16,
    pub data: u8,
    pub irq: bool,
    pub rdy: bool,
    pub res: bool,
    pub rw: bool,
    pub sync: bool,
    addr_write: bool,
    data_write: bool,
}

impl Pins {
    pub fn startup() -> Self {
        Pins {
            res: true,
            rw: true,
            sync: true,
            ..Pins::default()
        }
    }
}

impl fmt::Display for Pins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bus = format!("address: {:#04X}, data: {:#02X}", self.addr, self.data);
        let flags = format!(
            "irq: {}, rdy: {}, res: {}, rw: {}, sync: {}",
            self.irq as u8, self.irq as u8, self.irq as u8, self.irq as u8, self.irq as u8
        );
        f.write_str(&format!("{{{}, {}}}", bus, flags))
    }
}

impl Pins {
    fn cycle_reset(&mut self) {
        self.rw = true;
        self.sync = false;
        self.addr_write = false;
        self.data_write = false;
    }

    fn addr_changed(&self) -> bool {
        self.addr_write
    }

    fn set_data(&mut self, data: u8) {
        self.data = data;
        self.rw = false;
    }

    fn set_addr(&mut self, low: u8, high: u8) {
        self.addr = (low as u16) | ((high as u16) << 8);
        self.addr_write = true;
    }

    fn set_addr8(&mut self, low: u8) {
        self.addr = low as u16;
        self.addr_write = true;
    }

    fn set_addr16(&mut self, addr: u16) {
        self.addr = addr;
        self.addr_write = true;
    }

    fn inc_addr(&mut self) {
        self.addr = self.addr.wrapping_add(1)
    }

    fn dec_addr(&mut self) {
        self.addr = self.addr.wrapping_sub(1)
    }

    fn inc_addr_page(&mut self) {
        self.addr = self.addr.wrapping_add(0x100)
    }

    fn set_addr_offset_nocarry(&mut self, low: u8, high: u8, offset: u8) -> bool {
        let (low, carry) = low.overflowing_add(offset);
        let high = high;
        self.set_addr(low, high);
        carry
    }

    fn set_addr_offset(&mut self, low: u8, high: u8, offset: u8) {
        let (low, carry) = low.overflowing_add(offset);
        let high = high + carry as u8;
        self.set_addr(low, high);
    }
}

#[derive(Clone, Copy, Debug)]
pub enum AddressMode {
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Implicit,
    Accumulator,
    Relative,
    Immediate,
}

#[derive(Clone, Copy, Debug)]
pub struct Instruction {
    pub opcode: u8,
    pub name: &'static str,
    pub mode: AddressMode,
    pub low: u8,
    pub high: u8,
    pub bytes: u8,
    pub cycles: u8,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!(
            "{{{:#04X}|{} ({}, {:?}){}}}",
            self.opcode,
            self.name,
            self.bytes,
            self.mode,
            match self.bytes {
                2 => format!(" {}", self.low),
                3 => format!(" {}, {}", self.low, self.high),
                _ => format!(""),
            }
        ))
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct Flags {
    pub carry: bool,
    pub zero: bool,
    pub irq_disable: bool,
    pub decimal: bool,
    pub overflow: bool,
    pub negative: bool,
}

impl From<u8> for Flags {
    fn from(item: u8) -> Self {
        Self {
            carry: item & 0x1 == 0x1,
            zero: item & 0x2 == 0x2,
            irq_disable: item & 0x4 == 0x4,
            decimal: item & 0x8 == 0x8,
            overflow: item & 0x40 == 0x40,
            negative: item & 0x80 == 0x80,
        }
    }
}

impl Into<u8> for Flags {
    fn into(self) -> u8 {
        let mut byte = 0b00110000;
        if self.carry {
            byte |= 0x1;
        }
        if self.zero {
            byte |= 0x2;
        }
        if self.irq_disable {
            byte |= 0x4;
        }
        if self.decimal {
            byte |= 0x8
        }
        if self.overflow {
            byte |= 0x40;
        }
        if self.negative {
            byte |= 0x80;
        }
        byte
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct CPU {
    pub pc: u16,
    pub s: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub t: u8,
    pub flags: Flags,
    pub pins: Pins,
    pub cycles: u64,
    jumped: bool,
    mdr: u8,
    pub ir_addr: u16,
    pub ir: Instruction,
}

impl CPU {
    const STACK_PAGE: u8 = 0x1;

    pub fn new() -> Self {
        Self {
            pc: 0,
            s: 0xFF,
            a: 0,
            x: 0,
            y: 0,
            t: 0,
            flags: Flags::default(),
            pins: Pins::startup(),
            cycles: 0,
            jumped: false,
            mdr: 0,
            ir: decode(0xEA).unwrap(), // TODO
            ir_addr: 0,
        }
    }

    /* Instruction Execution */

    pub fn cycle(&mut self) -> CPUResult<()> {
        trace!(
            "begin cycle {} [t{}->], pins: {}, ir: {}",
            self.cycles,
            self.t,
            self.pins,
            self.ir
        );

        self.pins.cycle_reset();
        self.jumped = false;

        let result = match self.t {
            0 => self.new_instruction(),
            _ => {
                self.update_instruction();
                self.dispatch(self.ir.opcode)
            }
        };

        match self.t {
            t if t + 1 == self.ir.cycles => {
                self.t = 0;
                if !self.jumped {
                    self.pc = self.ir_addr + self.ir.bytes as u16;
                    self.jumped = false;
                }
                self.pins.set_addr16(self.pc);
                self.pins.sync = true;
            }
            t if t < self.ir.bytes => {
                self.t += 1;
                if self.pins.rw && !self.pins.addr_changed() {
                    self.pc += 1;
                    self.pins.set_addr16(self.pc);
                }
            }
            _ => self.t += 1,
        }

        trace!(
            "end cycle {} [->t{}], pins: {}, ir: {}",
            self.cycles,
            self.t,
            self.pins,
            self.ir
        );
        self.cycles += 1;

        result
    }

    fn new_instruction(&mut self) -> CPUResult<()> {
        self.mdr = 0;
        self.ir_addr = self.pins.addr;
        self.ir = decode(self.pins.data)?;
        debug!("decoded @ {:#04X}: {}", self.pins.addr, self.ir);
        Ok(())
    }

    fn update_instruction(&mut self) {
        match self.t {
            1 if self.ir.bytes > 1 => self.ir.low = self.pins.data,
            2 if self.ir.bytes > 2 => self.ir.high = self.pins.data,
            _ => (),
        }
    }

    fn dispatch(&mut self, opcode: u8) -> CPUResult<()> {
        match opcode {
            0x69 /* ADC #imm     */ => self.cycle_memop(CPU::add_with_carry),
            0x65 /* ADC $zp      */ => self.cycle_memop(CPU::add_with_carry),
            0x75 /* ADC $zp,x    */ => self.cycle_memop(CPU::add_with_carry),
            0x6D /* ADC $abs     */ => self.cycle_memop(CPU::add_with_carry),
            0x7D /* ADC $abs,x   */ => self.cycle_memop(CPU::add_with_carry),
            0x79 /* ADC $abs,y   */ => self.cycle_memop(CPU::add_with_carry),
            0x61 /* ADC $(ind,x) */ => self.cycle_memop(CPU::add_with_carry),
            0x71 /* ADC $(ind),y */ => self.cycle_memop(CPU::add_with_carry),
            0x29 /* AND #imm     */ => self.cycle_memop(CPU::and),
            0x25 /* AND $zp      */ => self.cycle_memop(CPU::and),
            0x35 /* AND $zp,x    */ => self.cycle_memop(CPU::and),
            0x2D /* AND $abs     */ => self.cycle_memop(CPU::and),
            0x3D /* AND $abs,x   */ => self.cycle_memop(CPU::and),
            0x39 /* AND $abs,y   */ => self.cycle_memop(CPU::and),
            0x21 /* AND $(ind,x) */ => self.cycle_memop(CPU::and),
            0x31 /* AND $(ind),y */ => self.cycle_memop(CPU::and),
            0x0A /* ASL A        */ => self.cycle_unimplemented(CPU::nop),
            0x06 /* ASL $zp      */ => self.cycle_unimplemented(CPU::nop),
            0x16 /* ASL $zp,x    */ => self.cycle_unimplemented(CPU::nop),
            0x0E /* ASL $abs     */ => self.cycle_unimplemented(CPU::nop),
            0x1E /* ASL $abs,x   */ => self.cycle_unimplemented(CPU::nop),
            0x24 /* BIT $zp,x    */ => self.cycle_memop(CPU::bit),
            0x2C /* BIT $abs     */ => self.cycle_memop(CPU::bit),
            0x10 /* BPL $rel     */ => self.cycle_branch(CPU::branch_not_negative),
            0x30 /* BMI $rel     */ => self.cycle_branch(CPU::branch_negative),
            0x50 /* BVC $rel     */ => self.cycle_branch(CPU::branch_not_overflow),
            0x70 /* BVS $rel     */ => self.cycle_branch(CPU::branch_overflow),
            0x90 /* BCC $rel     */ => self.cycle_branch(CPU::branch_not_carry),
            0xB0 /* BCS $rel     */ => self.cycle_branch(CPU::branch_carry),
            0xD0 /* BNE $rel     */ => self.cycle_branch(CPU::branch_not_zero),
            0xF0 /* BEQ $rel     */ => self.cycle_branch(CPU::branch_zero),
            0x00 /* BRK #imm     */ => self.cycle_unimplemented(CPU::nop),
            0xC9 /* CMP #imm     */ => self.cycle_memop(CPU::compare_a),
            0xC5 /* CMP $zp      */ => self.cycle_memop(CPU::compare_a),
            0xD5 /* CMP $zp,x    */ => self.cycle_memop(CPU::compare_a),
            0xCD /* CMP $abs     */ => self.cycle_memop(CPU::compare_a),
            0xDD /* CMP $abs,x   */ => self.cycle_memop(CPU::compare_a),
            0xD9 /* CMP $abs,y   */ => self.cycle_memop(CPU::compare_a),
            0xC1 /* CMP $(ind,x) */ => self.cycle_memop(CPU::compare_a),
            0xD1 /* CMP $(ind),y */ => self.cycle_memop(CPU::compare_a),
            0xE0 /* CPX #imm     */ => self.cycle_memop(CPU::compare_x),
            0xE4 /* CPX $zp      */ => self.cycle_memop(CPU::compare_x),
            0xEC /* CPX $abs     */ => self.cycle_memop(CPU::compare_x),
            0xC0 /* CPY #imm     */ => self.cycle_memop(CPU::compare_y),
            0xC4 /* CPY $zp      */ => self.cycle_memop(CPU::compare_y),
            0xCC /* CPY $abs     */ => self.cycle_memop(CPU::compare_y),
            0xC6 /* DEC $zp      */ => self.cycle_rmw(CPU::dec),
            0xD6 /* DEC $zp,x    */ => self.cycle_rmw(CPU::dec),
            0xCE /* DEC $abs     */ => self.cycle_rmw(CPU::dec),
            0xDE /* DEC $abs,x   */ => self.cycle_rmw(CPU::dec),
            0x49 /* EOR #imm     */ => self.cycle_memop(CPU::xor),
            0x45 /* EOR $zp      */ => self.cycle_memop(CPU::xor),
            0x55 /* EOR $zp,x    */ => self.cycle_memop(CPU::xor),
            0x4D /* EOR $abs     */ => self.cycle_memop(CPU::xor),
            0x5D /* EOR $abs,x   */ => self.cycle_memop(CPU::xor),
            0x59 /* EOR $abs,y   */ => self.cycle_memop(CPU::xor),
            0x41 /* EOR $(ind,x) */ => self.cycle_memop(CPU::xor),
            0x51 /* EOR $(ind),y */ => self.cycle_memop(CPU::xor),
            0x18 /* CLC          */ => self.cycle_implied(CPU::clear_carry),
            0x38 /* SEC          */ => self.cycle_implied(CPU::set_carry),
            0x58 /* CLI          */ => self.cycle_implied(CPU::clear_irq_disable),
            0x78 /* SEI          */ => self.cycle_implied(CPU::set_irq_disable),
            0xB8 /* CLV          */ => self.cycle_implied(CPU::clear_negative),
            0xD8 /* CLD          */ => self.cycle_implied(CPU::clear_decimal),
            0xF8 /* SED          */ => self.cycle_implied(CPU::set_decimal),
            0xE6 /* INC $zp      */ => self.cycle_rmw(CPU::inc),
            0xF6 /* INC $zp,x    */ => self.cycle_rmw(CPU::inc),
            0xEE /* INC $abs     */ => self.cycle_rmw(CPU::inc),
            0xFE /* INC $abs,x   */ => self.cycle_rmw(CPU::inc),
            0x4C /* JMP $abs     */ => self.cycle_jump(),
            0x6C /* JMP $(ind)   */ => self.cycle_jump(),
            0x20 /* JSR $abs     */ => self.cycle_jsr(),
            0xA9 /* LDA #imm     */ => self.cycle_memop(CPU::set_a),
            0xA5 /* LDA $zp      */ => self.cycle_memop(CPU::set_a),
            0xB5 /* LDA $zp,x    */ => self.cycle_memop(CPU::set_a),
            0xAD /* LDA $abs     */ => self.cycle_memop(CPU::set_a),
            0xBD /* LDA $abs,x   */ => self.cycle_memop(CPU::set_a),
            0xB9 /* LDA $abs,y   */ => self.cycle_memop(CPU::set_a),
            0xA1 /* LDA $(ind,x) */ => self.cycle_memop(CPU::set_a),
            0xB1 /* LDA $(ind),y */ => self.cycle_memop(CPU::set_a),
            0xA2 /* LDX #imm     */ => self.cycle_memop(CPU::set_x),
            0xA6 /* LDX $zp      */ => self.cycle_memop(CPU::set_x),
            0xB6 /* LDX $zp y    */ => self.cycle_memop(CPU::set_x),
            0xAE /* LDX $abs     */ => self.cycle_memop(CPU::set_x),
            0xBE /* LDX $abs,y   */ => self.cycle_memop(CPU::set_x),
            0xA0 /* LDY #imm     */ => self.cycle_memop(CPU::set_y),
            0xA4 /* LDY $zp      */ => self.cycle_memop(CPU::set_y),
            0xB4 /* LDY $zp,x    */ => self.cycle_memop(CPU::set_y),
            0xAC /* LDY $abs     */ => self.cycle_memop(CPU::set_y),
            0xBC /* LDY $abs,y   */ => self.cycle_memop(CPU::set_y),
            0x4A /* LSR A        */ => self.cycle_memop(CPU::set_y),
            0x46 /* LSR $zp      */ => self.cycle_unimplemented(CPU::nop),
            0x56 /* LSR $zp,x    */ => self.cycle_unimplemented(CPU::nop),
            0x4E /* LSR $abs     */ => self.cycle_unimplemented(CPU::nop),
            0x5E /* LSR $abs,x   */ => self.cycle_unimplemented(CPU::nop),
            0xEA /* NOP          */ => self.cycle_implied(CPU::nop),
            0x09 /* ORA #imm     */ => self.cycle_memop(CPU::or),
            0x05 /* ORA $zp      */ => self.cycle_memop(CPU::or),
            0x15 /* ORA $zp,x    */ => self.cycle_memop(CPU::or),
            0x0D /* ORA $abs     */ => self.cycle_memop(CPU::or),
            0x1D /* ORA $abs,x   */ => self.cycle_memop(CPU::or),
            0x19 /* ORA $abs,y   */ => self.cycle_memop(CPU::or),
            0x01 /* ORA $(ind,x) */ => self.cycle_memop(CPU::or),
            0x11 /* ORA $(ind),y */ => self.cycle_memop(CPU::or),
            0xAA /* TAX          */ => self.cycle_implied(CPU::transfer_ax),
            0x8A /* TXA          */ => self.cycle_implied(CPU::transfer_xa),
            0xCA /* DEX          */ => self.cycle_implied(CPU::dec_x),
            0xE8 /* INX          */ => self.cycle_implied(CPU::inc_x),
            0xA8 /* TAY          */ => self.cycle_implied(CPU::transfer_ay),
            0x98 /* TYA          */ => self.cycle_implied(CPU::transfer_ya),
            0x88 /* DEY          */ => self.cycle_implied(CPU::dec_y),
            0xC8 /* INY          */ => self.cycle_implied(CPU::inc_y),
            0x2A /* ROL A        */ => self.cycle_rmw(CPU::rotate_left),
            0x26 /* ROL $zp      */ => self.cycle_rmw(CPU::rotate_left),
            0x36 /* ROL $zp,x    */ => self.cycle_rmw(CPU::rotate_left),
            0x2E /* ROL $abs     */ => self.cycle_rmw(CPU::rotate_left),
            0x3E /* ROL $abs,x   */ => self.cycle_rmw(CPU::rotate_left),
            0x6A /* ROR A        */ => self.cycle_rmw(CPU::rotate_right),
            0x66 /* ROR $zp      */ => self.cycle_rmw(CPU::rotate_right),
            0x76 /* ROR $zp,x    */ => self.cycle_rmw(CPU::rotate_right),
            0x6E /* ROR $abs     */ => self.cycle_rmw(CPU::rotate_right),
            0x7E /* ROR $abs,x   */ => self.cycle_rmw(CPU::rotate_right),
            0x40 /* RTI          */ => self.cycle_unimplemented(CPU::nop),
            0x60 /* RTS          */ => self.cycle_unimplemented(CPU::nop),
            0xE9 /* SBC #imm     */ => self.cycle_unimplemented(CPU::nop),
            0xE5 /* SBC $zp      */ => self.cycle_memop(CPU::sub_with_carry),
            0xF5 /* SBC $zp,x    */ => self.cycle_memop(CPU::sub_with_carry),
            0xED /* SBC $abs     */ => self.cycle_memop(CPU::sub_with_carry),
            0xFD /* SBC $abs,x   */ => self.cycle_memop(CPU::sub_with_carry),
            0xF9 /* SBC $abs,y   */ => self.cycle_memop(CPU::sub_with_carry),
            0xE1 /* SBC $(ind,x) */ => self.cycle_memop(CPU::sub_with_carry),
            0xF1 /* SBC $(ind),y */ => self.cycle_memop(CPU::sub_with_carry),
            0x85 /* STA $zp      */ => self.cycle_store(CPU::get_a),
            0x95 /* STA $zp,x    */ => self.cycle_store(CPU::get_a),
            0x8D /* STA $abs     */ => self.cycle_store(CPU::get_a),
            0x9D /* STA $abs,x   */ => self.cycle_store(CPU::get_a),
            0x99 /* STA $abs,y   */ => self.cycle_store(CPU::get_a),
            0x81 /* STA $(ind,x) */ => self.cycle_store(CPU::get_a),
            0x91 /* STA $(ind),y */ => self.cycle_store(CPU::get_a),
            0x9A /* TXS          */ => self.cycle_implied(CPU::transfer_xs),
            0xBA /* TSX          */ => self.cycle_implied(CPU::transfer_sx),
            0x48 /* PHA          */ => self.cycle_stack_push(CPU::get_a),
            0x68 /* PLA          */ => self.cycle_stack_pull(CPU::set_a),
            0x08 /* PHP          */ => self.cycle_stack_push(CPU::get_flags),
            0x28 /* PLP          */ => self.cycle_stack_pull(CPU::set_flags),
            0x86 /* STX $zp      */ => self.cycle_store(CPU::get_x),
            0x96 /* STX $zp,y    */ => self.cycle_store(CPU::get_x),
            0x8E /* STX $abs     */ => self.cycle_store(CPU::get_x),
            0x84 /* STY $zp      */ => self.cycle_store(CPU::get_y),
            0x94 /* STY $zp,x    */ => self.cycle_store(CPU::get_y),
            0x8C /* STY $abs     */ => self.cycle_store(CPU::get_y),
            _ => bail!(CPUErrorKind::NotImplemented(self.ir)),
        }
    }

    fn cycle_implied(&mut self, action: fn(&mut CPU)) -> CPUResult<()> {
        match self.t {
            1 => action(self),
            _ => bail!(CPUErrorKind::InstructionTiming(self.ir, self.t)),
        }

        Ok(())
    }

    fn cycle_memop(&mut self, action: fn(&mut CPU, u8)) -> CPUResult<()> {
        let inst = self.ir;
        let pins = &mut self.pins;
        let data_in = pins.data;
        let t = self.t;

        match inst.mode {
            AddressMode::Immediate => match t {
                1 => action(self, inst.low),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::ZeroPage => match t {
                1 => pins.set_addr8(inst.low),
                2 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::Absolute => match t {
                1 => (),
                2 => self.pins.set_addr(inst.low, inst.high),
                3 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::ZeroPageX => match t {
                1 => pins.set_addr8(inst.low),
                2 => pins.set_addr8(wrap_add!(inst.low, self.x)),
                3 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::ZeroPageY => match t {
                1 => pins.set_addr8(inst.low),
                2 => pins.set_addr8(wrap_add!(inst.low, self.y)),
                3 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::IndirectX => match t {
                1 => pins.set_addr8(inst.low),
                2 => pins.set_addr8(wrap_add!(inst.low, self.x)),
                3 => {
                    self.mdr = data_in;
                    self.pins.inc_addr();
                }
                4 => self.pins.set_addr(self.mdr, data_in),
                5 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::IndirectY => match t {
                1 => pins.set_addr8(inst.low),
                2 => {
                    self.mdr = data_in;
                    self.pins.inc_addr();
                }
                3 => {
                    let carry = self.pins.set_addr_offset_nocarry(self.mdr, data_in, self.y);
                    self.ir.cycles -= !carry as u8;
                }
                4 => match inst.cycles {
                    5 => action(self, data_in),
                    _ => self.pins.inc_addr_page(),
                },
                5 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::AbsoluteX => match t {
                1 => (),
                2 => {
                    let carry = self
                        .pins
                        .set_addr_offset_nocarry(inst.low, inst.high, self.x);
                    self.ir.cycles -= !carry as u8;
                }
                3 => match inst.cycles {
                    4 => action(self, data_in),
                    _ => self.pins.inc_addr_page(),
                },
                4 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            AddressMode::AbsoluteY => match t {
                1 => (),
                2 => {
                    let carry = self
                        .pins
                        .set_addr_offset_nocarry(inst.low, inst.high, self.y);
                    self.ir.cycles -= !carry as u8;
                }
                3 => match inst.cycles {
                    4 => action(self, data_in),
                    _ => self.pins.inc_addr_page(),
                },
                4 => action(self, data_in),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.ir)),
        }
        Ok(())
    }

    fn cycle_branch(&mut self, branch: fn(&mut CPU) -> bool) -> CPUResult<()> {
        let inst = self.ir;

        match self.t {
            1 => {
                if !branch(self) {
                    self.ir.cycles = 2
                }
            }
            2 => {
                let (addr, carry) = self.get_relative(self.pc, inst.low);
                self.set_pc16(addr);
                self.ir.cycles -= !carry as u8;
            }
            3 => match self.ir.low {
                l if l & 0x80 == 0x80 => self.set_pc16(self.pc - 0x100),
                _ => self.set_pc16(self.pc + 0x100),
            },
            _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        }

        Ok(())
    }

    fn cycle_stack_push(&mut self, action: fn(&CPU) -> u8) -> CPUResult<()> {
        match self.t {
            1 => {
                let data = action(self);
                self.pins.set_addr(self.s, CPU::STACK_PAGE);
                self.pins.set_data(data);
                self.s = self.s.wrapping_sub(1); // (Wrapping(self.s) - Wrapping(1)).0;
            }
            2 => (),
            _ => bail!(CPUErrorKind::InstructionTiming(self.ir, self.t)),
        }
        Ok(())
    }

    fn cycle_stack_pull(&mut self, action: fn(&mut CPU, u8)) -> CPUResult<()> {
        let data = self.pins.data;

        match self.t {
            1 => (),
            2 => {
                self.s = self.s.wrapping_add(1); // (Wrapping(self.s) + Wrapping(1)).0;
                self.pins.set_addr(self.s, CPU::STACK_PAGE)
            }
            3 => action(self, data),
            _ => bail!(CPUErrorKind::InstructionTiming(self.ir, self.t)),
        }
        Ok(())
    }

    fn cycle_store(&mut self, getter: fn(&CPU) -> u8) -> CPUResult<()> {
        let inst = self.ir;
        let pins = &mut self.pins;
        let data_in = pins.data;

        match inst.mode {
            AddressMode::ZeroPage => match self.t {
                1 => {
                    pins.set_addr8(inst.low);
                    self.write_data(getter);
                }
                2 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::Absolute => match self.t {
                1 => (),
                2 => {
                    pins.set_addr(inst.low, inst.high);
                    self.write_data(getter);
                }
                3 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPageX => match self.t {
                1 => pins.set_addr8(inst.low),
                2 => {
                    pins.set_addr8(inst.low.wrapping_add(self.x));
                    self.write_data(getter);
                }
                3 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPageY => match self.t {
                1 => pins.set_addr8(inst.low),
                2 => {
                    pins.set_addr8(inst.low.wrapping_add(self.y));
                    self.write_data(getter);
                }
                3 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::IndirectX => match self.t {
                1 => pins.set_addr8(inst.low),
                2 => pins.set_addr8(inst.low.wrapping_add(self.x)),
                3 => {
                    self.mdr = data_in;
                    pins.inc_addr();
                }
                4 => {
                    pins.set_addr(self.mdr, data_in);
                    self.write_data(getter);
                }
                5 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::IndirectY => match self.t {
                1 => pins.set_addr8(inst.low),
                2 => {
                    self.mdr = data_in;
                    pins.inc_addr();
                }
                3 => pins.set_addr(self.mdr.wrapping_add(self.y), data_in),
                4 => self.write_data(getter),
                5 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::AbsoluteX => match self.t {
                1 => (),
                2 => pins.set_addr_offset(inst.low, inst.high, self.x),
                3 => self.write_data(getter),
                4 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::AbsoluteY => match self.t {
                1 => (),
                2 => pins.set_addr_offset(inst.low, inst.high, self.y),
                3 => self.write_data(getter),
                4 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.ir)),
        }

        Ok(())
    }

    fn cycle_rmw(&mut self, action: fn(&mut CPU, u8) -> u8) -> CPUResult<()> {
        let inst = self.ir;
        let data = self.pins.data;
        let addr = self.pins.addr;

        match inst.mode {
            AddressMode::ZeroPage => match self.t {
                1 => self.pins.set_addr8(data),
                2 => self.mdr = data,
                3 => {
                    let value = action(self, self.mdr);
                    self.pins.set_data(value);
                }
                4 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::Absolute => match self.t {
                1 => self.mdr = data,
                2 => self.pins.set_addr(self.mdr, data),
                3 => self.mdr = data,
                4 => {
                    let value = action(self, self.mdr);
                    self.pins.set_data(value);
                }
                5 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPageX => match self.t {
                1 => self.pins.set_addr8(data),
                2 => self.pins.set_addr8((addr as u8).wrapping_add(self.x)),
                3 => self.mdr = data,
                4 => {
                    let value = action(self, self.mdr);
                    self.pins.set_data(value);
                }
                5 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // AddressMode::AbsoluteX => match self.t {
            //     1 => (),
            //     2 => (),
            //     3 => {
            //         let (low, carry) = inst.low.overflowing_add(self.x);
            //         let high = inst.high + carry as u8;
            //         self.pins.set_addr(low, high);
            //     }
            //     4 => (),
            //     5 => (),
            //     6 => self.pins.set_data(self.mdr),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // AddressMode::ZeroPageY => match self.t {
            //     1 => self.pins.set_addr8(data),
            //     2 => self.pins.set_addr8((Wrapping(addr as u8) + Wrapping(self.y)).0),
            //     3 => action(self, data),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // AddressMode::IndirectX => match self.t {
            //     1 => self.pins.set_addr8(data),
            //     2 => self.pins.set_addr16(addr + self.x as u16),
            //     3 => {
            //         self.pins.set_addr16(addr + 1);
            //         self.mdr = data;
            //     }
            //     4 => self.pins.set_addr(self.mdr, data),
            //     5 => action(self, data),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // AddressMode::IndirectY => match self.t {
            //     1 => self.pins.set_addr8(data),
            //     2 => {
            //         self.mdr = data;
            //         self.pins.set_addr16(addr + 1);
            //     }
            //     3 => {
            //         let low = (Wrapping(self.mdr) + Wrapping(self.y)).0;
            //         let carry = low < self.y;
            //         let high = data + (carry as u8);
            //         self.pins.set_addr(low, high);

            //         if !carry {
            //             self.current.cycles -= 1;
            //         }
            //     }
            //     4 => {
            //         if inst.cycles == 5 {
            //             action(self, data);
            //         }
            //     }
            //     5 => action(self, data),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // AddressMode::AbsoluteX => match self.t {
            //     1 => self.mdr = data,
            //     2 => {
            //         let low = (Wrapping(self.mdr) + Wrapping(self.x)).0;
            //         let carry = low < self.x;
            //         let high = data + (carry as u8);
            //         self.pins.set_addr(low, high);

            //         if !carry {
            //             self.current.cycles -= 1;
            //         }
            //     }
            //     3 => {
            //         if inst.cycles == 4 {
            //             action(self, data);
            //         }
            //     }
            //     4 => action(self, data),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // AddressMode::AbsoluteY => match self.t {
            //     1 => self.mdr = data,
            //     2 => {
            //         let low = (Wrapping(self.mdr) + Wrapping(self.y)).0;
            //         let carry = low < self.y;
            //         let high = data + (carry as u8);
            //         self.pins.set_addr(low, high);

            //         if !carry {
            //             self.current.cycles -= 1;
            //         }
            //     }
            //     3 => {
            //         if inst.cycles == 4 {
            //             action(self, data);
            //         }
            //     }
            //     4 => action(self, data),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            _ => bail!(CPUErrorKind::InstructionExecution(self.ir)),
        }

        Ok(())
    }

    fn cycle_jump(&mut self) -> CPUResult<()> {
        let inst = self.ir;
        let data = self.pins.data;

        match inst.mode {
            AddressMode::Absolute => match self.t {
                1 => (),
                2 => self.set_pc(inst.low, inst.high),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::Indirect => match self.t {
                1 => self.mdr = data,
                2 => self.pins.set_addr(self.mdr, data),
                3 => {
                    self.mdr = data;
                    self.pins.inc_addr();
                }
                4 => self.set_pc(self.mdr, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(inst)),
        }
        Ok(())
    }

    fn cycle_jsr(&mut self) -> CPUResult<()> {
        let inst = self.ir;
        let pins = &mut self.pins;

        match inst.mode {
            AddressMode::Absolute => match self.t {
                1 => (),
                2 => (),
                3 => {
                    pins.set_addr(self.s, CPU::STACK_PAGE);
                    pins.set_data((self.pc - 1 >> 8) as u8);
                }
                4 => {
                    pins.dec_addr();
                    pins.set_data((self.pc - 1) as u8);
                    self.set_pc(inst.low, inst.high);
                }
                5 => (),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(inst)),
        }
        Ok(())
    }

    fn cycle_unimplemented(&mut self, _action: fn(&mut CPU)) -> CPUResult<()> {
        bail!(CPUErrorKind::NotImplemented(self.ir))
    }

    /* Getters */

    fn get_flags(&self) -> u8 {
        self.flags.into()
    }

    fn get_a(&self) -> u8 {
        self.a
    }

    fn get_x(&self) -> u8 {
        self.x
    }

    fn get_y(&self) -> u8 {
        self.y
    }

    /* Setters */

    fn set_a(&mut self, byte: u8) {
        self.a = byte;
        self.flags.zero = byte == 0;
        self.flags.negative = byte & 0x80 == 0x80;
    }

    fn set_x(&mut self, byte: u8) {
        self.x = byte;
        self.flags.zero = byte == 0;
        self.flags.negative = byte & 0x80 == 0x80;
    }

    fn set_y(&mut self, byte: u8) {
        self.y = byte;
        self.flags.zero = byte == 0;
        self.flags.negative = byte & 0x80 == 0x80;
    }

    fn set_s(&mut self, byte: u8) {
        self.s = byte;
    }

    fn set_flags(&mut self, byte: u8) {
        self.flags = Flags::from(byte)
    }

    /* Modifiers */

    fn inc_x(&mut self) {
        self.set_x((Wrapping(self.x) + Wrapping(1)).0)
    }

    fn inc_y(&mut self) {
        self.set_y((Wrapping(self.y) + Wrapping(1)).0)
    }

    fn dec_x(&mut self) {
        self.set_x((Wrapping(self.x) - Wrapping(1)).0)
    }

    fn dec_y(&mut self) {
        self.set_y((Wrapping(self.y) - Wrapping(1)).0)
    }

    fn inc(&mut self, byte: u8) -> u8 {
        let value = (Wrapping(byte) + Wrapping(1)).0;
        self.flags.zero = value == 0;
        self.flags.negative = value & 0x80 == 0x80;
        value
    }

    fn dec(&mut self, byte: u8) -> u8 {
        let value = (Wrapping(byte) - Wrapping(1)).0;
        self.flags.zero = value == 0;
        self.flags.negative = value & 0x80 == 0x80;
        value
    }

    fn rotate_left(&mut self, byte: u8) -> u8 {
        let old_carry = self.flags.carry as u8;
        let new_carry = (byte & 0x80) >> 7;
        self.flags.carry = new_carry & 1 == 1;

        let result = (byte << 1) | old_carry;
        self.flags.zero = result == 0;
        self.flags.negative = result & 0x80 == 0x80;

        result
    }

    fn rotate_right(&mut self, byte: u8) -> u8 {
        let old_carry = self.flags.carry as u8;
        let new_carry = byte & 1;
        self.flags.carry = new_carry & 1 == 1;

        let result = (byte >> 1) | (old_carry << 7);
        self.flags.zero = result == 0;
        self.flags.negative = result & 0x80 == 0x80;

        result
    }

    /* Branch Conditions */

    fn branch_zero(&mut self) -> bool {
        self.flags.zero
    }

    fn branch_not_zero(&mut self) -> bool {
        !self.flags.zero
    }

    fn branch_carry(&mut self) -> bool {
        self.flags.carry
    }

    fn branch_not_carry(&mut self) -> bool {
        !self.flags.carry
    }

    fn branch_overflow(&mut self) -> bool {
        self.flags.overflow
    }

    fn branch_not_overflow(&mut self) -> bool {
        !self.flags.overflow
    }

    fn branch_negative(&mut self) -> bool {
        self.flags.negative
    }

    fn branch_not_negative(&mut self) -> bool {
        !self.flags.negative
    }

    /* Flags */

    fn compare_a(&mut self, byte: u8) {
        self.flags.carry = self.a >= byte;
        self.flags.zero = self.a == byte;
        self.flags.negative = self.a < byte;
    }

    fn compare_x(&mut self, byte: u8) {
        self.flags.carry = self.x >= byte;
        self.flags.zero = self.x == byte;
        self.flags.negative = self.x < byte;
    }

    fn compare_y(&mut self, byte: u8) {
        self.flags.carry = self.y >= byte;
        self.flags.zero = self.y == byte;
        self.flags.negative = self.y < byte;
    }

    fn clear_carry(&mut self) {
        self.flags.carry = false
    }

    fn set_carry(&mut self) {
        self.flags.carry = true;
    }

    fn clear_irq_disable(&mut self) {
        self.flags.irq_disable = false
    }

    fn set_irq_disable(&mut self) {
        self.flags.irq_disable = true;
    }

    fn clear_decimal(&mut self) {
        self.flags.decimal = false
    }

    fn set_decimal(&mut self) {
        self.flags.decimal = true;
    }

    fn clear_negative(&mut self) {
        self.flags.negative = false
    }

    /* Transfers */

    fn transfer_ax(&mut self) {
        self.set_x(self.a);
    }

    fn transfer_ay(&mut self) {
        self.set_y(self.a);
    }

    fn transfer_sx(&mut self) {
        self.set_x(self.s);
    }

    fn transfer_xs(&mut self) {
        self.set_s(self.x);
    }

    fn transfer_xa(&mut self) {
        self.set_a(self.x);
    }

    fn transfer_ya(&mut self) {
        self.set_a(self.y);
    }

    /* Logical */

    fn xor(&mut self, byte: u8) {
        self.a = self.a ^ byte;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a & 0x80 == 0x80;
    }

    fn and(&mut self, byte: u8) {
        self.a = self.a & byte;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a & 0x80 == 0x80;
    }

    fn or(&mut self, byte: u8) {
        self.a = self.a | byte;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a & 0x80 == 0x80;
    }

    fn bit(&mut self, byte: u8) {
        let value = self.a & byte;
        self.flags.zero = value == 0;
        self.flags.overflow = value & 0x40 == 0x40;
        self.flags.negative = value & 0x80 == 0x80;
    }

    /* Math */

    fn add_with_carry(&mut self, byte: u8) {
        let (sum, carry) = {
            let (s, c1) = self.a.overflowing_add(byte);
            let (s, c2) = s.overflowing_add(self.flags.carry as u8);
            (s, c1 | c2)
        };

        self.flags.carry = carry;
        self.flags.zero = sum == 0;
        self.flags.overflow = (!(self.a ^ byte) & (self.a ^ sum) & 0x80) != 0;
        self.flags.negative = sum & 0x80 == 0x80;
        self.a = sum;
    }

    fn sub_with_carry(&mut self, byte: u8) {
        let (diff, carry) = {
            let (d, c1) = self.a.overflowing_sub(byte);
            let (d, c2) = d.overflowing_sub(!self.flags.carry as u8);
            (d, c1 | c2)
        };

        self.flags.carry = !carry;
        self.flags.zero = diff == 0;
        self.flags.overflow = (!(self.a ^ byte) & (self.a ^ diff) & 0x80) != 0;
        self.flags.negative = diff & 0x80 == 0x80;
        self.a = diff;
    }

    /* Misc */

    fn nop(&mut self) {}

    fn set_pc(&mut self, low: u8, high: u8) {
        self.pc = (low as u16) | ((high as u16) << 8);
        self.pins.addr = self.pc;
        self.jumped = true;
    }

    fn set_pc16(&mut self, pc: u16) {
        self.pc = pc;
        self.pins.addr = self.pc;
        self.jumped = true;
    }

    fn write_data(&mut self, getter: fn(&CPU) -> u8) {
        let data_out = getter(self);
        self.pins.set_data(data_out);
    }

    fn get_relative(&self, addr: u16, offset: u8) -> (u16, bool) {
        let low = addr as u8;

        let (next, carry) = match offset as i8 {
            o if o == i8::MIN => low.overflowing_sub(i8::MAX as u8 + 1),
            o if o < 0 => low.overflowing_sub(o.abs() as u8),
            o => low.overflowing_add(o as u8),
        };

        ((addr & 0xFF00) | (next as u16), carry)
    }
}

/*
pub fn decode_all(bytes: &[u8]) -> CPUResult<Vec<(u16, Instruction)>> {
    let mut instructions = vec![];
    let mut address = 0;

    while address < bytes.len() {
        match decode_bytes(bytes, address) {
            Ok(instruction) => {
                let length = instruction.bytes as usize;
                instructions.push((address as u16, instruction));
                address += length;
            }
            Err(e) => bail!(e),
        }
    }

    Ok(instructions)
}
*/

pub fn decode_bytes(bytes: &[u8], address: usize) -> CPUResult<Instruction> {
    let (opcode, arg_1, arg_2) = match address {
        o if o + 2 < bytes.len() => (bytes[o], bytes[o + 1], bytes[o + 2]),
        o if o + 1 < bytes.len() => (bytes[o], bytes[o + 1], 0),
        o if o < bytes.len() => (bytes[o], 0, 0),
        _ => (0, 0, 0),
    };
    let mut instruction = decode(opcode)?;
    instruction.low = arg_1;
    instruction.high = arg_2;
    Ok(instruction)
}

pub fn decode(byte: u8) -> CPUResult<Instruction> {
    match OPCODES.get(&byte) {
        Some(i) => Ok(*i),
        None => bail!(CPUErrorKind::UnrecognizedOpcode(byte)),
    }
}

pub fn disassemble(instruction: &Instruction) -> String {
    let low = instruction.low;
    let high = instruction.high;

    use AddressMode::*;
    let arg_string = match instruction.mode {
        Absolute => format!("${:02X}{:02X}", high, low),
        AbsoluteX => format!("${:02X}{:02X},X", high, low),
        AbsoluteY => format!("${:02X}{:02X},Y", high, low),
        Accumulator => "A".to_owned(),
        Immediate => format!("$#{:02X}", low),
        Implicit => "".to_owned(),
        Indirect => format!("(${:02X}{:02X})", high, low),
        IndirectX => format!("(${:02X},X)", low),
        IndirectY => format!("(${:02X}),Y", low),
        Relative => format!("${:02X}", low),
        ZeroPage => format!("${:02X}", low),
        ZeroPageX => format!("${:02X},X", low),
        ZeroPageY => format!("${:02X},Y", low),
    };

    format!("{} {}", instruction.name, arg_string)
}

pub struct VM {
    pub cpu: CPU,
    pub memory: [u8; 0xFFFF],
    program_end: u16,
    pub error: Option<CPUError>,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            cpu: CPU::new(),
            memory: [0u8; 0xFFFF],
            error: None,
            program_end: 0,
        }
    }
}

impl VM {
    pub fn load_program(&mut self, bytes: &[u8], start: u16) {
        let end = min(bytes.len(), 0xFFFF);

        for i in 0..self.memory.len() {
            self.memory[i] = match i {
                i if i < end => bytes[i],
                _ => 0,
            };
        }

        self.program_end = end as u16;
        self.cpu.pc = start;
        self.cpu.pins.addr = start;
        self.cpu.pins.data = self.memory[start as usize];
    }

    fn bus_write(&mut self) {
        let pins = &mut self.cpu.pins;

        if pins.addr < self.memory.len() as u16 {
            self.memory[pins.addr as usize] = pins.data;
        }
    }

    fn bus_read(&mut self) {
        let pins = &self.cpu.pins;

        if pins.addr < self.memory.len() as u16 {
            self.cpu.pins.data = self.memory[self.cpu.pins.addr as usize];
        }
    }

    pub fn update(&mut self) {
        if self.error.is_some() {
            return;
        }

        match self.cpu.cycle() {
            Ok(_) => {
                match self.cpu.pins.rw {
                    true => self.bus_read(),
                    false => self.bus_write(),
                }
                if self.cpu.pins.sync {
                    self.bus_read();
                }
            }
            Err(e) => {
                error!("cpu error: {}", e);
                self.error = Some(e.into());
            }
        }
    }
}

#[cfg(test)]
#[path = "./cpu_tests.rs"]
mod cpu_tests;
