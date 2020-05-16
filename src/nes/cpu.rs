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
            arg1: 0,
            arg2: 0,
            bytes: $bytes,
            cycles: $cycles,
        }
    };
}

lazy_static! {
    static ref OPCODES: HashMap<u8, Instruction> = hashmap! {
        0x69 => instruction!(0x69, &"ADC", 2, 2, AddressMode::Immediate),
        0x65 => instruction!(0x65, &"ADC", 2, 3, AddressMode::ZeroPage),
        0x75 => instruction!(0x75, &"ADC", 2, 4, AddressMode::ZeroPageX),
        0x6D => instruction!(0x6D, &"ADC", 3, 4, AddressMode::Absolute),
        0x7D => instruction!(0x7D, &"ADC", 3, 4, AddressMode::AbsoluteX),
        0x79 => instruction!(0x79, &"ADC", 3, 4, AddressMode::AbsoluteY),
        0x61 => instruction!(0x61, &"ADC", 2, 6, AddressMode::IndirectX),
        0x71 => instruction!(0x71, &"ADC", 2, 5, AddressMode::IndirectY),
        0x29 => instruction!(0x29, &"AND", 2, 2, AddressMode::Immediate),
        0x25 => instruction!(0x25, &"AND", 2, 3, AddressMode::ZeroPage),
        0x35 => instruction!(0x35, &"AND", 2, 4, AddressMode::ZeroPageX),
        0x2D => instruction!(0x2D, &"AND", 3, 4, AddressMode::Absolute),
        0x3D => instruction!(0x3D, &"AND", 3, 4, AddressMode::AbsoluteX),
        0x39 => instruction!(0x39, &"AND", 3, 4, AddressMode::AbsoluteY),
        0x21 => instruction!(0x21, &"AND", 2, 6, AddressMode::IndirectX),
        0x31 => instruction!(0x31, &"AND", 2, 5, AddressMode::IndirectY),
        0x0A => instruction!(0x0A, &"ASL", 1, 2, AddressMode::Accumulator),
        0x06 => instruction!(0x06, &"ASL", 2, 5, AddressMode::ZeroPage),
        0x16 => instruction!(0x16, &"ASL", 2, 6, AddressMode::ZeroPageX),
        0x0E => instruction!(0x0E, &"ASL", 3, 6, AddressMode::Absolute),
        0x1E => instruction!(0x1E, &"ASL", 3, 7, AddressMode::AbsoluteX),
        0x24 => instruction!(0x24, &"BIT", 2, 3, AddressMode::ZeroPageX),
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
        0xDD => instruction!(0xDD, &"CMP", 3, 4, AddressMode::AbsoluteX),
        0xD9 => instruction!(0xD9, &"CMP", 3, 4, AddressMode::AbsoluteY),
        0xC1 => instruction!(0xC1, &"CMP", 2, 6, AddressMode::IndirectX),
        0xD1 => instruction!(0xD1, &"CMP", 2, 5, AddressMode::IndirectY),
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
        0x5D => instruction!(0x5D, &"EOR", 3, 4, AddressMode::AbsoluteX),
        0x59 => instruction!(0x59, &"EOR", 3, 4, AddressMode::AbsoluteY),
        0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
        0x51 => instruction!(0x51, &"EOR", 2, 5, AddressMode::IndirectY),
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
        0xB1 => instruction!(0xB1, &"LDA", 2, 5, AddressMode::IndirectY),
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
        0x1D => instruction!(0x1D, &"ORA", 3, 4, AddressMode::AbsoluteX),
        0x19 => instruction!(0x19, &"ORA", 3, 4, AddressMode::AbsoluteY),
        0x01 => instruction!(0x01, &"ORA", 2, 6, AddressMode::IndirectX),
        0x11 => instruction!(0x11, &"ORA", 2, 5, AddressMode::IndirectY),
        0xAA => instruction!(0xAA, &"TAX", 1, 2, AddressMode::Implicit),
        0x8A => instruction!(0x8A, &"TXA", 1, 2, AddressMode::Implicit),
        0xCA => instruction!(0xCA, &"DEX", 1, 2, AddressMode::Implicit),
        0xE8 => instruction!(0xE8, &"INX", 1, 2, AddressMode::Implicit),
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
        0xFD => instruction!(0xFD, &"SBC", 3, 4, AddressMode::AbsoluteX),
        0xF9 => instruction!(0xF9, &"SBC", 3, 4, AddressMode::AbsoluteY),
        0xE1 => instruction!(0xE1, &"SBC", 2, 6, AddressMode::IndirectX),
        0xF1 => instruction!(0xF1, &"SBC", 2, 4, AddressMode::IndirectY),
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
        0x96 => instruction!(0x96, &"STX", 2, 4, AddressMode::ZeroPageX),
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
    pub arg1: u8,
    pub arg2: u8,
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
                2 => format!(" {}", self.arg1),
                3 => format!(" {}, {}", self.arg1, self.arg2),
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
    addr_write: bool,
    pc_write: bool,
    addr_buf: [u8; 2],
    pub current_addr: u16,
    pub current: Instruction,
}

impl CPU {
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
            addr_write: false,
            pc_write: false,
            addr_buf: [0, 0],
            current: decode(0xEA).unwrap(), // TODO
            current_addr: 0,
        }
    }

    fn fetch_data(&mut self) -> CPUResult<()> {
        match self.t {
            0 => {
                self.current_addr = self.pins.addr;
                self.addr_buf = [0, 0];
                let decoded = decode(self.pins.data)?;
                trace!("decoded @ {:#04X}: {}", self.pins.addr, decoded);
                self.current = decoded;
                Ok(())
            }
            1 => {
                self.current.arg1 = self.pins.data;
                Ok(()) // TODO
            }
            2 => {
                self.current.arg2 = self.pins.data;
                Ok(()) // TODO
            }
            _ => Ok(()), // TODO
        }
    }

    pub fn cycle(&mut self) -> CPUResult<()> {
        trace!(
            "begin cycle {} [t{}->], pins: {}, ir: {}",
            self.cycles,
            self.t,
            self.pins,
            self.current
        );

        self.fetch_data()?; // TODO
        self.pins.rw = true;
        self.addr_write = false;
        self.pc_write = false;

        let result = match self.t {
            0 => {
                self.t += 1;
                self.pc += 1;
                self.set_addr16(self.pc);
                Ok(())
            }
            1..=6 => {
                let mut result = match self.current.opcode {

                    // In-memory ops
                    0xA9 /* LDA #imm */ => Some(self.cycle_memop(CPU::set_a)),
                    0xA5 /* LDA $zp */ => Some(self.cycle_memop(CPU::set_a)),
                    0xB5 /* LDA $zp, x */ => Some(self.cycle_memop(CPU::set_a)),
                    0xAD /* LDA $abs */ => Some(self.cycle_memop(CPU::set_a)),
                    0xBD /* LDA $abs, x */ => Some(self.cycle_memop(CPU::set_a)),
                    0xB9 /* LDA $abs, y */ => Some(self.cycle_memop(CPU::set_a)),
                    // 0xA1 /* LDA $(ind, x) */ => unimplemented!(),
                    // 0xB1 /* LDA $(ind), y */ => unimplemented!(),
                    0xA2 /* LDX #imm */ => Some(self.cycle_memop(CPU::set_x)),
                    0xA6 /* LDX $zp */ => Some(self.cycle_memop(CPU::set_x)),
                    0xB6 /* LDX $zp, y */ => Some(self.cycle_memop(CPU::set_x)),
                    0xAE /* LDX $abs */ => Some(self.cycle_memop(CPU::set_x)),
                    0xBE /* LDX $abs, y */ => Some(self.cycle_memop(CPU::set_x)),
                    0xA0 /* LDY #imm */ => Some(self.cycle_memop(CPU::set_y)),
                    0xA4 /* LDY $zp */ => Some(self.cycle_memop(CPU::set_y)),
                    0xB4 /* LDY $zp, x */ => Some(self.cycle_memop(CPU::set_y)),
                    0xAC /* LDY $abs */ => Some(self.cycle_memop(CPU::set_y)),
                    0xBC /* LDY $abs, y */ => Some(self.cycle_memop(CPU::set_y)),

                    0xC9 /* CMP #imm */ => Some(self.cycle_memop(CPU::compare_a)),
                    // 0xC5 /* CMP $zp */ => unimplemented!(),
                    // 0xD5 /* CMP $zp, x */ => unimplemented!(),
                    0xCD /* CMP $abs */ => Some(self.cycle_memop(CPU::compare_a)),
                    // 0xDD /* CMP $abs, x */ => unimplemented!(),
                    // 0xD9 /* CMP $abs, y */ => unimplemented!(),
                    // 0xC1 /* CMP $(ind, x) */ => unimplemented!(),
                    // 0xD1 /* CMP $(ind), y */ => unimplemented!(),
                    0xE0 /* CPX #imm */ => Some(self.cycle_memop(CPU::compare_x)),
                    // 0xE4 /* CPX $zp */ => unimplemented!(),
                    0xEC /* CPX $abs */ => Some(self.cycle_memop(CPU::compare_x)),
                    0xC0 /* CPY #imm */ => Some(self.cycle_memop(CPU::compare_y)),
                    // 0xC4 /* CPY $zp */ => unimplemented!(),
                    0xCC /* CPY $abs */ => Some(self.cycle_memop(CPU::compare_y)),
                    _ => None,
                };
                if result.is_none() {
                    result = Some(match self.current.opcode {
                        0x69 => self.cycle_math(),
                        0x65 => self.cycle_math(),
                        0x75 => self.cycle_math(),
                        0x6D => self.cycle_math(),
                        0x7D => self.cycle_math(),
                        0x79 => self.cycle_math(),
                        0x61 => self.cycle_math(),
                        0x71 => self.cycle_math(),
                        0x29 => self.cycle_bitwise(),
                        0x25 => self.cycle_bitwise(),
                        0x35 => self.cycle_bitwise(),
                        0x2D => self.cycle_bitwise(),
                        0x3D => self.cycle_bitwise(),
                        0x39 => self.cycle_bitwise(),
                        0x21 => self.cycle_bitwise(),
                        0x31 => self.cycle_bitwise(),
                        0x0A => self.cycle_rotate(),
                        0x06 => self.cycle_rotate(),
                        0x16 => self.cycle_rotate(),
                        0x0E => self.cycle_rotate(),
                        0x1E => self.cycle_rotate(),
                        0x24 => self.cycle_bitwise(),
                        0x2C => self.cycle_bitwise(),
                        0x10 => self.cycle_branch(),
                        0x30 => self.cycle_branch(),
                        0x50 => self.cycle_branch(),
                        0x70 => self.cycle_branch(),
                        0x90 => self.cycle_branch(),
                        0xB0 => self.cycle_branch(),
                        0xD0 => self.cycle_branch(),
                        0xF0 => self.cycle_branch(),
                        0x00 => self.cycle_unimplemented(),
                        0xC6 => self.cycle_inc_dec(),
                        0xD6 => self.cycle_inc_dec(),
                        0xCE => self.cycle_inc_dec(),
                        0xDE => self.cycle_inc_dec(),
                        0x49 => self.cycle_bitwise(),
                        0x45 => self.cycle_bitwise(),
                        0x55 => self.cycle_bitwise(),
                        0x4D => self.cycle_bitwise(),
                        0x5D => self.cycle_bitwise(),
                        0x59 => self.cycle_bitwise(),
                        0x41 => self.cycle_bitwise(),
                        0x51 => self.cycle_bitwise(),
                        0x18 => self.cycle_single_byte(),
                        0x38 => self.cycle_single_byte(),
                        0x58 => self.cycle_single_byte(),
                        0x78 => self.cycle_single_byte(),
                        0xB8 => self.cycle_single_byte(),
                        0xD8 => self.cycle_single_byte(),
                        0xF8 => self.cycle_single_byte(),
                        0xE6 => self.cycle_inc_dec(),
                        0xF6 => self.cycle_inc_dec(),
                        0xEE => self.cycle_inc_dec(),
                        0xFE => self.cycle_inc_dec(),
                        0x4C => self.cycle_jump(),
                        0x6C => self.cycle_jump(),
                        0x20 => self.cycle_jump(),
                        0x4A => self.cycle_rotate(),
                        0x46 => self.cycle_rotate(),
                        0x56 => self.cycle_rotate(),
                        0x4E => self.cycle_rotate(),
                        0x5E => self.cycle_rotate(),
                        0xEA => self.cycle_single_byte(),
                        0x09 => self.cycle_bitwise(),
                        0x05 => self.cycle_bitwise(),
                        0x15 => self.cycle_bitwise(),
                        0x0D => self.cycle_bitwise(),
                        0x1D => self.cycle_bitwise(),
                        0x19 => self.cycle_bitwise(),
                        0x01 => self.cycle_bitwise(),
                        0x11 => self.cycle_bitwise(),
                        0xAA => self.cycle_single_byte(),
                        0x8A => self.cycle_single_byte(),
                        0xCA => self.cycle_inc_dec(),
                        0xE8 => self.cycle_inc_dec(),
                        0xA8 => self.cycle_single_byte(),
                        0x98 => self.cycle_single_byte(),
                        0x88 => self.cycle_inc_dec(),
                        0xC8 => self.cycle_inc_dec(),
                        0x2A => self.cycle_rotate(),
                        0x26 => self.cycle_rotate(),
                        0x36 => self.cycle_rotate(),
                        0x2E => self.cycle_rotate(),
                        0x3E => self.cycle_rotate(),
                        0x6A => self.cycle_rotate(),
                        0x66 => self.cycle_rotate(),
                        0x76 => self.cycle_rotate(),
                        0x6E => self.cycle_rotate(),
                        0x7E => self.cycle_rotate(),
                        0x40 => self.cycle_unimplemented(),
                        0x60 => self.cycle_unimplemented(),
                        0xE9 => self.cycle_math(),
                        0xE5 => self.cycle_math(),
                        0xF5 => self.cycle_math(),
                        0xED => self.cycle_math(),
                        0xFD => self.cycle_math(),
                        0xF9 => self.cycle_math(),
                        0xE1 => self.cycle_math(),
                        0xF1 => self.cycle_math(),
                        0x85 => self.cycle_store(),
                        0x95 => self.cycle_store(),
                        0x8D => self.cycle_store(),
                        0x9D => self.cycle_store(),
                        0x99 => self.cycle_store(),
                        0x81 => self.cycle_store(),
                        0x91 => self.cycle_store(),
                        0x9A => self.cycle_single_byte(),
                        0xBA => self.cycle_single_byte(),
                        0x48 => self.cycle_stack(),
                        0x68 => self.cycle_stack(),
                        0x08 => self.cycle_stack(),
                        0x28 => self.cycle_stack(),
                        0x86 => self.cycle_store(),
                        0x96 => self.cycle_store(),
                        0x8E => self.cycle_store(),
                        0x84 => self.cycle_store(),
                        0x94 => self.cycle_store(),
                        0x8C => self.cycle_store(),
                        // 0xEA => Ok(()),
                        // 0x18 | 0x38 | 0x58 | 0x78 | 0xB8 | 0xD8 | 0xF8 | 0xAA | 0xA8 | 0xBA | 0x8A
                        // | 0x9A | 0x98 => self.cycle_single_byte(),
                        // 0xA0 | 0xA1 | 0xA2 | 0xA4 | 0xA5 | 0xA9 | 0xA6 | 0xAC | 0xAD | 0xAE | 0xB1
                        // | 0xB4 | 0xB5 | 0xB6 | 0xB9 | 0xBC | 0xBD | 0xBE => self.cycle_load(),
                        // 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 | 0x86 | 0x96 | 0x8E | 0x84 | 0x94
                        // | 0x8C => self.cycle_store(),
                        // 0x4C | 0x6C => self.cycle_jump(),
                        // 0xD0 | 0xF0 => self.cycle_branch(),
                        // 0xCA | 0x88 => self.cycle_inc_dec(),
                        // 0xC9 => self.cycle_compare(),
                        _ => bail!(CPUErrorKind::NotImplemented(self.current)),
                    });
                };

                if self.t < self.current.bytes {
                    if !self.pc_write {
                        self.pc += 1;
                    }
                    if !self.addr_write {
                        self.set_addr16(self.pc);
                    }
                }

                match self.current.cycles {
                    c if c - 1 == self.t => {
                        self.t = 0;
                        if self.pins.rw {
                            self.set_addr16(self.pc);
                        }
                    }
                    _ => self.t += 1,
                }

                result.unwrap()
            }
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        };

        trace!(
            "end cycle {} [->t{}], pins: {}, ir: {}",
            self.cycles,
            self.t,
            self.pins,
            self.current
        );
        self.cycles += 1;

        result
    }

    fn cycle_unimplemented(&mut self) -> CPUResult<()> {
        bail!(CPUErrorKind::NotImplemented(self.current))
    }

    fn cycle_rotate(&mut self) -> CPUResult<()> {
        // 0x2A => instruction!(0x2A, &"ROL", 1, 2, AddressMode::Accumulator),
        // 0x26 => instruction!(0x26, &"ROL", 2, 5, AddressMode::ZeroPage),
        // 0x36 => instruction!(0x36, &"ROL", 2, 6, AddressMode::ZeroPageX),
        // 0x2E => instruction!(0x2E, &"ROL", 3, 6, AddressMode::Absolute),
        // 0x3E => instruction!(0x3E, &"ROL", 3, 7, AddressMode::AbsoluteX),
        // 0x6A => instruction!(0x6A, &"ROR", 1, 2, AddressMode::Accumulator),
        // 0x66 => instruction!(0x66, &"ROR", 2, 5, AddressMode::ZeroPage),
        // 0x76 => instruction!(0x76, &"ROR", 2, 6, AddressMode::ZeroPageX),
        // 0x6E => instruction!(0x6E, &"ROR", 3, 6, AddressMode::Absolute),
        // 0x7E => instruction!(0x7E, &"ROR", 3, 7, AddressMode::AbsoluteX),
        let inst = self.current;
        let byte = self.pins.data;

        // match self.current.opcode {
        //     // ROL #imm
        //     0x2A => match self.t {
        //         1 => {
        //             let carry = self.a & 0x80 == 0x80;
        //             self.a = (self.a << 1) & self.flags.carry as u8;
        //             self.flags.carry = carry;
        //             self.flags.zero = self.a != 0;
        //             self.flags.negative = self.a & 0x80 == 0x80;
        //         }
        //         _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        //     },
        //     // ROL $zp, x
        //     0x36 => match self.t {
        //         1 => self.set_addr8(byte),
        //         2 => {
        //             let addr = (Wrapping(self.pins.addr as u8) + Wrapping(self.x)).0;
        //             self.set_addr8(byte);
        //         }
        //         3 => self.set_addr8(byte + self.x),
        //     },
        //     _ => bail!(CPUErrorKind::InstructionExecution(inst)),
        // }

        Ok(())
    }

    fn cycle_stack(&mut self) -> CPUResult<()> {
        let inst = self.current;
        let byte = self.pins.data;

        match self.current.opcode {
            // PHA
            0x48 => match self.t {
                1 => (),
                2 => {
                    self.write_data(self.s, 0x01, self.a);
                    self.s = (Wrapping(self.s) - Wrapping(1)).0;
                }
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // PLA
            0x68 => match self.t {
                1 => (),
                2 => {
                    self.s = (Wrapping(self.s) + Wrapping(1)).0;
                    self.set_addr(self.s, 0x01)
                }
                3 => {
                    self.a = byte;
                    self.flags.zero = self.a == 0;
                    self.flags.negative = self.a & 0x80 == 0x80
                }
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // PHP
            0x08 => match self.t {
                1 => (),
                2 => {
                    self.write_data(self.s, 0x01, self.flags.into());
                    self.s = (Wrapping(self.s) - Wrapping(1)).0;
                }
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },

            // PLP
            0x28 => match self.t {
                1 => (),
                2 => {
                    self.s = (Wrapping(self.s) + Wrapping(1)).0;
                    self.set_addr(self.s, 0x01)
                }
                3 => self.flags = Flags::from(byte),

                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }

        Ok(())
    }

    fn cycle_bitwise(&mut self) -> CPUResult<()> {
        // 0x49 => instruction!(0x49, &"EOR", 2, 2, AddressMode::Immediate),
        // 0x45 => instruction!(0x45, &"EOR", 2, 3, AddressMode::ZeroPage),
        // 0x55 => instruction!(0x55, &"EOR", 2, 4, AddressMode::ZeroPageX),
        // 0x4D => instruction!(0x4D, &"EOR", 3, 4, AddressMode::Absolute),
        // 0x5D => instruction!(0x5D, &"EOR", 3, 4, AddressMode::AbsoluteX),
        // 0x59 => instruction!(0x59, &"EOR", 3, 4, AddressMode::AbsoluteY),
        // 0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
        // 0x51 => instruction!(0x51, &"EOR", 2, 5, AddressMode::IndirectY),
        // 0x09 => instruction!(0x09, &"ORA", 2, 2, AddressMode::Immediate),
        // 0x05 => instruction!(0x05, &"ORA", 2, 3, AddressMode::ZeroPage),
        // 0x15 => instruction!(0x15, &"ORA", 2, 4, AddressMode::ZeroPageX),
        // 0x0D => instruction!(0x0D, &"ORA", 3, 4, AddressMode::Absolute),
        // 0x1D => instruction!(0x1D, &"ORA", 3, 4, AddressMode::AbsoluteX),
        // 0x19 => instruction!(0x19, &"ORA", 3, 4, AddressMode::AbsoluteY),
        // 0x01 => instruction!(0x01, &"ORA", 2, 6, AddressMode::IndirectX),
        // 0x11 => instruction!(0x11, &"ORA", 2, 5, AddressMode::IndirectY),
        let inst = self.current;
        let byte = self.pins.data;
        match self.current.opcode {
            // ADC #im
            0x49 => match self.t {
                1 => self.xor(byte),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }

        Ok(())
    }

    fn cycle_math(&mut self) -> CPUResult<()> {
        let inst = self.current;
        let byte = self.pins.data;

        // 0x65 => instruction!(0x65, &"ADC", 2, 3, AddressMode::ZeroPage),
        // 0x75 => instruction!(0x75, &"ADC", 2, 4, AddressMode::ZeroPageX),
        // 0x6D => instruction!(0x6D, &"ADC", 3, 4, AddressMode::Absolute),
        // 0x7D => instruction!(0x7D, &"ADC", 3, 4, AddressMode::AbsoluteX),
        // 0x79 => instruction!(0x79, &"ADC", 3, 4, AddressMode::AbsoluteY),
        // 0x61 => instruction!(0x61, &"ADC", 2, 6, AddressMode::IndirectX),
        // 0x71 => instruction!(0x71, &"ADC", 2, 5, AddressMode::IndirectY),
        // 0xE9 => instruction!(0xE9, &"SBC", 2, 2, AddressMode::Immediate),
        // 0xE5 => instruction!(0xE5, &"SBC", 2, 3, AddressMode::ZeroPage),
        // 0xF5 => instruction!(0xF5, &"SBC", 2, 4, AddressMode::ZeroPageX),
        // 0xED => instruction!(0xED, &"SBC", 3, 4, AddressMode::Absolute),
        // 0xFD => instruction!(0xFD, &"SBC", 3, 4, AddressMode::AbsoluteX),
        // 0xF9 => instruction!(0xF9, &"SBC", 3, 4, AddressMode::AbsoluteY),
        // 0xE1 => instruction!(0xE1, &"SBC", 2, 6, AddressMode::IndirectX),
        // 0xF1 => instruction!(0xF1, &"SBC", 2, 4, AddressMode::IndirectY),

        match self.current.opcode {
            // ADC #im
            0x69 => match self.t {
                1 => self.add_a_carry(byte),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }

        Ok(())
    }

    fn cycle_branch(&mut self) -> CPUResult<()> {
        let inst = self.current;
        // 0x50 => instruction!(0x50, &"BVC", 2, 2, AddressMode::Relative),
        // 0x70 => instruction!(0x70, &"BVS", 2, 2, AddressMode::Relative),
        // 0xB0 => instruction!(0xB0, &"BCS", 2, 2, AddressMode::Relative),

        match self.t {
            1 => match inst.opcode {
                // BNE
                0xD0 => {
                    if self.flags.zero {
                        self.current.cycles = 2
                    }
                }
                // BEQ
                0xF0 => {
                    if !self.flags.zero {
                        self.current.cycles = 2
                    }
                }
                // BPL
                0x10 => {
                    if self.flags.negative {
                        self.current.cycles = 2
                    }
                }
                // BCC
                0x90 => {
                    if self.flags.carry {
                        self.current.cycles = 2
                    }
                }
                // BMI
                0x30 => {
                    if !self.flags.negative {
                        self.current.cycles = 2
                    }
                }
                // BCS
                0xB0 => {
                    if !self.flags.carry {
                        self.current.cycles = 2
                    }
                }
                // BVC
                0x50 => {
                    if self.flags.overflow {
                        self.current.cycles = 2
                    }
                }
                // BVS
                0x70 => {
                    if !self.flags.overflow {
                        self.current.cycles = 2
                    }
                }
                _ => bail!(CPUErrorKind::InstructionExecution(inst)),
            },
            2 => {
                let pc_low = (self.pc & 0xFF) as i32;
                let offset = self.current.arg1 as i8 as i32;
                let relative = pc_low + offset;

                if relative > 0 && relative < 0xFF {
                    let addr = ((self.pc as i32) + offset) as u16;
                    self.set_pc16(addr);
                    self.current.cycles = 3;
                }
            }
            3 => {
                let offset = self.current.arg1 as i8 as i32;
                let addr = ((self.pc as i32) + offset) as u16;
                self.set_pc16(addr);
            }
            _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        }

        Ok(())
    }

    fn cycle_jump(&mut self) -> CPUResult<()> {
        let inst = self.current;
        // 0x4C => instruction!(0x4C, &"JMP", 3, 3, AddressMode::Absolute),
        // 0x6C => instruction!(0x6C, &"JMP", 3, 5, AddressMode::Indirect),

        match self.current.opcode {
            // STA $abs
            0x4C => match self.t {
                1 => (),
                2 => self.set_pc(inst.arg1, inst.arg2),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(inst)),
        }

        Ok(())
    }

    fn cycle_single_byte(&mut self) -> CPUResult<()> {
        let op = self.current.opcode;

        match self.t {
            1 => {
                match op {
                    0xEA => (),                             // NOP
                    0x18 => self.flags.carry = false,       // CLC
                    0x38 => self.flags.carry = true,        // SEC
                    0x58 => self.flags.irq_disable = false, // CLI
                    0x78 => self.flags.irq_disable = true,  // SEI
                    0xB8 => self.flags.negative = false,    // CLV
                    0xD8 => self.flags.decimal = false,     // CLD
                    0xF8 => self.flags.decimal = true,      // SED
                    // TAX
                    0xAA => {
                        self.set_x(self.a);
                        self.flags.zero = self.x == 0;
                        self.flags.negative = self.x & 0x80 == 0x80
                    }
                    // TAY
                    0xA8 => {
                        self.set_y(self.a);
                        self.flags.zero = self.y == 0;
                        self.flags.negative = self.y & 0x80 == 0x80
                    }
                    // TSX
                    0xBA => {
                        self.set_x(self.s);
                        self.flags.zero = self.x == 0;
                        self.flags.negative = self.x & 0x80 == 0x80
                    }
                    // TXA
                    0x8A => {
                        self.set_a(self.x);
                        self.flags.zero = self.a == 0;
                        self.flags.negative = self.a & 0x80 == 0x80
                    }
                    // TXS
                    0x9A => self.set_s(self.x),
                    // TYA
                    0x98 => {
                        self.set_a(self.y);
                        self.flags.zero = self.a == 0;
                        self.flags.negative = self.a & 0x80 == 0x80
                    }
                    _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
                }
            }
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        }

        Ok(())
    }

    fn cycle_inc_dec(&mut self) -> CPUResult<()> {
        match self.t {
            1 => match self.current.opcode {
                // DEX
                0xCA => {
                    self.x = match self.x {
                        0 => 0xFF,
                        _ => self.x - 1,
                    };
                    self.flags.zero = self.x == 0;
                    self.flags.negative = self.x & 0x80 == 0x80;
                }
                // DEY
                0x88 => {
                    self.y = match self.y {
                        0 => 0xFF,
                        _ => self.y - 1,
                    };
                    self.flags.zero = self.y == 0;
                    self.flags.negative = self.y & 0x80 == 0x80;
                }
                _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
            },
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        }

        Ok(())
    }

    fn cycle_store(&mut self) -> CPUResult<()> {
        let inst = self.current;
        // 0x85 => instruction!(byte, &"STA", 2, 3, ZeroPage),
        // 0x95 => instruction!(byte, &"STA", 2, 4, ZeroPageX),
        // 0x8D => instruction!(byte, &"STA", 3, 4, Absolute),
        // 0x9D => instruction!(byte, &"STA", 3, 5, AbsoluteX),
        // 0x99 => instruction!(byte, &"STA", 3, 5, AbsoluteY),
        // 0x81 => instruction!(byte, &"STA", 2, 6, IndirectX),
        // 0x91 => instruction!(byte, &"STA", 2, 6, IndirectY),
        // 0x86 => instruction!(byte, &"STX", 2, 3, ZeroPage),
        // 0x96 => instruction!(byte, &"STX", 2, 4, ZeroPageX),
        // 0x8E => instruction!(byte, &"STX", 3, 4, Absolute),
        // 0x84 => instruction!(byte, &"STY", 2, 3, ZeroPage),
        // 0x94 => instruction!(byte, &"STY", 2, 4, ZeroPageX),
        // 0x8C => instruction!(byte, &"STY", 3, 4, Absolute),
        match self.current.opcode {
            // STA $abs
            0x8D => match self.t {
                1 => (),
                2 => (),
                3 => self.write_data(inst.arg1, inst.arg2, self.a),
                _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }

        Ok(())
    }

    fn cycle_memop(&mut self, action: fn(&mut CPU, u8)) -> CPUResult<()> {
        // 0x69 => instruction!(0x69, &"ADC", 2, 2, AddressMode::Immediate),
        // 0x65 => instruction!(0x65, &"ADC", 2, 3, AddressMode::ZeroPage),
        // 0x75 => instruction!(0x75, &"ADC", 2, 4, AddressMode::ZeroPageX),
        // 0x6D => instruction!(0x6D, &"ADC", 3, 4, AddressMode::Absolute),
        // 0x7D => instruction!(0x7D, &"ADC", 3, 4, AddressMode::AbsoluteX),
        // 0x79 => instruction!(0x79, &"ADC", 3, 4, AddressMode::AbsoluteY),
        // 0x61 => instruction!(0x61, &"ADC", 2, 6, AddressMode::IndirectX),
        // 0x71 => instruction!(0x71, &"ADC", 2, 5, AddressMode::IndirectY),
        // 0x29 => instruction!(0x29, &"AND", 2, 2, AddressMode::Immediate),
        // 0x25 => instruction!(0x25, &"AND", 2, 3, AddressMode::ZeroPage),
        // 0x35 => instruction!(0x35, &"AND", 2, 4, AddressMode::ZeroPageX),
        // 0x2D => instruction!(0x2D, &"AND", 3, 4, AddressMode::Absolute),
        // 0x3D => instruction!(0x3D, &"AND", 3, 4, AddressMode::AbsoluteX),
        // 0x39 => instruction!(0x39, &"AND", 3, 4, AddressMode::AbsoluteY),
        // 0x21 => instruction!(0x21, &"AND", 2, 6, AddressMode::IndirectX),
        // 0x31 => instruction!(0x31, &"AND", 2, 5, AddressMode::IndirectY),
        // 0x24 => instruction!(0x24, &"BIT", 2, 3, AddressMode::ZeroPageX),
        // 0x2C => instruction!(0x2C, &"BIT", 3, 4, AddressMode::Absolute),
        // 0xC9 => instruction!(0xC9, &"CMP", 2, 2, AddressMode::Immediate),
        // 0xC5 => instruction!(0xC5, &"CMP", 2, 3, AddressMode::ZeroPage),
        // 0xD5 => instruction!(0xD5, &"CMP", 2, 4, AddressMode::ZeroPageX),
        // 0xCD => instruction!(0xCD, &"CMP", 3, 4, AddressMode::Absolute),
        // 0xDD => instruction!(0xDD, &"CMP", 3, 4, AddressMode::AbsoluteX),
        // 0xD9 => instruction!(0xD9, &"CMP", 3, 4, AddressMode::AbsoluteY),
        // 0xC1 => instruction!(0xC1, &"CMP", 2, 6, AddressMode::IndirectX),
        // 0xD1 => instruction!(0xD1, &"CMP", 2, 5, AddressMode::IndirectY),
        // 0xE0 => instruction!(0xE0, &"CPX", 2, 2, AddressMode::Immediate),
        // 0xE4 => instruction!(0xE4, &"CPX", 2, 3, AddressMode::ZeroPage),
        // 0xEC => instruction!(0xEC, &"CPX", 3, 4, AddressMode::Absolute),
        // 0xC0 => instruction!(0xC0, &"CPY", 2, 2, AddressMode::Immediate),
        // 0xC4 => instruction!(0xC4, &"CPY", 2, 3, AddressMode::ZeroPage),
        // 0xCC => instruction!(0xCC, &"CPY", 3, 4, AddressMode::Absolute),
        // 0x49 => instruction!(0x49, &"EOR", 2, 2, AddressMode::Immediate),
        // 0x45 => instruction!(0x45, &"EOR", 2, 3, AddressMode::ZeroPage),
        // 0x55 => instruction!(0x55, &"EOR", 2, 4, AddressMode::ZeroPageX),
        // 0x4D => instruction!(0x4D, &"EOR", 3, 4, AddressMode::Absolute),
        // 0x5D => instruction!(0x5D, &"EOR", 3, 4, AddressMode::AbsoluteX),
        // 0x59 => instruction!(0x59, &"EOR", 3, 4, AddressMode::AbsoluteY),
        // 0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
        // 0x51 => instruction!(0x51, &"EOR", 2, 5, AddressMode::IndirectY),
        // 0xA9 => instruction!(0xA9, &"LDA", 2, 2, AddressMode::Immediate),
        // 0xA5 => instruction!(0xA5, &"LDA", 2, 3, AddressMode::ZeroPage),
        // 0xB5 => instruction!(0xB5, &"LDA", 2, 4, AddressMode::ZeroPageX),
        // 0xAD => instruction!(0xAD, &"LDA", 3, 4, AddressMode::Absolute),
        // 0xBD => instruction!(0xBD, &"LDA", 3, 4, AddressMode::AbsoluteX),
        // 0xB9 => instruction!(0xB9, &"LDA", 3, 4, AddressMode::AbsoluteY),
        // 0xA1 => instruction!(0xA1, &"LDA", 2, 6, AddressMode::IndirectX),
        // 0xB1 => instruction!(0xB1, &"LDA", 2, 5, AddressMode::IndirectY),
        // 0xA2 => instruction!(0xA2, &"LDX", 2, 2, AddressMode::Immediate),
        // 0xA6 => instruction!(0xA6, &"LDX", 2, 3, AddressMode::ZeroPage),
        // 0xB6 => instruction!(0xB6, &"LDX", 2, 4, AddressMode::ZeroPageY),
        // 0xAE => instruction!(0xAE, &"LDX", 3, 4, AddressMode::Absolute),
        // 0xBE => instruction!(0xBE, &"LDX", 3, 4, AddressMode::AbsoluteY),
        // 0xA0 => instruction!(0xA0, &"LDY", 2, 2, AddressMode::Immediate),
        // 0xA4 => instruction!(0xA4, &"LDY", 2, 3, AddressMode::ZeroPage),
        // 0xB4 => instruction!(0xB4, &"LDY", 2, 4, AddressMode::ZeroPageY),
        // 0xAC => instruction!(0xAC, &"LDY", 3, 4, AddressMode::Absolute),
        // 0xBC => instruction!(0xBC, &"LDY", 3, 4, AddressMode::AbsoluteY),
        // 0x09 => instruction!(0x09, &"ORA", 2, 2, AddressMode::Immediate),
        // 0x05 => instruction!(0x05, &"ORA", 2, 3, AddressMode::ZeroPage),
        // 0x15 => instruction!(0x15, &"ORA", 2, 4, AddressMode::ZeroPageX),
        // 0x0D => instruction!(0x0D, &"ORA", 3, 4, AddressMode::Absolute),
        // 0x1D => instruction!(0x1D, &"ORA", 3, 4, AddressMode::AbsoluteX),
        // 0x19 => instruction!(0x19, &"ORA", 3, 4, AddressMode::AbsoluteY),
        // 0x01 => instruction!(0x01, &"ORA", 2, 6, AddressMode::IndirectX),
        // 0x11 => instruction!(0x11, &"ORA", 2, 5, AddressMode::IndirectY),
        // 0xE9 => instruction!(0xE9, &"SBC", 2, 2, AddressMode::Immediate),
        // 0xE5 => instruction!(0xE5, &"SBC", 2, 3, AddressMode::ZeroPage),
        // 0xF5 => instruction!(0xF5, &"SBC", 2, 4, AddressMode::ZeroPageX),
        // 0xED => instruction!(0xED, &"SBC", 3, 4, AddressMode::Absolute),
        // 0xFD => instruction!(0xFD, &"SBC", 3, 4, AddressMode::AbsoluteX),
        // 0xF9 => instruction!(0xF9, &"SBC", 3, 4, AddressMode::AbsoluteY),
        // 0xE1 => instruction!(0xE1, &"SBC", 2, 6, AddressMode::IndirectX),
        // 0xF1 => instruction!(0xF1, &"SBC", 2, 4, AddressMode::IndirectY),

        let data = self.pins.data;
        let addr = self.pins.addr;
        let inst = self.current;
        let mode = self.current.mode;
        println!("mode {:?}", mode);

        match inst.mode {
            AddressMode::Immediate => match self.t {
                1 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPage => match self.t {
                1 => self.set_addr8(data),
                2 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::Absolute => match self.t {
                1 => self.addr_buf[0] = data,
                2 => self.set_addr(self.addr_buf[0], data),
                3 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPageX => match self.t {
                1 => self.set_addr8(data),
                2 => self.set_addr8((Wrapping(addr as u8) + Wrapping(self.x)).0),
                3 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::ZeroPageY => match self.t {
                1 => self.set_addr8(data),
                2 => self.set_addr8((Wrapping(addr as u8) + Wrapping(self.y)).0),
                3 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::IndirectX => match self.t {
                1 => self.set_addr8(data),
                2 => self.set_addr8((Wrapping(addr as u8) + Wrapping(self.x)).0),
                3 => {
                    self.addr_buf[0] = data;
                    self.set_addr16(addr + 1);
                }
                4 => self.set_addr(self.addr_buf[0], data),
                5 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::IndirectY => match self.t {
                1 => self.set_addr8(data),
                2 => {
                    self.addr_buf[0] = data;
                    self.set_addr16(addr + 1);
                }
                3 => {
                    let low = (Wrapping(addr as u8) + Wrapping(self.y)).0;
                    let carry = low < self.y;
                    let high = self.addr_buf[0] + (carry as u8);
                    self.set_addr(low, high);
                    if !carry {
                        self.current.cycles -= 1;
                    }
                }
                4 => {
                    if inst.cycles == 5 {
                        action(self, data);
                    }
                }
                5 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::AbsoluteX => match self.t {
                1 => self.addr_buf[0] = data,
                2 => {
                    let low = (Wrapping(self.addr_buf[0]) + Wrapping(self.x)).0;
                    let carry = low < self.x;
                    let high = data + (carry as u8);
                    self.set_addr(low, high);
                    if !carry {
                        self.current.cycles -= 1;
                    }
                }
                3 => {
                    if inst.cycles == 4 {
                        action(self, data);
                    }
                }
                4 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            AddressMode::AbsoluteY => match self.t {
                1 => self.addr_buf[0] = data,
                2 => {
                    let low = (Wrapping(self.addr_buf[0]) + Wrapping(self.y)).0;
                    let carry = low < self.y;
                    let high = data + (carry as u8);
                    self.set_addr(low, high);
                    if !carry {
                        self.current.cycles -= 1;
                    }
                }
                3 => {
                    if inst.cycles == 4 {
                        action(self, data);
                    }
                }
                4 => action(self, data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }
        Ok(())
    }

    fn xor(&mut self, byte: u8) {
        self.a = self.a ^ byte;
        self.flags.zero = self.a == 0;
        self.flags.negative = self.a & 0x80 == 0x80;
    }

    fn add_a_carry(&mut self, byte: u8) {
        let sum = self.a as u16 + byte as u16 + self.flags.carry as u16;
        self.flags.carry = sum > 0xFF;
        let sum = sum as u8;
        self.flags.zero = sum == 0;
        self.flags.overflow = (!(self.a ^ byte) & (self.a ^ sum) & 0x80) != 0;
        self.flags.negative = sum & 0x80 == 0x80;
        self.a = sum;
    }

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

    fn write_data(&mut self, low: u8, high: u8, data: u8) {
        self.pins.addr = (low as u16) | ((high as u16) << 8);
        self.pins.data = data;
        self.pins.rw = false;
        self.addr_write = true;
    }

    fn set_pc(&mut self, low: u8, high: u8) {
        self.pc = (low as u16) | ((high as u16) << 8);
        self.pins.addr = self.pc;
        self.pc_write = true;
    }

    fn set_pc16(&mut self, pc: u16) {
        self.pc = pc;
        self.pins.addr = self.pc;
        self.pc_write = true;
    }

    fn set_addr8(&mut self, byte: u8) {
        self.pins.addr = byte as u16;
        self.addr_write = true;
    }

    fn set_addr(&mut self, low: u8, high: u8) {
        self.pins.addr = (low as u16) | ((high as u16) << 8);
        self.addr_write = true;
    }

    fn set_addr16(&mut self, bytes: u16) {
        self.pins.addr = bytes;
        self.addr_write = true;
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
    instruction.arg1 = arg_1;
    instruction.arg2 = arg_2;
    Ok(instruction)
}

pub fn decode(byte: u8) -> CPUResult<Instruction> {
    match OPCODES.get(&byte) {
        Some(i) => Ok(*i),
        None => bail!(CPUErrorKind::UnrecognizedOpcode(byte)),
    }
}

pub fn disassemble(instruction: &Instruction) -> String {
    let arg1 = instruction.arg1;
    let arg2 = instruction.arg2;

    use AddressMode::*;
    let arg_string = match instruction.mode {
        Absolute => format!("${:02X}{:02X}", arg2, arg1),
        AbsoluteX => format!("${:02X}{:02X},X", arg2, arg1),
        AbsoluteY => format!("${:02X}{:02X},Y", arg2, arg1),
        Accumulator => "A".to_owned(),
        Immediate => format!("$#{:02X}", arg1),
        Implicit => "".to_owned(),
        Indirect => format!("(${:02X}{:02X})", arg2, arg1),
        IndirectX => format!("(${:02X},X)", arg1),
        IndirectY => format!("(${:02X}),Y", arg1),
        Relative => format!("${:02X}", arg1),
        ZeroPage => format!("${:02X}", arg1),
        ZeroPageX => format!("${:02X},X", arg1),
        ZeroPageY => format!("${:02X},Y", arg1),
    };

    format!("{} {}", instruction.name, arg_string)
}

pub struct VM {
    pub cpu: CPU,
    pub memory: [u8; 4096],
    program_end: u16,
    error: Option<CPUError>,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            cpu: CPU::new(),
            memory: [0u8; 4096],
            error: None,
            program_end: 0,
        }
    }
}

impl VM {
    pub fn load_program(&mut self, bytes: &[u8], start: u16) {
        let end = min(bytes.len(), 0xC00);

        for i in 0..self.memory.len() {
            self.memory[i] = match i {
                i if i < end => bytes[i],
                _ => 0,
            };
        }

        self.program_end = end as u16;
        self.cpu.pc = start;
        self.cpu.pins.data = self.memory[start as usize];
    }

    fn bus_write(&mut self) {
        let mut pins = &mut self.cpu.pins;

        if pins.addr < self.memory.len() as u16 {
            self.memory[pins.addr as usize] = pins.data;
            if self.cpu.pc < self.program_end {
                pins.addr = self.cpu.pc;
                pins.data = self.memory[self.cpu.pc as usize];
            }
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
            Ok(_) => match self.cpu.pins.rw {
                true => self.bus_read(),
                false => self.bus_write(),
            },
            Err(e) => {
                error!("cpu error: {}", e);
                self.error = Some(e.into());
            }
        }
    }
}

#[allow(unused_imports)]
#[allow(dead_code)]
#[allow(unused_macros)]
mod test {
    use super::*;

    type CycleCallbacks = HashMap<usize, Box<dyn Fn(&mut VM)>>;

    fn run_test(
        rom: &[u8],
        init: impl Fn(&mut VM),
        exit: impl Fn(&mut VM),
        pre: CycleCallbacks,
        post: CycleCallbacks,
    ) {
        let mut vm = VM::default();
        vm.load_program(rom, 0);
        init(&mut vm);

        loop {
            if let Some(callback) = pre.get(&(vm.cpu.cycles as usize)) {
                callback(&mut vm);
            }

            vm.update();
            if let Some(e) = vm.error {
                panic!("error: {}", e.description());
            }

            if let Some(callback) = post.get(&(vm.cpu.cycles as usize)) {
                callback(&mut vm);
            }
            if vm.cpu.pc >= vm.program_end && vm.cpu.t == 0 {
                break;
            }
        }

        exit(&mut vm);
    }

    macro_rules! cpu_test {
        (@builder $m:expr, $i:expr, $e:expr, pre:{$($k1:expr => $v1:expr),*$(,)*}, post:{$($k2:expr => $v2:expr),*$(,)*}) => {{
            #[allow(unused_mut)]
            let mut pre_map: CycleCallbacks = HashMap::new();
            $(pre_map.insert($k1, Box::new($v1));)*
            #[allow(unused_mut)]
            let mut post_map: CycleCallbacks = HashMap::new();
            $(post_map.insert($k2, Box::new($v2));)*
            run_test($m, $i, $e, pre_map, post_map)
        }};
        ($m:expr, init:$init:expr, post:$post:tt, exit:$exit:expr) => {
            cpu_test!(@builder $m, $init, $exit, pre:{}, post:$post)
        };
        ($m:expr, pre:$pre:tt, post:$post:tt) => {
            cpu_test!(@builder $m, |_| {}, |_| {}, pre:$pre, post:$post)
        };
        ($m:expr, init:$init:expr, exit:$exit:expr) => {
            cpu_test!(@builder $m, $init, $exit, pre:{}, post:{})
        };
        ($m:expr, $exit:expr) => {
            cpu_test!(@builder $m, |_| {}, $exit, pre:{}, post:{})
        };
        ($m:expr) => {
            cpu_test!(@builder $m, |_| {}, |_| {}, pre:{}, post:{})
        };
    }

    macro_rules! branch_test {
        ($opcode:expr, branch:$branch:expr, nobranch:$nobranch:expr) => {{
            let memory = [$opcode, 2, 0xA9, 2, 0xEA];
            let exit = |vm: &mut VM| assert_eq!(vm.cpu.a, 1);
            let init = |vm: &mut VM| {
                vm.cpu.a = 1;
                $branch(vm);
            };
            let mut post: CycleCallbacks = HashMap::new();
            post.insert(3, Box::new(|vm: &mut VM| assert_eq!(vm.cpu.pc, 4)));
            run_test(&memory, init, exit, HashMap::new(), post);

            let exit = |vm: &mut VM| assert_eq!(vm.cpu.a, 2);
            let init = |vm: &mut VM| {
                vm.cpu.a = 2;
                $nobranch(vm);
            };
            let mut post: CycleCallbacks = HashMap::new();
            post.insert(2, Box::new(|vm: &mut VM| assert_eq!(vm.cpu.pc, 2)));
            run_test(&memory, init, exit, HashMap::new(), post);

            let nops: &[u8] = &[0xEA; 250];
            let ops: &[u8] = &[$opcode, 4, 0xA9, 2];
            let memory = &[nops, ops].concat();
            let exit = |vm: &mut VM| assert_eq!(vm.cpu.a, 1);
            let init = |vm: &mut VM| {
                vm.cpu.a = 1;
                vm.cpu.pc = 250;
                vm.cpu.pins.data = vm.memory[vm.cpu.pc as usize];
                $branch(vm);
            };
            let mut post: CycleCallbacks = HashMap::new();
            post.insert(4, Box::new(|vm: &mut VM| assert_eq!(vm.cpu.pc, 0x100)));
            run_test(&memory, init, exit, HashMap::new(), post);
        }};
    }

    #[test]
    fn test_ld_imm() {
        macro_rules! ld_imm_test {
            ($op:expr, $field:ident) => {{
                cpu_test!(&[$op, 1], |vm: &mut VM| assert_eq!(vm.cpu.$field, 1));
                cpu_test!(&[$op, 0],
                    init: |vm| vm.cpu.$field = 1,
                    exit: |vm| {
                        assert_eq!(vm.cpu.$field, 0);
                        assert_eq!(vm.cpu.flags, Flags { zero: true, ..Flags::default() });
                });
                cpu_test!(&[$op, 0x81], |vm| {
                    assert_eq!(vm.cpu.$field, 0x81);
                    assert_eq!(
                        vm.cpu.flags,
                        Flags {
                            negative: true,
                            ..Flags::default()
                        }
                    );
                });
            }};
        }

        ld_imm_test!(0xA9, a); // LDA #imm
        ld_imm_test!(0xA2, x); // LDX #imm
        ld_imm_test!(0xA0, y); // LDY #imm
    }

    #[test]
    fn test_ld_zp() {
        macro_rules! ld_zp_test {
            ($op:expr, $field:ident) => {{
                cpu_test!(&[$op, 0xF1],
                    init: |vm: &mut VM| vm.memory[0xF1] = 1,
                    exit: |vm: &mut VM| assert_eq!(vm.cpu.$field, 1)
                );
            }};
        }

        ld_zp_test!(0xA5, a); // LDA $zp
        ld_zp_test!(0xA6, x); // LDX $zp
        ld_zp_test!(0xA4, y); // LDY $zp
    }

    #[test]
    fn test_ld_zpxy() {
        macro_rules! ld_zpxy_test {
            ($op:expr, $field:ident, $offset:ident) => {{
                cpu_test!(&[$op, 0xF1],
                    init: |vm: &mut VM| {
                        vm.memory[0xF2] = 1;
                        vm.cpu.$offset = 1;
                    },
                    exit: |vm: &mut VM| assert_eq!(vm.cpu.$field, 1)
                );
            }};
        }

        ld_zpxy_test!(0xB5, a, x);
        ld_zpxy_test!(0xB6, x, y);
        ld_zpxy_test!(0xB4, y, x);
    }

    #[test]
    fn test_ld_abs() {
        macro_rules! ld_abs_test {
            ($op:expr, $field:ident) => {{
                cpu_test!(&[$op, 0x01, 0x08],
                    init: |vm: &mut VM| vm.memory[0x801] = 1,
                    exit: |vm: &mut VM| assert_eq!(vm.cpu.$field, 1)
                );
            }};
        }

        ld_abs_test!(0xAD, a); // LDA $abs
        ld_abs_test!(0xAE, x); // LDX $abs
        ld_abs_test!(0xAC, y); // LDY $abs
    }

    #[test]
    fn test_ld_absxy() {
        macro_rules! ld_absxy_test {
            ($op:expr, $field:ident, $offset:ident) => {{
                cpu_test!(&[$op, 0x01, 0x08],
                    init: |vm: &mut VM| {
                        vm.memory[0x802] = 1;
                        vm.cpu.$offset = 1;
                    },
                    exit: |vm: &mut VM| assert_eq!(vm.cpu.$field, 1)
                );
            }};
        }

        ld_absxy_test!(0xBD, a, x); // LDA $abs, x
        ld_absxy_test!(0xB9, a, y); // LDA $abs, y
        ld_absxy_test!(0xBE, x, y); // LDX $abs, y
        ld_absxy_test!(0xBC, y, x); // LDY $abs, x
    }

    #[test]
    fn test_flags() {
        // TODO 0xB8 CLN
        cpu_test!(&[0xF8, 0x78, 0x38, 0xD8, 0x58, 0x18],
            pre: {
                0 => |vm| assert!(!vm.cpu.flags.decimal),
                2 => |vm| assert!(!vm.cpu.flags.irq_disable),
                4 => |vm| assert!(!vm.cpu.flags.carry),
            },
            post: {
                2 => |vm| assert!(vm.cpu.flags.decimal),
                4 => |vm| assert!(vm.cpu.flags.irq_disable),
                6 => |vm| assert!(vm.cpu.flags.carry),
                8 => |vm| assert!(!vm.cpu.flags.decimal),
                10 => |vm| assert!(!vm.cpu.flags.irq_disable),
                12 => |vm| assert!(!vm.cpu.flags.carry),
            }
        );
    }

    #[test]
    fn test_transfer() {
        // TAX
        cpu_test!(&[0xAA],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0xAA],
            init: |vm| vm.cpu.a = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0xEE);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[0xAA],
            init: |vm| {
                vm.cpu.a = 0;
                vm.cpu.x = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );

        // TAY
        cpu_test!(&[0xA8],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.y, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0xA8],
            init: |vm| vm.cpu.a = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.y, 0xEE);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[0xA8],
            init: |vm| {
                vm.cpu.a = 0;
                vm.cpu.y = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.y, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );

        // TSX
        cpu_test!(&[0xBA],
            init: |vm| vm.cpu.s = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0xBA],
            init: |vm| vm.cpu.s = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0xEE);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[0xBA],
            init: |vm| {
                vm.cpu.s = 0;
                vm.cpu.x = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );

        // TXA
        cpu_test!(&[0x8A],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0x8A],
            init: |vm| vm.cpu.x = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 0xEE);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[0x8A],
            init: |vm| {
                vm.cpu.x = 0;
                vm.cpu.a = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.a, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );

        // TXS
        cpu_test!(&[0x9A],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.s, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0x9A],
            init: |vm| vm.cpu.x = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xEE);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0x9A],
            init: |vm| {
                vm.cpu.x = 0;
                vm.cpu.s = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );

        // TYA
        cpu_test!(&[0x98],
            init: |vm| vm.cpu.y = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[0x98],
            init: |vm| vm.cpu.y = 0xEE,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 0xEE);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[0x98],
            init: |vm| {
                vm.cpu.y = 0;
                vm.cpu.a = 1;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.a, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );
    }

    #[test]
    fn test_store() {
        // STA $abs
        cpu_test!(&[0xA9, 1, 0x8D, 0x11, 0x0D, 0xA9, 2],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 2);
                assert_eq!(vm.memory[0x0D11], 1);
        });
    }

    #[test]
    fn test_jump() {
        // 0x6C => instruction!(0x6C, &"JMP", 3, 5, AddressMode::Indirect),
        // JMP $abs
        cpu_test!(&[0x4C, 5, 0, 0xA9, 2, 0xA9, 1], |vm| {
            assert_eq!(vm.cpu.a, 1);
            assert_eq!(vm.cpu.cycles, 5);
        });
    }

    #[test]
    fn test_branch() {
        // BNE $rel
        branch_test!(0xD0,
            branch: |vm: &mut VM| vm.cpu.flags.zero = false,
            nobranch: |vm: &mut VM| vm.cpu.flags.zero = true
        );
        // BEQ $rel
        branch_test!(0xF0,
            branch: |vm: &mut VM| vm.cpu.flags.zero = true,
            nobranch: |vm: &mut VM| vm.cpu.flags.zero = false
        );
        // BPL $rel
        branch_test!(0x10,
            branch: |vm: &mut VM| vm.cpu.flags.negative = false,
            nobranch: |vm: &mut VM| vm.cpu.flags.negative = true
        );
        // BVC $rel
        branch_test!(0x50,
            branch: |vm: &mut VM| vm.cpu.flags.overflow = false,
            nobranch: |vm: &mut VM| vm.cpu.flags.overflow = true
        );
        // BVS $rel
        branch_test!(0x70,
            branch: |vm: &mut VM| vm.cpu.flags.overflow = true,
            nobranch: |vm: &mut VM| vm.cpu.flags.overflow = false
        );
    }

    #[test]
    fn test_inc_dec() {
        // DEX
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 2,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 1);
                assert!(!vm.cpu.flags.zero);
                assert!(!vm.cpu.flags.negative);
        });
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0);
                assert!(vm.cpu.flags.zero);
                assert!(!vm.cpu.flags.negative);
        });
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 0,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0xFF);
                assert!(!vm.cpu.flags.zero);
                assert!(vm.cpu.flags.negative);
        });
    }

    #[test]
    fn test_compare() {
        // 0xC9 CMP #imm
        cpu_test!(&[0xC9, 1],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xC9, 1],
            init: |vm| vm.cpu.a = 2,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xC9, 2],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );

        // 0xC0 CPY #imm
        cpu_test!(&[0xC0, 1],
            init: |vm| vm.cpu.y = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xC0, 1],
            init: |vm| vm.cpu.y = 2,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xC0, 2],
            init: |vm| vm.cpu.y = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );

        // 0xE0 CPX $imm
        cpu_test!(&[0xE0, 1],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xE0, 1],
            init: |vm| vm.cpu.x = 2,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xE0, 2],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );

        // 0xCD CMP $abs
        cpu_test!(&[0xCD, 0x00, 0x09],
            init: |vm| {vm.cpu.a = 1; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xCD, 0x00, 0x09],
            init: |vm| {vm.cpu.a = 2; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xCD, 0x00, 0x09],
            init: |vm| {vm.cpu.a = 1; vm.memory[0x900] = 2;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );

        // 0xEC CPX $imm
        cpu_test!(&[0xEC, 0x00, 0x09],
            init: |vm| {vm.cpu.x = 1; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xEC, 0x00, 0x09],
            init: |vm| {vm.cpu.x = 2; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xEC, 0x00, 0x09],
            init: |vm| {vm.cpu.x = 1; vm.memory[0x900] = 2;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );

        // 0xEC CPY $imm
        cpu_test!(&[0xCC, 0x00, 0x09],
            init: |vm| {vm.cpu.y = 1; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xCC, 0x00, 0x09],
            init: |vm| {vm.cpu.y = 2; vm.memory[0x900] = 1;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()})
        );
        cpu_test!(&[0xCC, 0x00, 0x09],
            init: |vm| {vm.cpu.y = 1; vm.memory[0x900] = 2;},
            exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()})
        );
    }

    #[test]
    fn test_math() {
        // 0x65 => instruction!(0x65, &"ADC", 2, 3, AddressMode::ZeroPage),
        // 0x75 => instruction!(0x75, &"ADC", 2, 4, AddressMode::ZeroPageX),
        // 0x6D => instruction!(0x6D, &"ADC", 3, 4, AddressMode::Absolute),
        // 0x7D => instruction!(0x7D, &"ADC", 3, 4, AddressMode::AbsoluteX),
        // 0x79 => instruction!(0x79, &"ADC", 3, 4, AddressMode::AbsoluteY),
        // 0x61 => instruction!(0x61, &"ADC", 2, 6, AddressMode::IndirectX),
        // 0x71 => instruction!(0x71, &"ADC", 2, 5, AddressMode::IndirectY),
        // 0xE9 => instruction!(0xE9, &"SBC", 2, 2, AddressMode::Immediate),
        // 0xE5 => instruction!(0xE5, &"SBC", 2, 3, AddressMode::ZeroPage),
        // 0xF5 => instruction!(0xF5, &"SBC", 2, 4, AddressMode::ZeroPageX),
        // 0xED => instruction!(0xED, &"SBC", 3, 4, AddressMode::Absolute),
        // 0xFD => instruction!(0xFD, &"SBC", 3, 4, AddressMode::AbsoluteX),
        // 0xF9 => instruction!(0xF9, &"SBC", 3, 4, AddressMode::AbsoluteY),
        // 0xE1 => instruction!(0xE1, &"SBC", 2, 6, AddressMode::IndirectX),
        // 0xF1 => instruction!(0xF1, &"SBC", 2, 4, AddressMode::IndirectY),
        // 0x69 ADC $imm
        cpu_test!(&[0x69, 1],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert!(!vm.cpu.flags.zero);
                assert!(!vm.cpu.flags.negative);
                assert!(!vm.cpu.flags.overflow);
                assert!(!vm.cpu.flags.carry);
                assert_eq!(vm.cpu.a, 2);
        });
        // 0x69 ADC $imm
        cpu_test!(&[0x69, 1],
            init: |vm| vm.cpu.a = 0xFF,
            exit: |vm| {
                assert!(vm.cpu.flags.zero);
                assert!(vm.cpu.flags.carry);
                assert!(!vm.cpu.flags.overflow);
                assert!(!vm.cpu.flags.negative);
                assert_eq!(vm.cpu.a, 0);
        });
        // 0x69 ADC $imm
        cpu_test!(&[0x69, 1],
            init: |vm| vm.cpu.a = 0x7F,
            exit: |vm| {
                assert!(vm.cpu.flags.overflow);
                assert!(vm.cpu.flags.negative);
                assert!(!vm.cpu.flags.zero);
                assert!(!vm.cpu.flags.carry);
                assert_eq!(vm.cpu.a, 0x80);
        });
    }

    #[test]
    fn test_bitwise() {
        // 0x45 => instruction!(0x45, &"EOR", 2, 3, AddressMode::ZeroPage),
        // 0x55 => instruction!(0x55, &"EOR", 2, 4, AddressMode::ZeroPageX),
        // 0x4D => instruction!(0x4D, &"EOR", 3, 4, AddressMode::Absolute),
        // 0x5D => instruction!(0x5D, &"EOR", 3, 4, AddressMode::AbsoluteX),
        // 0x59 => instruction!(0x59, &"EOR", 3, 4, AddressMode::AbsoluteY),
        // 0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
        // 0x51 => instruction!(0x51, &"EOR", 2, 5, AddressMode::IndirectY),
        // 0x09 => instruction!(0x09, &"ORA", 2, 2, AddressMode::Immediate),
        // 0x05 => instruction!(0x05, &"ORA", 2, 3, AddressMode::ZeroPage),
        // 0x15 => instruction!(0x15, &"ORA", 2, 4, AddressMode::ZeroPageX),
        // 0x0D => instruction!(0x0D, &"ORA", 3, 4, AddressMode::Absolute),
        // 0x1D => instruction!(0x1D, &"ORA", 3, 4, AddressMode::AbsoluteX),
        // 0x19 => instruction!(0x19, &"ORA", 3, 4, AddressMode::AbsoluteY),
        // 0x01 => instruction!(0x01, &"ORA", 2, 6, AddressMode::IndirectX),
        // 0x11 => instruction!(0x11, &"ORA", 2, 5, AddressMode::IndirectY),

        // 0x49 EOR $imm
        cpu_test!(&[0x49, 0b01010101],
            init: |vm| vm.cpu.a = 0b10101010,
            exit: |vm| {
                assert!(!vm.cpu.flags.zero);
                assert!(vm.cpu.flags.negative);
                assert!(!vm.cpu.flags.overflow);
                assert!(!vm.cpu.flags.carry);
                assert_eq!(vm.cpu.a, 0b11111111);
        });
        // 0x49 EOR $imm
        cpu_test!(&[0x49, 0b10101010],
            init: |vm| vm.cpu.a = 0b10101010,
            exit: |vm| {
                assert!(vm.cpu.flags.zero);
                assert!(!vm.cpu.flags.negative);
                assert!(!vm.cpu.flags.overflow);
                assert!(!vm.cpu.flags.carry);
                assert_eq!(vm.cpu.a, 0b00000000);
        });
    }

    #[test]
    fn test_stack() {
        // 0x48 PHA
        cpu_test!(&[0x48, 0x48],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xFD);
                assert_eq!(vm.memory[0x01FF], 1);
                assert_eq!(vm.memory[0x01FE], 1);
        });
        // 0x68 PLA
        cpu_test!(&[0x68, 0x68],
            init: |vm| {
                vm.cpu.s = 0xFD;
                vm.memory[0x01FF] = 0xEF;
                vm.memory[0x01FE] = 0xEE;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xFF);
                assert_eq!(vm.cpu.a, 0xEF);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
        });
        // 0x68 PLA
        cpu_test!(&[0x68],
            init: |vm| {
                vm.cpu.s = 0xFE;
                vm.memory[0x01FF] = 0;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xFF);
                assert_eq!(vm.cpu.a, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
        });

        // 0x08 PHP
        cpu_test!(&[0x08],
            init: |vm| {
                vm.cpu.flags.carry= true;
                vm.cpu.flags.zero= true;
                vm.cpu.flags.irq_disable= true;
                vm.cpu.flags.decimal= true;
                vm.cpu.flags.overflow= true;
                vm.cpu.flags.negative= true;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xFE);
                assert_eq!(vm.memory[0x01FF], 0b11111111);
        });

        // PLP
        cpu_test!(&[0x28],
            init: |vm| {
                vm.cpu.s = 0xFE;
                vm.memory[0x01FF] = 0b11001111;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.s, 0xFF);
                assert_eq!(vm.cpu.flags, Flags {
                    carry: true,
                    zero: true,
                    irq_disable: true,
                    decimal: true,
                    overflow: true,
                    negative: true,
                });
        });
    }
}
