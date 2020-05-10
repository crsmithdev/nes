use std::cmp::min;
use std::collections::HashMap;
use std::fmt;

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
        0xC0 => instruction!(0xC0, &"CPY", 2, 2, AddressMode::ZeroPage),
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
        0xBD => instruction!(0xBD, &"LDA", 3, 4, AddressMode::AbsoluteX),
        0xB9 => instruction!(0xB9, &"LDA", 3, 4, AddressMode::AbsoluteY),
        0xA1 => instruction!(0xA1, &"LDA", 2, 6, AddressMode::IndirectX),
        0xB1 => instruction!(0xB1, &"LDA", 2, 5, AddressMode::IndirectY),
        0xA2 => instruction!(0xA2, &"LDX", 2, 2, AddressMode::Immediate),
        0xA6 => instruction!(0xA6, &"LDX", 2, 3, AddressMode::ZeroPage),
        0xB6 => instruction!(0xB6, &"LDX", 2, 4, AddressMode::ZeroPageY),
        0xAE => instruction!(0xAE, &"LDX", 3, 4, AddressMode::Absolute),
        0xBE => instruction!(0xBE, &"LDX", 3, 4, AddressMode::AbsoluteY),
        0xA0 => instruction!(0xA0, &"LDY", 2, 2, AddressMode::Immediate),
        0xA4 => instruction!(0xA4, &"LDY", 2, 3, AddressMode::ZeroPage),
        0xB4 => instruction!(0xB4, &"LDY", 2, 4, AddressMode::ZeroPageY),
        0xAC => instruction!(0xAC, &"LDY", 3, 4, AddressMode::Absolute),
        0xBC => instruction!(0xBC, &"LDY", 3, 4, AddressMode::AbsoluteY),
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

#[derive(Debug, Default)]
pub struct Flags {
    pub c: bool,
    pub z: bool,
    pub i: bool,
    pub d: bool,
    pub v: bool,
    pub n: bool,
}

#[allow(dead_code)]
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
    pub current: Instruction,
}

impl CPU {
    pub fn new() -> Self {
        Self {
            pc: 0,
            s: 0,
            a: 0,
            x: 0,
            y: 0,
            t: 0,
            flags: Flags::default(),
            pins: Pins::startup(),
            cycles: 0,
            addr_write: false,
            pc_write: false,
            current: decode(0xEA).unwrap(), // TODO
        }
    }

    fn fetch_data(&mut self) -> CPUResult<()> {
        match self.t {
            0 => {
                let decoded = decode(self.pins.data)?;
                debug!("decoded @ {:#04X}: {}", self.pins.addr, decoded);
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
        debug!(
            "begin cycle {} [t{}->], pins: {}, ir: {}",
            self.cycles, self.t, self.pins, self.current
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
                let result = match self.current.opcode {
                    0x69 => self.cycle_unimplemented(),
                    0x65 => self.cycle_unimplemented(),
                    0x75 => self.cycle_unimplemented(),
                    0x6D => self.cycle_unimplemented(),
                    0x7D => self.cycle_unimplemented(),
                    0x79 => self.cycle_unimplemented(),
                    0x61 => self.cycle_unimplemented(),
                    0x71 => self.cycle_unimplemented(),
                    0x29 => self.cycle_unimplemented(),
                    0x25 => self.cycle_unimplemented(),
                    0x35 => self.cycle_unimplemented(),
                    0x2D => self.cycle_unimplemented(),
                    0x3D => self.cycle_unimplemented(),
                    0x39 => self.cycle_unimplemented(),
                    0x21 => self.cycle_unimplemented(),
                    0x31 => self.cycle_unimplemented(),
                    0x0A => self.cycle_unimplemented(),
                    0x06 => self.cycle_unimplemented(),
                    0x16 => self.cycle_unimplemented(),
                    0x0E => self.cycle_unimplemented(),
                    0x1E => self.cycle_unimplemented(),
                    0x24 => self.cycle_unimplemented(),
                    0x2C => self.cycle_unimplemented(),
                    0x10 => self.cycle_branch_op(),
                    0x30 => self.cycle_branch_op(),
                    0x50 => self.cycle_branch_op(),
                    0x70 => self.cycle_branch_op(),
                    0x90 => self.cycle_branch_op(),
                    0xB0 => self.cycle_branch_op(),
                    0xD0 => self.cycle_branch_op(),
                    0xF0 => self.cycle_branch_op(),
                    0x00 => self.cycle_unimplemented(),
                    0xC9 => self.cycle_compare_op(),
                    0xC5 => self.cycle_compare_op(),
                    0xD5 => self.cycle_compare_op(),
                    0xCD => self.cycle_compare_op(),
                    0xDD => self.cycle_compare_op(),
                    0xD9 => self.cycle_compare_op(),
                    0xC1 => self.cycle_compare_op(),
                    0xD1 => self.cycle_compare_op(),
                    0xE0 => self.cycle_compare_op(),
                    0xE4 => self.cycle_compare_op(),
                    0xEC => self.cycle_compare_op(),
                    0xC0 => self.cycle_compare_op(),
                    0xC4 => self.cycle_compare_op(),
                    0xCC => self.cycle_compare_op(),
                    0xC6 => self.cycle_inc_dec_op(),
                    0xD6 => self.cycle_inc_dec_op(),
                    0xCE => self.cycle_inc_dec_op(),
                    0xDE => self.cycle_inc_dec_op(),
                    0x49 => self.cycle_unimplemented(),
                    0x45 => self.cycle_unimplemented(),
                    0x55 => self.cycle_unimplemented(),
                    0x4D => self.cycle_unimplemented(),
                    0x5D => self.cycle_unimplemented(),
                    0x59 => self.cycle_unimplemented(),
                    0x41 => self.cycle_unimplemented(),
                    0x51 => self.cycle_unimplemented(),
                    0x18 => self.cycle_single_byte_op(),
                    0x38 => self.cycle_single_byte_op(),
                    0x58 => self.cycle_single_byte_op(),
                    0x78 => self.cycle_single_byte_op(),
                    0xB8 => self.cycle_single_byte_op(),
                    0xD8 => self.cycle_single_byte_op(),
                    0xF8 => self.cycle_single_byte_op(),
                    0xE6 => self.cycle_inc_dec_op(),
                    0xF6 => self.cycle_inc_dec_op(),
                    0xEE => self.cycle_inc_dec_op(),
                    0xFE => self.cycle_inc_dec_op(),
                    0x4C => self.cycle_jump_op(),
                    0x6C => self.cycle_jump_op(),
                    0x20 => self.cycle_unimplemented(),
                    0xA9 => self.cycle_load_op(),
                    0xA5 => self.cycle_load_op(),
                    0xB5 => self.cycle_load_op(),
                    0xAD => self.cycle_load_op(),
                    0xBD => self.cycle_load_op(),
                    0xB9 => self.cycle_load_op(),
                    0xA1 => self.cycle_load_op(),
                    0xB1 => self.cycle_load_op(),
                    0xA2 => self.cycle_load_op(),
                    0xA6 => self.cycle_load_op(),
                    0xB6 => self.cycle_load_op(),
                    0xAE => self.cycle_load_op(),
                    0xBE => self.cycle_load_op(),
                    0xA0 => self.cycle_load_op(),
                    0xA4 => self.cycle_load_op(),
                    0xB4 => self.cycle_load_op(),
                    0xAC => self.cycle_load_op(),
                    0xBC => self.cycle_load_op(),
                    0x4A => self.cycle_unimplemented(),
                    0x46 => self.cycle_unimplemented(),
                    0x56 => self.cycle_unimplemented(),
                    0x4E => self.cycle_unimplemented(),
                    0x5E => self.cycle_unimplemented(),
                    0xEA => self.cycle_nop(),
                    0x09 => self.cycle_unimplemented(),
                    0x05 => self.cycle_unimplemented(),
                    0x15 => self.cycle_unimplemented(),
                    0x0D => self.cycle_unimplemented(),
                    0x1D => self.cycle_unimplemented(),
                    0x19 => self.cycle_unimplemented(),
                    0x01 => self.cycle_unimplemented(),
                    0x11 => self.cycle_unimplemented(),
                    0xAA => self.cycle_single_byte_op(),
                    0x8A => self.cycle_single_byte_op(),
                    0xCA => self.cycle_inc_dec_op(),
                    0xE8 => self.cycle_unimplemented(),
                    0xA8 => self.cycle_single_byte_op(),
                    0x98 => self.cycle_single_byte_op(),
                    0x88 => self.cycle_inc_dec_op(),
                    0xC8 => self.cycle_unimplemented(),
                    0x2A => self.cycle_unimplemented(),
                    0x26 => self.cycle_unimplemented(),
                    0x36 => self.cycle_unimplemented(),
                    0x2E => self.cycle_unimplemented(),
                    0x3E => self.cycle_unimplemented(),
                    0x6A => self.cycle_unimplemented(),
                    0x66 => self.cycle_unimplemented(),
                    0x76 => self.cycle_unimplemented(),
                    0x6E => self.cycle_unimplemented(),
                    0x7E => self.cycle_unimplemented(),
                    0x40 => self.cycle_unimplemented(),
                    0x60 => self.cycle_unimplemented(),
                    0xE9 => self.cycle_unimplemented(),
                    0xE5 => self.cycle_unimplemented(),
                    0xF5 => self.cycle_unimplemented(),
                    0xED => self.cycle_unimplemented(),
                    0xFD => self.cycle_unimplemented(),
                    0xF9 => self.cycle_unimplemented(),
                    0xE1 => self.cycle_unimplemented(),
                    0xF1 => self.cycle_unimplemented(),
                    0x85 => self.cycle_store_op(),
                    0x95 => self.cycle_store_op(),
                    0x8D => self.cycle_store_op(),
                    0x9D => self.cycle_store_op(),
                    0x99 => self.cycle_store_op(),
                    0x81 => self.cycle_store_op(),
                    0x91 => self.cycle_store_op(),
                    0x9A => self.cycle_single_byte_op(),
                    0xBA => self.cycle_unimplemented(),
                    0x48 => self.cycle_unimplemented(),
                    0x68 => self.cycle_unimplemented(),
                    0x08 => self.cycle_unimplemented(),
                    0x28 => self.cycle_unimplemented(),
                    0x86 => self.cycle_store_op(),
                    0x96 => self.cycle_store_op(),
                    0x8E => self.cycle_store_op(),
                    0x84 => self.cycle_store_op(),
                    0x94 => self.cycle_store_op(),
                    0x8C => self.cycle_store_op(),
                    // 0xEA => Ok(()),
                    // 0x18 | 0x38 | 0x58 | 0x78 | 0xB8 | 0xD8 | 0xF8 | 0xAA | 0xA8 | 0xBA | 0x8A
                    // | 0x9A | 0x98 => self.cycle_single_byte_op(),
                    // 0xA0 | 0xA1 | 0xA2 | 0xA4 | 0xA5 | 0xA9 | 0xA6 | 0xAC | 0xAD | 0xAE | 0xB1
                    // | 0xB4 | 0xB5 | 0xB6 | 0xB9 | 0xBC | 0xBD | 0xBE => self.cycle_load_op(),
                    // 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 | 0x86 | 0x96 | 0x8E | 0x84 | 0x94
                    // | 0x8C => self.cycle_store_op(),
                    // 0x4C | 0x6C => self.cycle_jump_op(),
                    // 0xD0 | 0xF0 => self.cycle_branch_op(),
                    // 0xCA | 0x88 => self.cycle_inc_dec_op(),
                    // 0xC9 => self.cycle_compare_op(),
                    _ => bail!(CPUErrorKind::NotImplemented(self.current)),
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

                result
            }
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        };

        debug!(
            "end cycle {} [->t{}], pins: {}, ir: {}",
            self.cycles, self.t, self.pins, self.current
        );
        self.cycles += 1;

        result
    }

    fn cycle_unimplemented(&mut self) -> CPUResult<()> {
        bail!(CPUErrorKind::NotImplemented(self.current))
    }

    fn cycle_nop(&mut self) -> CPUResult<()> {
        Ok(())
    }

    fn cycle_compare_op(&mut self) -> CPUResult<()> {
        let inst = self.current;

        match self.current.opcode {
            // CMP #imm
            0xC9 => match self.t {
                1 => {
                    self.flags.c = self.a > inst.arg1;
                    self.flags.z = self.a == inst.arg1;
                    self.flags.n = self.a < inst.arg1;
                }
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            _ => bail!(CPUErrorKind::InstructionExecution(inst)),
        }

        Ok(())
    }

    fn cycle_branch_op(&mut self) -> CPUResult<()> {
        let inst = self.current;
        // 0x30 => instruction!(0x30, &"BMI", 2, 2, AddressMode::Relative),
        // 0x50 => instruction!(0x50, &"BVC", 2, 2, AddressMode::Relative),
        // 0x70 => instruction!(0x70, &"BVS", 2, 2, AddressMode::Relative),
        // 0x90 => instruction!(0x90, &"BCC", 2, 2, AddressMode::Relative),
        // 0xB0 => instruction!(0xB0, &"BCS", 2, 2, AddressMode::Relative),

        match self.t {
            1 => match inst.opcode {
                // BNE
                0xD0 => {
                    if self.flags.z {
                        self.current.cycles = 2
                    }
                }
                // BEQ
                0xF0 => {
                    if !self.flags.z {
                        self.current.cycles = 2
                    }
                }
                // BPL
                0x10 => {
                    if self.flags.n {
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

    fn cycle_jump_op(&mut self) -> CPUResult<()> {
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

    fn cycle_single_byte_op(&mut self) -> CPUResult<()> {
        let op = self.current.opcode;

        match self.t {
            1 => {
                match op {
                    0x18 => self.flags.c = false, // CLC
                    0x38 => self.flags.c = true,  // SEC
                    0x58 => self.flags.i = false, // CLI
                    0x78 => self.flags.i = true,  // SEI
                    0xB8 => self.flags.n = false, // CLV
                    0xD8 => self.flags.d = false, // CLD
                    0xF8 => self.flags.d = true,  // SED
                    0xAA => self.set_x(self.a),   // TAX
                    0xA8 => self.set_y(self.a),   // TAY
                    0xBA => self.set_x(self.s),   // TSX
                    0x8A => self.set_a(self.x),   // TXA
                    0x9A => self.set_s(self.x),   // TXS
                    0x98 => self.set_a(self.y),   // TYA
                    _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
                }
            }
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        }

        Ok(())
    }

    fn cycle_inc_dec_op(&mut self) -> CPUResult<()> {
        match self.t {
            1 => match self.current.opcode {
                // DEX
                0xCA => {
                    self.x = match self.x {
                        0 => 0xFF,
                        _ => self.x - 1,
                    };
                    self.flags.z = self.x == 0;
                    self.flags.n = self.x & 0x80 == 0x80;
                }
                // DEY
                0x88 => {
                    self.y = match self.y {
                        0 => 0xFF,
                        _ => self.y - 1,
                    };
                    self.flags.z = self.y == 0;
                    self.flags.n = self.y & 0x80 == 0x80;
                }
                _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
            },
            _ => bail!(CPUErrorKind::InstructionTiming(self.current, self.t)),
        }

        Ok(())
    }

    fn cycle_store_op(&mut self) -> CPUResult<()> {
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

    fn cycle_load_op(&mut self) -> CPUResult<()> {
        let inst = self.current;
        let byte = self.pins.data;

        match self.current.opcode {
            // LDA #im
            0xA9 => match self.t {
                1 => self.set_a(inst.arg1),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDX #im
            0xA2 => match self.t {
                1 => self.set_x(inst.arg1),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDY #im
            0xA0 => match self.t {
                1 => self.set_y(inst.arg1),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDA $zp
            0xA5 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_a(self.pins.data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDX $zp
            0xA6 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_x(self.pins.data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDY $zp
            0xA4 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_y(self.pins.data),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDA $abs
            0xAD => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_a(byte),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDX $abs
            0xAE => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_x(byte),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // LDY $abs
            0xAC => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_y(byte),
                _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            },
            // // LDA $zp,X
            // 0xB5 => match self.t {
            //     1 => self.set_addr8(self.pins.data),
            //     2 => self.set_addr8(self.x + self.current.arg1),
            //     3 => self.set_a(byte),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // // LDX $zp,Y
            // 0xB6 => match self.t {
            //     1 => self.set_addr8(self.pins.data),
            //     2 => self.set_addr8(self.y + self.current.arg1),
            //     3 => self.set_x(byte),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },
            // // LDY $zp,X
            // 0xB4 => match self.t {
            //     1 => self.set_addr8(self.pins.data),
            //     2 => self.set_addr8(self.x + self.current.arg1),
            //     3 => self.set_y(byte),
            //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
            // },

            // 0xBD => instruction!(byte, &"LDA", 3, AbsoluteX),
            // 0xBC => instruction!(byte, &"LDY", 3, AbsoluteX),
            // 0xB9 => instruction!(byte, &"LDA", 3, AbsoluteY),
            // 0xBE => instruction!(byte, &"LDX", 3, AbsoluteY),
            // 0xB9 => instruction!(byte, &"LDA", 3, AbsoluteY),
            // 0xA1 => instruction!(byte, &"LDA", 2, IndirectX),
            // 0xB1 => instruction!(byte, &"LDA", 2, IndirectY),
            _ => bail!(CPUErrorKind::InstructionExecution(self.current)),
        }

        Ok(())
    }

    fn set_a(&mut self, byte: u8) {
        self.a = byte;
        self.flags.z = byte == 0;
        self.flags.n = byte & 0x80 == 0x80;
    }

    fn set_x(&mut self, byte: u8) {
        self.x = byte;
        self.flags.z = byte == 0;
        self.flags.n = byte & 0x80 == 0x80;
    }

    fn set_y(&mut self, byte: u8) {
        self.y = byte;
        self.flags.z = byte == 0;
        self.flags.n = byte & 0x80 == 0x80;
    }

    fn set_s(&mut self, byte: u8) {
        self.s = byte;
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

    type VMCallback = Box<dyn Fn(&mut VM)>;
    type CycleCallbacks = HashMap<usize, Box<dyn Fn(&mut VM)>>;

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

    #[test]
    fn test_ld() {
        // LDA #imm, LDX #imm, LDY #imm
        cpu_test!(&[0xA9, 0x01, 0xA2, 0x02, 0xA0, 0x03], |vm| {
            assert_eq!(vm.cpu.a, 0x01);
            assert_eq!(vm.cpu.x, 0x02);
            assert_eq!(vm.cpu.y, 0x03);
        });

        // LDA $abs, LDX $abs, LDY %abs
        cpu_test!(&[0xAD, 221, 0, 0xAE, 222, 0, 0xAC, 223, 0],
            init: |vm| {
                vm.memory[221] = 1;
                vm.memory[222] = 2;
                vm.memory[223] = 3;
            },
            exit: |vm| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.x, 2);
                assert_eq!(vm.cpu.y, 3);
        });

        // // LDA $zp
        // 0xA5 => match self.t {
        //     1 => self.set_addr8(self.pins.data),
        //     2 => self.set_a(self.pins.data),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
        // // LDX $zp
        // 0xA6 => match self.t {
        //     1 => self.set_addr8(self.pins.data),
        //     2 => self.set_x(self.pins.data),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
        // // LDY $zp
        // 0xA4 => match self.t {
        //     1 => self.set_addr8(self.pins.data),
        //     2 => self.set_y(self.pins.data),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
        // // LDA $abs
        // 0xAD => match self.t {
        //     1 => (),
        //     2 => self.set_addr(self.current.arg1, byte),
        //     3 => self.set_a(byte),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
        // // LDX $abs
        // 0xAE => match self.t {
        //     1 => (),
        //     2 => self.set_addr(self.current.arg1, byte),
        //     3 => self.set_x(byte),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
        // // LDY $abs
        // 0xAC => match self.t {
        //     1 => (),
        //     2 => self.set_addr(self.current.arg1, byte),
        //     3 => self.set_y(byte),
        //     _ => bail!(CPUErrorKind::InstructionTiming(inst, self.t)),
        // },
    }

    #[test]
    fn test_ld_flags() {
        // LDA #imm, Z
        cpu_test!(&[0xA9, 0x00], |vm| {
            assert_eq!(vm.cpu.a, 0x00);
            assert!(vm.cpu.flags.z);
        });

        // LDX #imm, N
        cpu_test!(&[0xA2, 0xFE], |vm| {
            assert_eq!(vm.cpu.x, 0xFE);
            assert!(vm.cpu.flags.n);
        });
    }

    #[test]
    fn test_ld_zeropage() {
        // LDA $zp, LDX $zp, LDY $zp
        cpu_test!(&[0xA5, 3, 0xA6, 5, 0xA4, 1], |vm| {
            assert_eq!(vm.cpu.a, 5);
            assert_eq!(vm.cpu.x, 1);
            assert_eq!(vm.cpu.y, 3);
        });
    }

    #[test]
    fn test_flags() {
        // TODO 0xB8 CLN
        cpu_test!(&[0xF8, 0x78, 0x38, 0xD8, 0x58, 0x18],
            pre: {
                0 => |vm| assert!(!vm.cpu.flags.d),
                2 => |vm| assert!(!vm.cpu.flags.i),
                4 => |vm| assert!(!vm.cpu.flags.c),
            },
            post: {
                2 => |vm| assert!(vm.cpu.flags.d),
                4 => |vm| assert!(vm.cpu.flags.i),
                6 => |vm| assert!(vm.cpu.flags.c),
                8 => |vm| assert!(!vm.cpu.flags.d),
                10 => |vm| assert!(!vm.cpu.flags.i),
                12 => |vm| assert!(!vm.cpu.flags.c),
            }
        );
    }

    #[test]
    fn test_transfer() {
        // TODO 0xBA TSX

        cpu_test!(&[0xA9, 1, 0xAA], |vm| assert_eq!(vm.cpu.x, 1)); // TAX
        cpu_test!(&[0xA9, 1, 0xA8], |vm| assert_eq!(vm.cpu.y, 1)); // TAY
        cpu_test!(&[0xA2, 1, 0x8A], |vm| assert_eq!(vm.cpu.a, 1)); // TXA
        cpu_test!(&[0xA0, 1, 0x98], |vm| assert_eq!(vm.cpu.a, 1)); // TYA
        cpu_test!(&[0xA2, 1, 0x9A], |vm| assert_eq!(vm.cpu.s, 1)); // TXS
    }

    #[test]
    fn test_st_absolute() {
        // STA $abs
        cpu_test!(&[0xA9, 1, 0x8D, 0x11, 0x0D, 0xA9, 2],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.a, 2);
                assert_eq!(vm.memory[0x0D11], 1);
        });
    }

    #[test]
    fn test_jmp() {
        // 0x6C => instruction!(0x6C, &"JMP", 3, 5, AddressMode::Indirect),
        // JMP $abs
        cpu_test!(&[0x4C, 5, 0, 0xA9, 2, 0xA9, 1], |vm| {
            assert_eq!(vm.cpu.a, 1);
            assert_eq!(vm.cpu.cycles, 5);
        });
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
    fn test_bne() {
        // BNE $rel
        branch_test!(0xD0,
            branch: |vm: &mut VM| vm.cpu.flags.z = false,
            nobranch: |vm: &mut VM| vm.cpu.flags.z = true
        );
        // BEQ $rel
        branch_test!(0xF0,
            branch: |vm: &mut VM| vm.cpu.flags.z = true,
            nobranch: |vm: &mut VM| vm.cpu.flags.z = false
        );
        // BPL $rel
        branch_test!(0x10,
            branch: |vm: &mut VM| vm.cpu.flags.n = false,
            nobranch: |vm: &mut VM| vm.cpu.flags.n = true
        );
    }

    #[test]
    fn test_dex() {
        // DEX
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 2,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 1);
                assert!(!vm.cpu.flags.z);
                assert!(!vm.cpu.flags.n);
        });
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0);
                assert!(vm.cpu.flags.z);
                assert!(!vm.cpu.flags.n);
        });
        cpu_test!(&[0xCA],
            init: |vm| vm.cpu.x = 0,
            exit: |vm| {
                assert_eq!(vm.cpu.x, 0xFF);
                assert!(!vm.cpu.flags.z);
                assert!(vm.cpu.flags.n);
        });
    }

    #[test]
    fn test_cmp() {
        // 0xC9 CMP $imm
        cpu_test!(&[0xC9, 1],
            init: |vm| vm.cpu.a = 1,
            exit: |vm| {
                assert!(vm.cpu.flags.z);
                assert!(!vm.cpu.flags.n);
                assert!(!vm.cpu.flags.c);
        });
        // cpu_test!(&[0xC9, 1],
        //     init: |vm| vm.cpu.a = 1,
        //     exit: |vm| { assert_eq!(vm.cpu.flags.z)}
        // );
    }
}
