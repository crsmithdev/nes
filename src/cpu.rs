use std::fmt;

error_chain! {
    types {
        CPUError, CPUErrorKind, CPUResultExt, CPUResult;
    }

    errors {
        UnknownInstruction(o: u8) { }
        ExecutionTiming(i: Instruction, t: u8) {
            description("execution timing error")
            display("exeuction timing error: {}, t={}", i, t)
        }
        AddressMode(o: u8) { }
        UnexpectedInstruction(o: u8) { }
        NotImplemented(i: Instruction) {
            description("instruction not implemented")
            display("instruction not implemented: {}", i)
         }
    }
}

macro_rules! instruction {
    ($opcode:expr, $name:expr, $bytes:expr, $cycles: expr, $mode:expr) => {
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
        debug!("begin cycle {} [t{}]", self.cycles, self.t);

        self.fetch_data()?;

        let result = match self.t {
            0 => {
                self.t += 1;
                self.pc += 1;
                self.set_addr16(self.pc);
                Ok(())
            }
            1..=6 => {
                let result = match self.current.opcode {
                    0xEA => Ok(()),
                    0x18 | 0x38 | 0x58 | 0x78 | 0xB8 | 0xD8 | 0xF8 => self.cycle_flag_op(),
                    0xA0 | 0xA1 | 0xA2 | 0xA4 | 0xA5 | 0xA9 | 0xA6 | 0xAC | 0xAD | 0xAE | 0xB1
                    | 0xB4 | 0xB5 | 0xB6 | 0xB9 | 0xBC | 0xBD | 0xBE => self.cycle_load_op(),
                    _ => bail!(CPUErrorKind::NotImplemented(self.current)),
                };

                self.t = match self.current.cycles {
                    c if c - 1 == self.t => 0,
                    _ => self.t + 1,
                };
                if self.t == 0 {
                    self.pc += 1;
                    self.set_addr16(self.pc);
                }
                result
            }
            _ => bail!(CPUErrorKind::ExecutionTiming(self.current, self.t)),
        };

        debug!("end cycle {} [t{}]", self.cycles, self.t);
        self.cycles += 1;

        result
    }

    fn cycle_flag_op(&mut self) -> CPUResult<()> {
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
                    _ => bail!(CPUErrorKind::UnexpectedInstruction(op)),
                }
            }
            _ => bail!(CPUErrorKind::ExecutionTiming(self.current, self.t)),
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

    fn set_addr(&mut self, low: u8, high: u8) {
        self.pins.addr = (low as u16) | ((high as u16) << 8)
    }

    fn set_addr8(&mut self, byte: u8) {
        self.pins.addr = byte as u16;
    }

    fn set_addr16(&mut self, bytes: u16) {
        self.pins.addr = bytes;
    }

    fn cycle_load_op(&mut self) -> CPUResult<()> {
        let inst = self.current;
        let op = self.current.opcode;
        let byte = self.pins.data;
        let t = self.t;

        match op {
            // LDA #im
            0xA9 => match t {
                1 => self.set_a(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDX #im
            0xA2 => match self.t {
                1 => self.set_x(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDY #im
            0xA0 => match self.t {
                1 => self.set_y(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDA $zp
            0xA5 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_a(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDX $zp
            0xA6 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_x(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDY $zp
            0xA4 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_y(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDA $zp,X
            0xB5 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_addr8(self.x + self.current.arg1),
                3 => self.set_a(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDX $zp,Y
            0xB6 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_addr8(self.y + self.current.arg1),
                3 => self.set_x(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDY $zp,X
            0xB4 => match self.t {
                1 => self.set_addr8(self.pins.data),
                2 => self.set_addr8(self.x + self.current.arg1),
                3 => self.set_y(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDA $abs
            0xAD => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_a(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDX $abs
            0xAE => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_x(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // LDY $abs
            0xAC => match self.t {
                1 => (),
                2 => self.set_addr(self.current.arg1, byte),
                3 => self.set_y(byte),
                _ => bail!(CPUErrorKind::ExecutionTiming(inst, t)),
            },
            // 0xBD => instruction!(byte, &"LDA", 3, AbsoluteX),
            // 0xBC => instruction!(byte, &"LDY", 3, AbsoluteX),
            // 0xB9 => instruction!(byte, &"LDA", 3, AbsoluteY),
            // 0xBE => instruction!(byte, &"LDX", 3, AbsoluteY),
            // 0xB9 => instruction!(byte, &"LDA", 3, AbsoluteY),
            // 0xA1 => instruction!(byte, &"LDA", 2, IndirectX),
            // 0xB1 => instruction!(byte, &"LDA", 2, IndirectY),
            _ => bail!(CPUErrorKind::UnexpectedInstruction(op)),
        }

        Ok(())
    }
}

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
    use AddressMode::*;

    let instruction = match byte {
        0x69 => instruction!(byte, &"ADC", 2, 2, Immediate),
        0x65 => instruction!(byte, &"ADC", 2, 3, ZeroPage),
        0x75 => instruction!(byte, &"ADC", 2, 4, ZeroPageX),
        0x6D => instruction!(byte, &"ADC", 3, 4, Absolute),
        0x7D => instruction!(byte, &"ADC", 3, 4, AbsoluteX),
        0x79 => instruction!(byte, &"ADC", 3, 4, AbsoluteY),
        0x61 => instruction!(byte, &"ADC", 2, 6, IndirectX),
        0x71 => instruction!(byte, &"ADC", 2, 5, IndirectY),
        0x29 => instruction!(byte, &"AND", 2, 2, Immediate),
        0x25 => instruction!(byte, &"AND", 2, 3, ZeroPage),
        0x35 => instruction!(byte, &"AND", 2, 4, ZeroPageX),
        0x2D => instruction!(byte, &"AND", 3, 4, Absolute),
        0x3D => instruction!(byte, &"AND", 3, 4, AbsoluteX),
        0x39 => instruction!(byte, &"AND", 3, 4, AbsoluteY),
        0x21 => instruction!(byte, &"AND", 2, 6, IndirectX),
        0x31 => instruction!(byte, &"AND", 2, 5, IndirectY),
        0x0A => instruction!(byte, &"ASL", 1, 2, Accumulator),
        0x06 => instruction!(byte, &"ASL", 2, 5, ZeroPage),
        0x16 => instruction!(byte, &"ASL", 2, 6, ZeroPageX),
        0x0E => instruction!(byte, &"ASL", 3, 6, Absolute),
        0x1E => instruction!(byte, &"ASL", 3, 7, AbsoluteX),
        0x24 => instruction!(byte, &"BIT", 2, 3, ZeroPageX),
        0x2C => instruction!(byte, &"BIT", 3, 4, Absolute),
        0x10 => instruction!(byte, &"BPL", 2, 2, Relative),
        0x30 => instruction!(byte, &"BMI", 2, 2, Relative),
        0x50 => instruction!(byte, &"BVC", 2, 2, Relative),
        0x70 => instruction!(byte, &"BVS", 2, 2, Relative),
        0x90 => instruction!(byte, &"BCC", 2, 2, Relative),
        0xB0 => instruction!(byte, &"BCS", 2, 2, Relative),
        0xD0 => instruction!(byte, &"BNE", 2, 2, Relative),
        0xF0 => instruction!(byte, &"BEQ", 2, 2, Relative),
        0x00 => instruction!(byte, &"BRK", 2, 7, Immediate),
        0xC9 => instruction!(byte, &"CMP", 2, 2, Immediate),
        0xC5 => instruction!(byte, &"CMP", 2, 3, ZeroPage),
        0xD5 => instruction!(byte, &"CMP", 2, 4, ZeroPageX),
        0xCD => instruction!(byte, &"CMP", 3, 4, Absolute),
        0xDD => instruction!(byte, &"CMP", 3, 4, AbsoluteX),
        0xD9 => instruction!(byte, &"CMP", 3, 4, AbsoluteY),
        0xC1 => instruction!(byte, &"CMP", 2, 6, IndirectX),
        0xD1 => instruction!(byte, &"CMP", 2, 5, IndirectY),
        0xE0 => instruction!(byte, &"CPX", 2, 2, Immediate),
        0xE4 => instruction!(byte, &"CPX", 2, 3, ZeroPage),
        0xEC => instruction!(byte, &"CPX", 3, 4, Absolute),
        0xC0 => instruction!(byte, &"CPY", 2, 2, ZeroPage),
        0xC4 => instruction!(byte, &"CPY", 2, 3, ZeroPage),
        0xCC => instruction!(byte, &"CPY", 3, 4, Absolute),
        0xC6 => instruction!(byte, &"DEC", 2, 5, ZeroPage),
        0xD6 => instruction!(byte, &"DEC", 2, 6, ZeroPageX),
        0xCE => instruction!(byte, &"DEC", 3, 6, Absolute),
        0xDE => instruction!(byte, &"DEC", 3, 7, AbsoluteX),
        0x49 => instruction!(byte, &"EOR", 2, 2, Immediate),
        0x45 => instruction!(byte, &"EOR", 2, 3, ZeroPage),
        0x55 => instruction!(byte, &"EOR", 2, 4, ZeroPageX),
        0x4D => instruction!(byte, &"EOR", 3, 4, Absolute),
        0x5D => instruction!(byte, &"EOR", 3, 4, AbsoluteX),
        0x59 => instruction!(byte, &"EOR", 3, 4, AbsoluteY),
        0x41 => instruction!(byte, &"EOR", 2, 6, IndirectX),
        0x51 => instruction!(byte, &"EOR", 2, 5, IndirectY),
        0x18 => instruction!(byte, &"CLC", 1, 2, Implicit),
        0x38 => instruction!(byte, &"SEC", 1, 2, Implicit),
        0x58 => instruction!(byte, &"CLI", 1, 2, Implicit),
        0x78 => instruction!(byte, &"SEI", 1, 2, Implicit),
        0xB8 => instruction!(byte, &"CLV", 1, 2, Implicit),
        0xD8 => instruction!(byte, &"CLD", 1, 2, Implicit),
        0xF8 => instruction!(byte, &"SED", 1, 2, Implicit),
        0xE6 => instruction!(byte, &"INC", 2, 5, ZeroPage),
        0xF6 => instruction!(byte, &"INC", 2, 6, ZeroPageX),
        0xEE => instruction!(byte, &"INC", 3, 6, Absolute),
        0xFE => instruction!(byte, &"INC", 3, 7, AbsoluteX),
        0x4C => instruction!(byte, &"JMP", 3, 3, Absolute),
        0x6C => instruction!(byte, &"JMP", 3, 5, Indirect),
        0x20 => instruction!(byte, &"JSR", 3, 6, Absolute),
        0xA9 => instruction!(byte, &"LDA", 2, 2, Immediate),
        0xA5 => instruction!(byte, &"LDA", 2, 3, ZeroPage),
        0xB5 => instruction!(byte, &"LDA", 2, 4, ZeroPageX),
        0xAD => instruction!(byte, &"LDA", 3, 4, Absolute),
        0xBD => instruction!(byte, &"LDA", 3, 4, AbsoluteX),
        0xB9 => instruction!(byte, &"LDA", 3, 4, AbsoluteY),
        0xA1 => instruction!(byte, &"LDA", 2, 6, IndirectX),
        0xB1 => instruction!(byte, &"LDA", 2, 5, IndirectY),
        0xA2 => instruction!(byte, &"LDX", 2, 2, Immediate),
        0xA6 => instruction!(byte, &"LDX", 2, 3, ZeroPage),
        0xB6 => instruction!(byte, &"LDX", 2, 4, ZeroPageY),
        0xAE => instruction!(byte, &"LDX", 3, 4, Absolute),
        0xBE => instruction!(byte, &"LDX", 3, 4, AbsoluteY),
        0xA0 => instruction!(byte, &"LDY", 2, 2, Immediate),
        0xA4 => instruction!(byte, &"LDY", 2, 3, ZeroPage),
        0xB4 => instruction!(byte, &"LDY", 2, 4, ZeroPageY),
        0xAC => instruction!(byte, &"LDY", 3, 4, Absolute),
        0xBC => instruction!(byte, &"LDY", 3, 4, AbsoluteY),
        0x4A => instruction!(byte, &"LSR", 1, 2, Accumulator),
        0x46 => instruction!(byte, &"LSR", 2, 5, ZeroPage),
        0x56 => instruction!(byte, &"LSR", 2, 6, ZeroPageX),
        0x4E => instruction!(byte, &"LSR", 3, 6, Absolute),
        0x5E => instruction!(byte, &"LSR", 3, 7, AbsoluteX),
        0xEA => instruction!(byte, &"NOP", 1, 2, Implicit),
        0x09 => instruction!(byte, &"ORA", 2, 2, Immediate),
        0x05 => instruction!(byte, &"ORA", 2, 3, ZeroPage),
        0x15 => instruction!(byte, &"ORA", 2, 4, ZeroPageX),
        0x0D => instruction!(byte, &"ORA", 3, 4, Absolute),
        0x1D => instruction!(byte, &"ORA", 3, 4, AbsoluteX),
        0x19 => instruction!(byte, &"ORA", 3, 4, AbsoluteY),
        0x01 => instruction!(byte, &"ORA", 2, 6, IndirectX),
        0x11 => instruction!(byte, &"ORA", 2, 5, IndirectY),
        0xAA => instruction!(byte, &"TAX", 1, 2, Implicit),
        0x8A => instruction!(byte, &"TXA", 1, 2, Implicit),
        0xCA => instruction!(byte, &"DEX", 1, 2, Implicit),
        0xE8 => instruction!(byte, &"INX", 1, 2, Implicit),
        0xA8 => instruction!(byte, &"TAY", 1, 2, Implicit),
        0x98 => instruction!(byte, &"TYA", 1, 2, Implicit),
        0x88 => instruction!(byte, &"DEY", 1, 2, Implicit),
        0xC8 => instruction!(byte, &"INY", 1, 2, Implicit),
        0x2A => instruction!(byte, &"ROL", 1, 2, Accumulator),
        0x26 => instruction!(byte, &"ROL", 2, 5, ZeroPage),
        0x36 => instruction!(byte, &"ROL", 2, 6, ZeroPageX),
        0x2E => instruction!(byte, &"ROL", 3, 6, Absolute),
        0x3E => instruction!(byte, &"ROL", 3, 7, AbsoluteX),
        0x6A => instruction!(byte, &"ROR", 1, 2, Accumulator),
        0x66 => instruction!(byte, &"ROR", 2, 5, ZeroPage),
        0x76 => instruction!(byte, &"ROR", 2, 6, ZeroPageX),
        0x6E => instruction!(byte, &"ROR", 3, 6, Absolute),
        0x7E => instruction!(byte, &"ROR", 3, 7, AbsoluteX),
        0x40 => instruction!(byte, &"RTI", 1, 6, Implicit),
        0x60 => instruction!(byte, &"RTS", 1, 6, Implicit),
        0xE9 => instruction!(byte, &"SBC", 2, 2, Immediate),
        0xE5 => instruction!(byte, &"SBC", 2, 3, ZeroPage),
        0xF5 => instruction!(byte, &"SBC", 2, 4, ZeroPageX),
        0xED => instruction!(byte, &"SBC", 3, 4, Absolute),
        0xFD => instruction!(byte, &"SBC", 3, 4, AbsoluteX),
        0xF9 => instruction!(byte, &"SBC", 3, 4, AbsoluteY),
        0xE1 => instruction!(byte, &"SBC", 2, 6, IndirectX),
        0xF1 => instruction!(byte, &"SBC", 2, 4, IndirectY),
        0x85 => instruction!(byte, &"STA", 2, 3, ZeroPage),
        0x95 => instruction!(byte, &"STA", 2, 4, ZeroPageX),
        0x8D => instruction!(byte, &"STA", 3, 4, Absolute),
        0x9D => instruction!(byte, &"STA", 3, 5, AbsoluteX),
        0x99 => instruction!(byte, &"STA", 3, 5, AbsoluteY),
        0x81 => instruction!(byte, &"STA", 2, 6, IndirectX),
        0x91 => instruction!(byte, &"STA", 2, 6, IndirectY),
        0x9A => instruction!(byte, &"TXS", 1, 2, Implicit),
        0xBA => instruction!(byte, &"TSX", 1, 2, Implicit),
        0x48 => instruction!(byte, &"PHA", 1, 3, Implicit),
        0x68 => instruction!(byte, &"PLA", 1, 4, Implicit),
        0x08 => instruction!(byte, &"PHP", 1, 3, Implicit),
        0x28 => instruction!(byte, &"PLP", 1, 4, Implicit),
        0x86 => instruction!(byte, &"STX", 2, 3, ZeroPage),
        0x96 => instruction!(byte, &"STX", 2, 4, ZeroPageX),
        0x8E => instruction!(byte, &"STX", 3, 4, Absolute),
        0x84 => instruction!(byte, &"STY", 2, 3, ZeroPage),
        0x94 => instruction!(byte, &"STY", 2, 4, ZeroPageX),
        0x8C => instruction!(byte, &"STY", 3, 4, Absolute),
        _ => bail!(CPUErrorKind::UnknownInstruction(byte)),
    };

    Ok(instruction)
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

#[allow(unused_imports)]
#[allow(dead_code)]
mod test {
    use super::*;

    fn run_test(memory: &[u8], post: impl Fn(&CPU) -> ()) {
        let mut cpu = CPU::new();
        let instructions = decode_all(memory).unwrap(); // TODO
        let length = instructions.iter().map(|i| i.1.cycles).sum();

        for _ in 0..length {
            if (cpu.pins.addr as usize) < memory.len() {
                cpu.pins.data = memory[cpu.pins.addr as usize];
            }
            if let Err(e) = cpu.cycle() {
                panic!("error: {}", e.description());
            }
        }

        post(&mut cpu);
    }

    #[test]
    fn test_ld_immediate() {
        // LDA #imm, LDX #imm, LDY #imm
        run_test(&[0xA9, 0x01, 0xA2, 0x02, 0xA0, 0x03], |cpu| {
            assert_eq!(cpu.a, 0x01);
            assert_eq!(cpu.x, 0x02);
            assert_eq!(cpu.y, 0x03);
        });
    }

    #[test]
    fn test_ld_flags() {
        // LDA #imm, Z
        run_test(&[0xA9, 0x00], |cpu| {
            assert_eq!(cpu.a, 0x00);
            assert!(cpu.flags.z);
        });

        // LDX #imm, N
        run_test(&[0xA2, 0xFE], |cpu| {
            assert_eq!(cpu.x, 0xFE);
            assert!(cpu.flags.n);
        });
    }

    #[test]
    fn test_ld_zeropage() {
        // LDA $zp, LDX $zp, LDY $zp
        run_test(&[0xA5, 3, 0xA6, 5, 0xA4, 1], |cpu| {
            assert_eq!(cpu.a, 5);
            assert_eq!(cpu.x, 1);
            assert_eq!(cpu.y, 3);
        });
    }

    #[test]
    fn test_flags() {
        run_test(&[0xF8], |cpu| assert!(cpu.flags.d));
        run_test(&[0xF8, 0xD8], |cpu| assert!(!cpu.flags.d));
        run_test(&[0x78], |cpu| assert!(cpu.flags.i));
        run_test(&[0x78, 0x58], |cpu| assert!(!cpu.flags.i));
        run_test(&[0x38], |cpu| assert!(cpu.flags.c));
        run_test(&[0x38, 0x18], |cpu| assert!(!cpu.flags.c));
        run_test(&[0xA9, 255], |cpu| assert!(cpu.flags.n));
        run_test(&[0xA9, 128, 0xB8], |cpu| assert!(!cpu.flags.n));
    }
}
