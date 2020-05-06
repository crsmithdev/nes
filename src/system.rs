use crate::cpu::{CPUError, CPUErrorKind, CPU};

error_chain! {
    types {
        VMError, VMErrorKind, VMResultExt, VMResult;
    }

    links {
        CPU(CPUError, CPUErrorKind);
    }

    errors {
        ExecutionError(o: u8) { }
    }
}

const TEST_ROM: &'static [u8] = include_bytes!("../data/6502_functional_test.bin");
const TEST_START: u16 = 0x400;

pub struct TestVM {
    pub cpu: CPU,
    pub memory: Vec<u8>,
    error: Option<VMError>,
}

impl TestVM {
    pub fn new() -> Self {
        let memory = Vec::from(&*TEST_ROM);
        let mut cpu = CPU::new();
        cpu.pc = TEST_START;

        TestVM {
            cpu,
            memory,
            error: None,
        }
    }

    pub fn update(&mut self) {
        if self.error.is_none() {
            self.cpu.pins.data = self.memory[self.cpu.pins.addr as usize];

            if let Err(e) = self.cpu.cycle() {
                error!("cpu error: {}", e);
                self.error = Some(e.into());
            }
        }
    }
}
