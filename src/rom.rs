pub struct ROM {
    pub bytes: [u8; 0xFFFF],
}

impl ROM {
    const PROGRAM_START: usize = 0x8000;

    pub fn empty() -> Self {
        Self {
            bytes: [0u8; 0xFFFF],
        }
    }

    pub fn load_program(&mut self, bytes: &[u8]) {
        for i in 0..bytes.len() {
            let offset = i + Self::PROGRAM_START;
            if offset >= 0xFFFF {
                break;
            }
            self.bytes[offset] = bytes[i];
        }
    }
}
