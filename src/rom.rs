pub struct ROM {
    pub bytes: Vec<u8>,
}

pub fn temp_rom() -> ROM {
    let bytes = include_bytes!("../data/nestest.nes");
    ROM {
        bytes: bytes.to_vec(),
    }
}
