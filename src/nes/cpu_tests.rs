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
    ($m:expr, post:$post:tt) => {
        cpu_test!(@builder $m, |_| {}, |_| {}, pre:{}, post:$post)
    };
    ($m:expr, init:$init:expr, exit:$exit:expr) => {
        cpu_test!(@builder $m, $init, $exit, pre:{}, post:{})
    };
    ($m:expr, post:$post:tt, exit:$exit:expr) => {
        cpu_test!(@builder $m, |_| {}, $exit, pre:{}, post:$post)
    };
    ($m:expr, $exit:expr) => {
        cpu_test!(@builder $m, |_| {}, $exit, pre:{}, post:{})
    };
    ($m:expr) => {
        cpu_test!(@builder $m, |_| {}, |_| {}, pre:{}, post:{})
    };
}

/* In-memory ops */

#[test]
fn test_lda_imm() {
    cpu_test!(&[0xA9, 1], |vm| assert_eq!(vm.cpu.a, 1));
    cpu_test!(&[0xA9, 0],
        init: |vm| vm.cpu.a = 1,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags { zero: true, ..Flags::default() });
    });
    cpu_test!(&[0xA9, 0x81], |vm| {
        assert_eq!(vm.cpu.a, 0x81);
        assert_eq!(
            vm.cpu.flags,
            Flags {
                negative: true,
                ..Flags::default()
            }
        );
    });
}

#[test]
fn test_ldy_zp() {
    cpu_test!(&[0xA4, 0xF1],
        init: |vm| vm.memory[0xF1] = 1,
        exit: |vm| {
            assert_eq!(vm.cpu.y, 1);
            assert_eq!(vm.cpu.flags, Flags::default())
    });
    cpu_test!(&[0xA4, 0xF1],
        init: |vm| vm.memory[0xF1] = 0,
        exit: |vm| {
            assert_eq!(vm.cpu.y, 0);
            assert_eq!(vm.cpu.flags, Flags { zero: true, ..Flags::default() });
    });
    cpu_test!(&[0xA4, 0xF1],
        init: |vm| vm.memory[0xF1] = 0x80,
        exit: |vm| {
            assert_eq!(vm.cpu.y, 0x80);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
}

#[test]
fn test_sbc_zpx() {
    cpu_test!(&[0xF5, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 2;
            vm.cpu.x = 1;
            vm.cpu.flags.carry = true;
            vm.memory[0xF2] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 1);
            assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()});
    });
    cpu_test!(&[0xF5, 0xF1], // 0
        init: |vm| {
            vm.cpu.a = 1;
            vm.cpu.x = 1;
            vm.cpu.flags.carry = true;
            vm.memory[0xF2] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {carry: true, zero: true, ..Flags::default()});
    });
    cpu_test!(&[0xF5, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 2;
            vm.cpu.x = 1;
            vm.cpu.flags.carry = true;
            vm.memory[0xF2] = 3;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, -1i8 as u8);
            vm.cpu.x = 1;
            assert_eq!(vm.cpu.flags, Flags {negative: true, overflow: true, ..Flags::default()});
    });
}

#[test]
fn test_ldx_zpy() {
    cpu_test!(&[0xB6, 0xF1], // 1
        init: |vm| {
            vm.memory[0xF2] = 1;
            vm.cpu.y = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.x, 1);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0xB6, 0xF1], // 1
        init: |vm| {
            vm.cpu.y = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.x, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0xB6, 0xF1], // 1
        init: |vm| {
            vm.memory[0xF2] = 0x80;
            vm.cpu.y = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.x, 0x80);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
    // cpu_test!(&[0xB6, 1], |vm: &mut VM| assert_eq!(vm.cpu.$field, 1));
    // cpu_test!(&[$op, 0],
    //     init: |vm| vm.cpu.$field = 1,
    //     exit: |vm| {
    //         assert_eq!(vm.cpu.$field, 0);
    //         assert_eq!(vm.cpu.flags, Flags { zero: true, ..Flags::default() });
    // });
    // cpu_test!(&[$op, 0x81], |vm| {
    //     assert_eq!(vm.cpu.$field, 0x81);
    //     assert_eq!(
    //         vm.cpu.flags,
    //         Flags {
    //             negative: true,
    //             ..Flags::default()
    //         }
    //     );
    // });
}

#[test]
fn test_and_abs() {
    cpu_test!(&[0x2D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x801] = 0b10111101;
            vm.cpu.a = 0b01111110;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0b00111100);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x2D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x801] = 0b10101010;
            vm.cpu.a = 0b01010101;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x2D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x801] = 0b10000000;
            vm.cpu.a = 0b11111111;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0b10000000);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
}

#[test]
fn test_ora_absx() {
    cpu_test!(&[0x1D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b00101010;
            vm.cpu.a = 0b01010101;
            vm.cpu.x = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0b01111111);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x1D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b00000000;
            vm.cpu.a = 0b00000000;
            vm.cpu.x = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x1D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b10000000;
            vm.cpu.a = 0b11111111;
            vm.cpu.x = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0xFF);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
}

#[test]
fn test_eor_absy() {
    cpu_test!(&[0x59, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b10101011;
            vm.cpu.a = 0b11010101;
            vm.cpu.y = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0b01111110);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x59, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b00000000;
            vm.cpu.a = 0b00000000;
            vm.cpu.y = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x59, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 0b00000000;
            vm.cpu.a = 0b11111111;
            vm.cpu.y = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.cpu.a, 0xFF);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
}

#[test]
fn test_cmp_indxy() {
    cpu_test!(&[0xC1, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x81] = 0x01;
            vm.memory[0x82] = 0x09;
            vm.memory[0x901] = 1;
            vm.cpu.x = 1;
            vm.cpu.a = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 1)
    );

    cpu_test!(&[0xD1, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x80] = 0x01;
            vm.memory[0x81] = 0x09;
            vm.memory[0x902] = 1;
            vm.cpu.y = 1;
            vm.cpu.a = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 1)
    );
}

#[test]
fn test_bit_zp() {
    cpu_test!(&[0x24, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 0b01111111;
            vm.memory[0xF1] = 0b10111111;
        },
        exit: |vm| assert_eq!(vm.cpu.flags, Flags::default())
    );
    cpu_test!(&[0x24, 0xF1], // 0
        init: |vm| {
            vm.cpu.a = 0b10101010;
            vm.memory[0xF1] = 0b01010101;
        },
        exit: |vm| assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()})
    );
    cpu_test!(&[0x24, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 0b11111111;
            vm.memory[0xF1] = 0b11000000;
        },
        exit: |vm| assert_eq!(vm.cpu.flags, Flags {negative: true, overflow: true, ..Flags::default()})
    );
    // cpu_test!(&[0x65, 0xF1], // 2
    //     init: |vm| {
    //         vm.cpu.a = 0xFF;
    //         vm.memory[0xF1] = 2;
    //     },
    //     exit: |vm| {
    //         assert_eq!(vm.cpu.a, 1);
    //         assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()});
    // });
}

#[test]
fn test_adc_decimal() {
    cpu_test!(&[0x69, 0x99],
        init: |vm| {
            vm.cpu.a = 0x99;
            vm.cpu.flags.decimal = true;
            vm.cpu.flags.carry = true;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0x99);
    })
}

macro_rules! sbc_dec {
    ($a:expr, $b:expr, $carry:expr, $eq:expr, $flags:expr) => {{
        let mut f = $flags.clone();
        f.decimal = true;
        cpu_test!(&[0xE9, $b],
            init: |vm: &mut VM| {
                vm.cpu.a = $a;
                vm.cpu.flags.carry = $carry;
                vm.cpu.flags.decimal = true;
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, $eq);
                assert_eq!(vm.cpu.flags, f);
        });
    }};
}

#[test]
fn test_sbc_decimal() {
    sbc_dec!(
        0x0,
        0x0,
        false,
        0x99,
        Flags {
            negative: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0x0,
        0x0,
        true,
        0x0,
        Flags {
            zero: true,
            carry: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0x0,
        0x1,
        true,
        0x99,
        Flags {
            negative: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0xA,
        0x0,
        true,
        0xA,
        Flags {
            carry: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0xB,
        0x0,
        false,
        0xA,
        Flags {
            carry: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0x9A,
        0x0,
        true,
        0x9A,
        Flags {
            negative: true,
            carry: true,
            ..Flags::default()
        }
    );
    sbc_dec!(
        0x9B,
        0x0,
        false,
        0x9A,
        Flags {
            negative: true,
            carry: true,
            ..Flags::default()
        }
    );
    // sbc_dec!(0, 0, 0, false, Flags {zero: true, ..Flags::default()});
    // cpu_test!(&[0xE9, 0],
    //     init: |vm| {
    //         vm.cpu.a = 0x99;
    //         vm.cpu.flags.decimal = true;
    //         //vm.cpu.flags.carry = true;
    //     },
    //     exit: |vm| {
    //         assert_eq!(vm.cpu.a, 0x98);
    // });
    // cpu_test!(&[0xE9, 0],
    //     init: |vm| {
    //         vm.cpu.a = 0;
    //         vm.cpu.flags.decimal = true;
    //         //vm.cpu.flags.carry = true;
    //     },
    //     exit: |vm| {
    //         assert_eq!(vm.cpu.a, 0x99);
    // })
}

/* Write ops */

#[test]
fn test_sta_zp() {
    macro_rules! st_zp_test {
        ($op:expr, $field:ident) => {{
            cpu_test!(&[$op, 0xF1],
                init: |vm: &mut VM| vm.cpu.$field = 1,
                exit: |vm: &mut VM| assert_eq!(vm.memory[0xF1], 1)
            );
        }};
    }

    st_zp_test!(0x85, a); // STA $zp
    st_zp_test!(0x86, x); // STX $zp
    st_zp_test!(0x84, y); // STY $zp
}

#[test]
fn test_sta_abs() {
    macro_rules! st_abs_test {
        ($op:expr, $field:ident) => {{
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| vm.cpu.$field = 1,
                exit: |vm: &mut VM| assert_eq!(vm.memory[0x801], 1)
            );
        }};
    }

    st_abs_test!(0x8D, a); // STA $abs
    st_abs_test!(0x8E, x); // STX $abs
    st_abs_test!(0x8C, y); // STY $abs
}

#[test]
fn test_stx_zpy() {
    cpu_test!(&[0x96, 0xF1],
        init: |vm| {
            vm.cpu.x = 1;
            vm.cpu.y = 1;
        },
        exit: |vm| assert_eq!(vm.memory[0xF2], 1)
    );
}

#[test]
fn test_sty_zpx() {
    cpu_test!(&[0x94, 0xF1],
        init: |vm| {
            vm.cpu.x = 1;
            vm.cpu.y = 1;
        },
        exit: |vm| assert_eq!(vm.memory[0xF2], 1)
    );
}

#[test]
fn test_sta_indx() {
    cpu_test!(&[0x81, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x81] = 0x01;
            vm.memory[0x82] = 0x09;
            vm.cpu.a = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.memory[0x901], 1)
    );
}

#[test]
fn test_sta_indy() {
    cpu_test!(&[0x91, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x80] = 0x01;
            vm.memory[0x81] = 0x09;
            vm.cpu.a = 1;
            vm.cpu.y = 1;
        },
        exit: |vm| assert_eq!(vm.memory[0x902], 1)
    );
}

#[test]
fn test_sta_absx() {
    cpu_test!(&[0x9D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.cpu.x = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.memory[0x802], 1);
            assert_eq!(vm.cpu.cycles, 5);
        }
    );

    cpu_test!(&[0x9D, 0xEE, 0x08],
        init: |vm: &mut VM| {
            vm.cpu.x = 0x20;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.memory[0x90E], 1);
            assert_eq!(vm.cpu.cycles, 5);
        }
    );
}

#[test]
fn test_sta_absy() {
    cpu_test!(&[0x99, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.cpu.y = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.memory[0x802], 1);
            assert_eq!(vm.cpu.cycles, 5);
        }
    );

    cpu_test!(&[0x99, 0xEE, 0x08],
        init: |vm: &mut VM| {
            vm.cpu.y = 0x20;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| {
            assert_eq!(vm.memory[0x90E], 1);
            assert_eq!(vm.cpu.cycles, 5);
        }
    );
}

/* Single-byte / implied ops */

macro_rules! flag_test {
    ($set:expr, $clear:expr, $flag:ident) => {
        cpu_test!(&[$set, $clear],
            post: {
                2 => |vm| assert!(vm.cpu.flags.$flag),
                4 => |vm| assert!(!vm.cpu.flags.$flag),
            }
        )
    };
}

#[test]
fn test_sec_clc_impl() {
    flag_test!(0x38, 0x18, carry);
}

#[test]
fn test_sed_cld_impl() {
    flag_test!(0xF8, 0xD8, decimal);
}

#[test]
fn test_sei_cli_impl() {
    flag_test!(0x78, 0x58, irq_disable);
}

#[test]
fn test_clv_impl() {
    cpu_test!(&[0xB8],
        init: |vm| vm.cpu.flags.negative = true,
        exit: |vm| assert!(!vm.cpu.flags.negative)
    );
}

macro_rules! transfer_test {
    ($op:expr, $from:ident, $to:ident) => {{
        cpu_test!(&[$op],
            init: |vm| vm.cpu.$from = 1,
            exit: |vm| {
                assert_eq!(vm.cpu.$to, 1);
                assert_eq!(vm.cpu.flags, Flags::default());
            }
        );
        cpu_test!(&[$op],
            init: |vm| vm.cpu.$from = 0x80,
            exit: |vm| {
                assert_eq!(vm.cpu.$to, 0x80);
                assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
            }
        );
        cpu_test!(&[$op],
            init: |vm| vm.cpu.$from = 0,
            exit: |vm| {
                assert_eq!(vm.cpu.$to, 0);
                assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
            }
        );
    }};
}

#[test]
fn test_tax_impl() {
    transfer_test!(0xAA, a, x);
}

#[test]
fn test_tay_impl() {
    transfer_test!(0xA8, a, y);
}

#[test]
fn test_tsx_impl() {
    transfer_test!(0xBA, s, x);
}

#[test]
fn test_txa_impl() {
    transfer_test!(0x8A, x, a);
}

#[test]
fn test_txs_impl() {
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
}

#[test]
fn test_tya_impl() {
    transfer_test!(0x98, y, a);
}

macro_rules! inc_dec_test {
    ($inc:expr, $dec:expr, $register:ident) => {{
        cpu_test!(&[$inc, $dec],
            pre: {
                0 => |vm| vm.cpu.$register = 1,
            },
            post: {
                2 => |vm| assert_eq!(vm.cpu.$register, 2),
                4 => |vm| {
                    assert_eq!(vm.cpu.$register, 1);
                    assert_eq!(vm.cpu.flags, Flags::default());
                }
            }
        );
    }};
}

#[test]
fn test_inx_dex_impl() {
    inc_dec_test!(0xE8, 0xCA, x);
}

#[test]
fn test_iny_dey_impl() {
    inc_dec_test!(0xC8, 0x88, y);
}

/* RMW ops */

#[test]
fn test_ror_zp() {
    cpu_test!(&[0x66, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 0b11111111;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0b01111111);
            assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()});
        }
    );

    cpu_test!(&[0x66, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 0b11111110;
            vm.cpu.flags.carry = true;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0b11111111);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
        }
    );

    cpu_test!(&[0x66, 0xF1], |vm| {
        assert_eq!(vm.memory[0xF1], 0);
        assert_eq!(
            vm.cpu.flags,
            Flags {
                zero: true,
                ..Flags::default()
            }
        );
    });
}

#[test]
fn test_rol_zpx() {
    cpu_test!(&[0x36, 0xF1],
        init: |vm| {
            vm.memory[0xF2] = 0b01111111;
            vm.cpu.x = 1;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF2], 0b11111110);
            assert_eq!(vm.cpu.flags, Flags {negative: true, carry: false, ..Flags::default()});
        }
    );

    cpu_test!(&[0x36, 0xF1],
        init: |vm| {
            vm.memory[0xF2] = 0b11111111;
            vm.cpu.x = 1;
            vm.cpu.flags.carry = true;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF2], 0b11111111);
            assert_eq!(vm.cpu.flags, Flags {negative: true, carry: true, ..Flags::default()});
        }
    );

    cpu_test!(&[0x36, 0xF1],
        init: |vm| {
            vm.cpu.x = 1;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF2], 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
        }
    );
}

#[test]
fn test_lsr_abs() {
    cpu_test!(&[0x4E, 0x1, 0x3],
        init: |vm| vm.memory[0x301] = 0b11111110,
        exit: |vm| {
            assert_eq!(vm.memory[0x301], 0b01111111);
            assert_eq!(vm.cpu.flags, Flags::default())
    });
}

#[test]
fn test_asl_absx() {
    cpu_test!(&[0x1E, 0x1, 0x3],
        init: |vm| {
            vm.memory[0x302] = 0b00111111;
            vm.cpu.x = 1;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0x302], 0b01111110);
            assert_eq!(vm.cpu.flags, Flags::default())
    });
}

#[test]
fn test_inc_zp() {
    cpu_test!(&[0xE6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 2);
            assert_eq!(vm.cpu.flags, Flags::default());
        }
    );
    cpu_test!(&[0xE6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 0x7F;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0x80);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
        }
    );
    cpu_test!(&[0xE6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 0xFF;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
        }
    );
}

#[test]
fn test_dec_zp() {
    cpu_test!(&[0xC6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 2;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 1);
            assert_eq!(vm.cpu.flags, Flags::default());
        }
    );
    cpu_test!(&[0xC6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 0;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0xFF);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
        }
    );
    cpu_test!(&[0xC6, 0xF1],
        init: |vm| {
            vm.memory[0xF1] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.memory[0xF1], 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
        }
    );
}

/* Branch / jump / return */

#[test]
fn test_jump_abs() {
    cpu_test!(&[0x4C, 5, 0, 0xA9, 2, 0xA9, 1], |vm| {
        assert_eq!(vm.cpu.a, 1);
        assert_eq!(vm.cpu.cycles, 5);
    });
}

#[test]
fn test_jump_ind() {
    cpu_test!(&[0x6C, 0x01, 0x10],
        init: |vm: &mut VM| {
            vm.memory[0x1001] = 0x02;
            vm.memory[0x1002] = 0x20;
        },
        exit: |vm| assert_eq!(vm.cpu.pc, 0x2002)
    );
}

#[test]
fn test_jsr_rts() {
    let mut memory = [0u8; 0x300];
    memory[0] = 0x20;
    memory[1] = 0x02;
    memory[2] = 0x02;
    memory[3] = 0xE8;
    memory[0x202] = 0xE8;
    memory[0x203] = 0x60;

    cpu_test!(&memory,
        post: {
            6 => |vm| {
                assert_eq!(vm.cpu.pc, 0x202);
                assert_eq!(vm.memory[0x1FF], 0);
                assert_eq!(vm.memory[0x1FE], 2);
                assert_eq!(vm.cpu.s, 0xFD);
            },
            13 => |vm| {
                assert_eq!(vm.cpu.pc, 2);
                assert_eq!(vm.cpu.x, 1);
                vm.program_end = 4;
            }
        },
        exit: |vm| {
            assert_eq!(vm.cpu.x, 2);
            assert_eq!(vm.cpu.s, 0xFF);
        }
    );
}

macro_rules! branch_test_not_taken {
    ($op:expr, $condition:expr) => {
        cpu_test!(&[$op, 10, 0xA9, 1],
            init: |vm: &mut VM| $condition(vm),
            post: {
                3 => |vm: &mut VM| assert_eq!(vm.cpu.pc, 3),
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.cycles, 4);
                assert_eq!(vm.cpu.pc, 4);
            }
        );
    }
}

macro_rules! branch_test_pos {
    ($op:expr, $condition:expr) => {
        let mut memory = [0u8; 0x400];
        memory[0] = $op;
        memory[1] = 10;
        memory[12] = 0xA9;
        memory[13] = 1;
        cpu_test!(&memory,
            init: |vm: &mut VM| {
                $condition(vm);
                vm.program_end = 13;
            },
            post: {
                3 => |vm: &mut VM| assert_eq!(vm.cpu.pc, 12),
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.cycles, 5);
                assert_eq!(vm.cpu.pc, 14);
            }
        );
    };
}

macro_rules! branch_test_neg {
    ($op:expr, $condition:expr) => {{
        let mut memory = [0u8; 0x400];
        memory[20] = $op;
        memory[21] = -12i8 as u8;
        memory[10] = 0xA9;
        memory[11] = 1;


        cpu_test!(&memory,
            init: |vm: &mut VM| {
                $condition(vm);
                vm.cpu.pc = 20;
                vm.cpu.pins.addr = 20;
                vm.cpu.pins.data = vm.memory[20];
            },
            post: {
                3 => |vm: &mut VM| {
                    vm.program_end = 11;
                    assert_eq!(vm.cpu.pc, 10);
                },
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.cycles, 5);
                assert_eq!(vm.cpu.pc, 12);
            }
        );
    }};
}

macro_rules! branch_test_pos_page {
    ($op:expr, $condition:expr) => {{
        let mut memory = [0u8; 0x400];
        memory[0x00F1] = $op;
        memory[0x00F2] = 0x17;
        memory[0x010A] = 0xA9;
        memory[0x010B] = 1;

        cpu_test!(&memory,
            init: |vm: &mut VM| {
                vm.cpu.flags.zero = false;
                vm.cpu.pc = 0xF1;
                vm.cpu.pins.addr = 0xF1;
                vm.cpu.pins.data = vm.memory[0xF1];
                vm.program_end = 0x10B;
                $condition(vm);
            },
            post: {
                4 => |vm: &mut VM| assert_eq!(vm.cpu.pc, 0x10A),
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.cycles, 6);
                assert_eq!(vm.cpu.pc, 0x10C);
            }
        );
    }};
}

macro_rules! branch_test_neg_page {
    ($op:expr, $condition:expr) => {{
        let mut memory = [0u8; 0x400];
        memory[0x0305] = $op;
        memory[0x0306] = -12i8 as u8;
        memory[0x02FB] = 0xA9;
        memory[0x02FC] = 1;

        cpu_test!(&memory,
            init: |vm: &mut VM| {
                vm.cpu.flags.zero = false;
                vm.cpu.pc = 0x305;
                vm.cpu.pins.addr = vm.cpu.pc;
                vm.cpu.pins.data = vm.memory[vm.cpu.pc as usize];
                vm.program_end = 0x2FC;
                $condition(vm);
            },
            post: {
                4 => |vm: &mut VM| assert_eq!(vm.cpu.pc, 0x2FB),
            },
            exit: |vm: &mut VM| {
                assert_eq!(vm.cpu.a, 1);
                assert_eq!(vm.cpu.cycles, 6);
            }
        )
    }};
}

#[test]
fn test_bne_rel() {
    branch_test_not_taken!(0xD0, |vm: &mut VM| vm.cpu.flags.zero = true);
    branch_test_pos!(0xD0, |vm: &mut VM| vm.cpu.flags.zero = false);
    branch_test_neg!(0xD0, |vm: &mut VM| vm.cpu.flags.zero = false);
    branch_test_pos_page!(0xD0, |vm: &mut VM| vm.cpu.flags.zero = false);
    branch_test_neg_page!(0xD0, |vm: &mut VM| vm.cpu.flags.zero = false);
}

#[test]
fn test_beq_rel() {
    branch_test_not_taken!(0xF0, |vm: &mut VM| vm.cpu.flags.zero = false);
    branch_test_pos!(0xF0, |vm: &mut VM| vm.cpu.flags.zero = true);
    branch_test_neg!(0xF0, |vm: &mut VM| vm.cpu.flags.zero = true);
    branch_test_pos_page!(0xF0, |vm: &mut VM| vm.cpu.flags.zero = true);
    branch_test_neg_page!(0xF0, |vm: &mut VM| vm.cpu.flags.zero = true);
}

#[test]
fn test_bpl_rel() {
    branch_test_not_taken!(0x10, |vm: &mut VM| vm.cpu.flags.negative = true);
    branch_test_pos!(0x10, |vm: &mut VM| vm.cpu.flags.negative = false);
    branch_test_neg!(0x10, |vm: &mut VM| vm.cpu.flags.negative = false);
    branch_test_pos_page!(0x10, |vm: &mut VM| vm.cpu.flags.negative = false);
    branch_test_neg_page!(0x10, |vm: &mut VM| vm.cpu.flags.negative = false);
}

#[test]
fn test_bvc_rel() {
    branch_test_not_taken!(0x50, |vm: &mut VM| vm.cpu.flags.overflow = true);
    branch_test_pos!(0x50, |vm: &mut VM| vm.cpu.flags.overflow = false);
    branch_test_neg!(0x50, |vm: &mut VM| vm.cpu.flags.overflow = false);
    branch_test_pos_page!(0x50, |vm: &mut VM| vm.cpu.flags.overflow = false);
    branch_test_neg_page!(0x50, |vm: &mut VM| vm.cpu.flags.overflow = false);
}

#[test]
fn test_bvs_rel() {
    branch_test_not_taken!(0x70, |vm: &mut VM| vm.cpu.flags.overflow = false);
    branch_test_pos!(0x70, |vm: &mut VM| vm.cpu.flags.overflow = true);
    branch_test_neg!(0x70, |vm: &mut VM| vm.cpu.flags.overflow = true);
    branch_test_pos_page!(0x70, |vm: &mut VM| vm.cpu.flags.overflow = true);
    branch_test_neg_page!(0x70, |vm: &mut VM| vm.cpu.flags.overflow = true);
}

/* Stack */

#[test]
fn test_pha_a() {
    cpu_test!(&[0x48, 0x48],
        init: |vm| vm.cpu.a = 1,
        exit: |vm| {
            assert_eq!(vm.cpu.s, 0xFD);
            assert_eq!(vm.memory[0x01FF], 1);
            assert_eq!(vm.memory[0x01FE], 1);
    });
}

#[test]
fn test_pla_a() {
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
}

#[test]
fn test_php_imp() {
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
}

#[test]
fn test_plp_imp() {
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

/* Programs */

#[test]
fn test_pc_increment() {
    cpu_test!(&[0xA9, 1, 0x8D, 0x1, 0x3, 0xA9, 2], |vm| {
        assert_eq!(vm.cpu.a, 2);
    });
}
