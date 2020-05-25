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
fn test_load_imm() {
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
fn test_adc_zp() {
    cpu_test!(&[0x65, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 1;
            vm.memory[0xF1] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 2);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x65, 0xF1], // 0
        init: |vm| {
            vm.cpu.a = 0;
            vm.memory[0xF1] = 0;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x65, 0xF1], // 1
        init: |vm| {
            vm.cpu.a = 0x7F;
            vm.memory[0xF1] = 1;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0x80);
            assert_eq!(vm.cpu.flags, Flags {negative: true, overflow: true, ..Flags::default()});
    });
    cpu_test!(&[0x65, 0xF1], // 2
        init: |vm| {
            vm.cpu.a = 0xFF;
            vm.memory[0xF1] = 2;
        },
        exit: |vm| {
            assert_eq!(vm.cpu.a, 1);
            assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()});
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
fn test_load_absxy() {
    macro_rules! ld_absxy_test {
        ($op:expr, $field:ident, $offset:ident) => {{
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x802] = 1;
                    vm.cpu.$offset = 1;
                },
                exit: |vm: &mut VM| {
                    assert_eq!(vm.cpu.$field, 1);
                    assert_eq!(vm.cpu.cycles, 4);
                }
            );
            cpu_test!(&[$op, 0xEE, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x90E] = 1;
                    vm.cpu.$offset = 0x20;
                },
                exit: |vm: &mut VM| {
                    assert_eq!(vm.cpu.$field, 1);
                    assert_eq!(vm.cpu.cycles, 5);
                }
            );
        }};
    }

    ld_absxy_test!(0xBD, a, x); // LDA $abs, x
    ld_absxy_test!(0xB9, a, y); // LDA $abs, y
    ld_absxy_test!(0xBE, x, y); // LDX $abs, y
    ld_absxy_test!(0xBC, y, x); // LDY $abs, x
}

#[test]
fn test_load_indxy() {
    cpu_test!(&[0xA1, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x81] = 0x01;
            vm.memory[0x82] = 0x09;
            vm.memory[0x901] = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 1)
    );

    cpu_test!(&[0xB1, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x80] = 0x01;
            vm.memory[0x81] = 0x09;
            vm.memory[0x902] = 1;
            vm.cpu.y = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 1)
    );
}

#[test]
fn test_compare_imm() {
    macro_rules! cmp_imm_test {
        ($op:expr, $field:ident) => {{
            cpu_test!(&[$op, 1],
                init: |vm| vm.cpu.$field = 1,
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { zero: true, carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0],
                init: |vm| vm.cpu.$field = 1,
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 2],
                init: |vm| vm.cpu.$field = 1,
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { negative: true, ..Flags::default() })
            );
        }};
    }

    cmp_imm_test!(0xC9, a); // CMP #imm
    cmp_imm_test!(0xE0, x); // CPX #imm
    cmp_imm_test!(0xC0, y); // CPY #imm
}

#[test]
fn test_compare_abs() {
    macro_rules! ld_abs_test {
        ($op:expr, $field:ident) => {{
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x801] = 1;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { zero: true, carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x801] = 0;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x801] = 2;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { negative: true, ..Flags::default() })
            );
        }};
    }

    ld_abs_test!(0xCD, a); // CMP $abs
    ld_abs_test!(0xEC, x); // CPX $abs
    ld_abs_test!(0xCC, y); // CPY $abs
}

#[test]
fn test_compare_zp() {
    macro_rules! cmp_zp_test {
        ($op:expr, $field:ident) => {{
            cpu_test!(&[$op, 0xF1],
                init: |vm: &mut VM| {
                    vm.memory[0xF1] = 1;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { zero: true, carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0xF1],
                init: |vm: &mut VM| {
                    vm.memory[0xF1] = 0;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0xF1],
                init: |vm: &mut VM| {
                    vm.memory[0xF1] = 2;
                    vm.cpu.$field = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { negative: true, ..Flags::default() })
            );
        }};
    }

    cmp_zp_test!(0xC5, a); // LDA $zp
    cmp_zp_test!(0xE4, x); // LDX $zp
    cmp_zp_test!(0xC4, y); // LDY $zp
}

#[test]
fn test_compare_zpx() {
    // CMP $zp,x
    cpu_test!(&[0xD5, 0xF1],
        init: |vm: &mut VM| {
            vm.memory[0xF2] = 1;
            vm.cpu.a = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.flags, Flags { zero: true, carry: true, ..Flags::default() })
    );
}

#[test]
fn test_compare_absxy() {
    macro_rules! cmp_zpxy_test {
        ($op:expr, $field:ident, $offset:ident) => {{
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x802] = 1;
                    vm.cpu.$offset = 1;
                    vm.cpu.a = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { zero: true, carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x802] = 0;
                    vm.cpu.$offset = 1;
                    vm.cpu.a = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { carry: true, ..Flags::default() })
            );
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.memory[0x802] = 2;
                    vm.cpu.$offset = 1;
                    vm.cpu.a = 1;
                },
                exit: |vm| assert_eq!(vm.cpu.flags, Flags { negative: true, ..Flags::default() })
            );
        }};
    }

    cmp_zpxy_test!(0xDD, a, x); // CMP $abs,x
    cmp_zpxy_test!(0xD9, a, y); // CMP $abs,y
}

#[test]
fn test_compare_indxy() {
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
fn test_add_imm() {
    cpu_test!(&[0x69, 1],
        init: |vm| vm.cpu.a = 1,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 2);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x69, 0],
        init: |vm| vm.cpu.a = 0,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x69, 1],
        init: |vm| vm.cpu.a = 0x7F,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0x80);
            assert_eq!(vm.cpu.flags, Flags {negative: true, overflow: true, ..Flags::default()});
    });
    cpu_test!(&[0x69, 2],
        init: |vm| vm.cpu.a = 0xFF,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 1);
            assert_eq!(vm.cpu.flags, Flags {carry: true, ..Flags::default()});
    });
}

#[test]
fn test_add_zp() {
    cpu_test!(&[0x65, 0xF1],
        init: |vm: &mut VM| {
            vm.memory[0xF1] = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| assert_eq!(vm.cpu.a, 2)
    );
}

#[test]
fn test_add_zpx() {
    cpu_test!(&[0x75, 0xF1],
        init: |vm| {
            vm.memory[0xF2] = 1;
            vm.cpu.a = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 2)
    );
}

#[test]
fn test_add_abs() {
    cpu_test!(&[0x6D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x801] = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| assert_eq!(vm.cpu.a, 2)
    );
}

#[test]
fn test_add_absxy() {
    cpu_test!(&[0x7D, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 1;
            vm.cpu.x = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| assert_eq!(vm.cpu.a, 2)
    );
    cpu_test!(&[0x79, 0x01, 0x08],
        init: |vm: &mut VM| {
            vm.memory[0x802] = 1;
            vm.cpu.y = 1;
            vm.cpu.a = 1;
        },
        exit: |vm: &mut VM| assert_eq!(vm.cpu.a, 2)
    );
}

#[test]
fn test_add_indxy() {
    cpu_test!(&[0x61, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x81] = 0x01;
            vm.memory[0x82] = 0x09;
            vm.memory[0x901] = 1;
            vm.cpu.x = 1;
            vm.cpu.a = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 2)
    );

    cpu_test!(&[0x71, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x80] = 0x01;
            vm.memory[0x81] = 0x09;
            vm.memory[0x902] = 1;
            vm.cpu.y = 1;
            vm.cpu.a = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 2)
    );
}

#[test]
fn test_xor_imm() {
    cpu_test!(&[0x49, 0b01010101],
        init: |vm| vm.cpu.a = 0b00101010,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0x7F);
            assert_eq!(vm.cpu.flags, Flags::default());
    });
    cpu_test!(&[0x49, 0],
        init: |vm| vm.cpu.a = 0,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0);
            assert_eq!(vm.cpu.flags, Flags {zero: true, ..Flags::default()});
    });
    cpu_test!(&[0x49, 0x80],
        init: |vm| vm.cpu.a = 0x7F,
        exit: |vm| {
            assert_eq!(vm.cpu.a, 0xFF);
            assert_eq!(vm.cpu.flags, Flags {negative: true, ..Flags::default()});
    });
}

#[test]
fn test_xor_zp() {
    cpu_test!(&[0x45, 0xF1],
        init: |vm: &mut VM| {
            vm.memory[0xF1] = 0x80;
            vm.cpu.a = 0x7F;
        },
        exit: |vm: &mut VM| assert_eq!(vm.cpu.a, 0xFF)
    );
}

#[test]
fn test_xor_zpx() {
    cpu_test!(&[0x75, 0xF1],
        init: |vm| {
            vm.memory[0xF2] = 1;
            vm.cpu.a = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.cpu.a, 2)
    );
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
fn test_store_zp() {
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
fn test_store_abs() {
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
fn test_store_zpxy() {
    macro_rules! st_zpx_test {
        ($op:expr, $field:ident, $offset:ident) => {{
            cpu_test!(&[$op, 0xF1],
                init: |vm| {
                    vm.cpu.$field = 1;
                    vm.cpu.$offset = 1;
                },
                exit: |vm| assert_eq!(vm.memory[0xF2], 1)
            )
        }};
    }

    st_zpx_test!(0x95, a, x); // STA $zp,x
    st_zpx_test!(0x94, y, x); // STY $zp,x
    st_zpx_test!(0x96, x, y); // STX $zp,y
}

#[test]
fn test_store_indxy() {
    cpu_test!(&[0x81, 0x80],
        init: |vm: &mut VM| {
            vm.memory[0x81] = 0x01;
            vm.memory[0x82] = 0x09;
            vm.cpu.a = 1;
            vm.cpu.x = 1;
        },
        exit: |vm| assert_eq!(vm.memory[0x901], 1)
    );

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
fn test_store_absxy() {
    macro_rules! st_absxy_test {
        ($op:expr, $field:ident, $offset:ident) => {{
            cpu_test!(&[$op, 0x01, 0x08],
                init: |vm: &mut VM| {
                    vm.cpu.$offset = 1;
                    vm.cpu.$field = 1;
                },
                exit: |vm: &mut VM| {
                    assert_eq!(vm.memory[0x802], 1);
                    assert_eq!(vm.cpu.cycles, 5);
                }
            );
            cpu_test!(&[$op, 0xEE, 0x08],
                init: |vm: &mut VM| {
                    vm.cpu.$offset = 0x20;
                    vm.cpu.$field = 1;
                },
                exit: |vm: &mut VM| {
                    assert_eq!(vm.memory[0x90E], 1);
                    assert_eq!(vm.cpu.cycles, 5);
                }
            );
        }};
    }

    st_absxy_test!(0x9D, a, x);
    st_absxy_test!(0x99, a, y);
}

#[test]
fn test_jump_abs() {
    // 0x6C => instruction!(0x6C, &"JMP", 3, 5, AddressMode::Indirect),
    // JMP $abs
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
fn test_inc_imm() {
    cpu_test!(&[0xE6, 0xF1],
        init: |vm: &mut VM| vm.memory[0xF1] = 1,
        exit: |vm: &mut VM| assert_eq!(vm.memory[0xF1], 2)
    );
}

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

// #[test]
// fn test_jsr() {
//     let mut memory = [0u8; 0x300];
//     memory[0x2F0] = 0x20;
//     memory[0x2F1] = 0x56;
//     memory[0x2F2] = 0x04;
//     cpu_test!(&memory,
//         init: |vm| {
//             vm.cpu.pc = 0x2F0;
//             vm.cpu.pins.addr = vm.cpu.pc;
//             vm.cpu.pins.data = vm.memory[vm.cpu.pc as usize];
//         },
//         exit: |vm| {
//             assert_eq!(vm.memory[0x1FF], 0x2);
//             assert_eq!(vm.memory[0x1FE], 0xF1);
//             assert_eq!(vm.cpu.pc, 0x456);
//         }
//     );
// }
// #[test]
// fn test_dec_absx() {
//     cpu_test!(&[0xDE, 0x01, 0x08],
//         init: |vm: &mut VM| {
//             vm.memory[0x802] = 2;
//             vm.cpu.x = 1;
//         },
//         exit: |vm: &mut VM| assert_eq!(vm.memory[0x802], 1)
//     );
// }

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
fn test_bitwise() {
    // 0x45 => instruction!(0x45, &"EOR", 2, 3, AddressMode::ZeroPage),
    // 0x55 => instruction!(0x55, &"EOR", 2, 4, AddressMode::ZeroPageX),
    // 0x4D => instruction!(0x4D, &"EOR", 3, 4, AddressMode::Absolute),
    // 0x5D => instruction!(0x5D, &"EOR", 3, 5, AddressMode::AbsoluteX),
    // 0x59 => instruction!(0x59, &"EOR", 3, 5, AddressMode::AbsoluteY),
    // 0x41 => instruction!(0x41, &"EOR", 2, 6, AddressMode::IndirectX),
    // 0x51 => instruction!(0x51, &"EOR", 2, 5, AddressMode::IndirectY),
    // 0x09 => instruction!(0x09, &"ORA", 2, 2, AddressMode::Immediate),
    // 0x05 => instruction!(0x05, &"ORA", 2, 3, AddressMode::ZeroPage),
    // 0x15 => instruction!(0x15, &"ORA", 2, 4, AddressMode::ZeroPageX),
    // 0x0D => instruction!(0x0D, &"ORA", 3, 4, AddressMode::Absolute),
    // 0x1D => instruction!(0x1D, &"ORA", 3, 5, AddressMode::AbsoluteX),
    // 0x19 => instruction!(0x19, &"ORA", 3, 5, AddressMode::AbsoluteY),
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

#[test]
fn test_pc_increment() {
    cpu_test!(&[0xA9, 1, 0x8D, 0x1, 0x3, 0xA9, 2], |vm| {
        assert_eq!(vm.cpu.a, 2);
    });
}
