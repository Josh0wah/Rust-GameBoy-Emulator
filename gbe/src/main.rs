struct CPU {
    registers: Registers,
    pc: u16,
    //sp: u16,
    bus: MemoryBus,
}

struct MemoryBus {
    memory: [u8; 0xFFFF]
}

impl MemoryBus {
    fn read_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }
}

struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,  //flags register
    h: u8,
    l: u8,
}

//2 byte virtual register implementation
impl Registers {
    //af virtual register
    fn get_af(&self) -> u16 {
        (self.a as  u16) << 8
        | u8::from(self.f) as u16
    }

    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((value & 0xFF) as u8);
    }

    //bc virtual register
    fn get_bc(&self) -> u16 {
        (self.b as  u16) << 8
        | self.c as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }

    //de virtual register
    fn get_de(&self) -> u16 {
        (self.d as  u16) << 8
        | self.e as u16
    }

    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }

    //hl virtual register
    fn get_hl(&self) -> u16 {
        (self.h as  u16) << 8
        | self.l as u16
    }

    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

/*impliment flags register
   ┌-> Carry (bit 4)
 ┌-+> Subtraction (bit 6)
 | |
1111 0000
| |
└-+> Zero (bit 7)
  └-> Half Carry (bit 5)
*/

//Creat struct for flags register
#[derive(Clone, Copy)]
struct FlagsRegister {
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool
}

// create consts for the positions of the four populatable bits of the flag register
const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

//implement conversion from flag reg to u8
impl std::convert::From<FlagsRegister> for u8 {
    fn from(flag: FlagsRegister) -> u8 {
        (if flag.zero        {1} else {0}) << ZERO_FLAG_BYTE_POSITION |
        (if flag.subtract    {1} else {0}) << SUBTRACT_FLAG_BYTE_POSITION |
        (if flag.half_carry  {1} else {0}) << HALF_CARRY_FLAG_BYTE_POSITION |
        (if flag.carry       {1} else {0}) << CARRY_FLAG_BYTE_POSITION
    }
}

//implement conversion from u8 to flags reg
impl std::convert::From<u8> for FlagsRegister {
    fn from(byte: u8) -> Self {
        let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
        let subtract = ((byte >> SUBTRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
        let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
        let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

        FlagsRegister {
            zero,
            subtract,
            half_carry,
            carry
        }
    }
}

//instructions
enum Instruction {
    ADD(ArithmeticTarget),
    ADDAF(ArithmeticTarget), //can be consolidated to 'add16' using VirtualTarget
    ADDBC(ArithmeticTarget),
    ADDDE(ArithmeticTarget),
    ADDHL(ArithmeticTarget),
    ADC(ArithmeticTarget),
    SUB(ArithmeticTarget),
    SBC(ArithmeticTarget),
    AND(ArithmeticTarget),
    OR(ArithmeticTarget),
    XOR(ArithmeticTarget),
    CP(ArithmeticTarget),
    INC(ArithmeticTarget),
    INC16(VirtualTarget),
    DEC(ArithmeticTarget),
    DEC16(VirtualTarget),
    CCF(),
    SCF(),
    RRA(),
    RLA(),
    RRCA(),
    RRLA(),
    CPL(),
    BIT(ArithmeticTarget, u8),
    RESET(ArithmeticTarget, u8),
    SET(ArithmeticTarget, u8),
    SRL(ArithmeticTarget),
    RR(ArithmeticTarget),
    RL(ArithmeticTarget),
    RRC(ArithmeticTarget),
    RLC(ArithmeticTarget),
    SRA(ArithmeticTarget),
    SLA(ArithmeticTarget),
    SWAP(ArithmeticTarget),
}

enum ArithmeticTarget {
    A, B, C, D, E, H, L,
}


enum VirtualTarget {
    AF, BC, DE, HL,
}

//convert hex into executable instructions
impl Instruction {
    fn from_byte (byte: u8) -> Option<Instruction> {
        match byte {
            //TODO: map remaining instructions
            _ => None
        }
    }
}

//step through pc
impl CPU {
    fn step(&mut self) {
        let mut instruction_byte = self.bus.read_byte(self.pc);

        let next_pc = if let Some(instruction) = Instruction::from_byte(instruction_byte) {
            self.execute(instruction)
        }
        else {
            panic!("Unknown instruction found for: 0x{:x}", instruction_byte);
        };

        self.pc = next_pc;
    }
}

//implement instruction execution by pattern matching instruction and target
impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADD(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::ADDAF(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.addaf(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::ADDBC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.addbc(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::ADDDE(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.addde(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::ADDHL(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.addhl(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::ADC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.add(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.adc(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::SUB(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.sub(value);

                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.sub(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::SBC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.sbc(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::AND(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.and(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::OR(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.or(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::XOR(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let value = self.registers.b;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::C => {
                        let value = self.registers.c;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::D => {
                        let value = self.registers.d;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::E => {
                        let value = self.registers.e;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::H => {
                        let value = self.registers.h;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::L => {
                        let value = self.registers.l;
                        let new_value = self.xor(value);
                        self.registers.a = new_value;
                    }
                }
            }

            Instruction::CP(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let value = self.registers.a;
                        let new_value = self.cp(value);
                        self.registers.a = new_value;
                    }
                    _ => {
                            panic!("Invalid target")
                        }
                }
            }

            Instruction::INC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.inc('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.inc('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.inc('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.inc('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.inc('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.inc('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.inc('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::INC16(target) => {
                match target {
                    VirtualTarget::AF => {
                        let new_value = self.inc16("af");
                        self.registers.set_af(new_value);
                    }
                    VirtualTarget::BC => {
                        let new_value = self.inc16("bc");
                        self.registers.set_bc(new_value);
                    }
                    VirtualTarget::DE => {
                        let new_value = self.inc16("de");
                        self.registers.set_de(new_value);
                    }
                    VirtualTarget::HL => {
                        let new_value = self.inc16("hl");
                        self.registers.set_hl(new_value);
                    }
                }
            }

            Instruction::DEC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.dec('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.dec('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.dec('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.dec('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.dec('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.dec('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.dec('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::DEC16(target) => {
                match target {
                    VirtualTarget::AF => {
                        let new_value = self.dec16("af");
                        self.registers.set_af(new_value);
                    }
                    VirtualTarget::BC => {
                        let new_value = self.dec16("bc");
                        self.registers.set_bc(new_value);
                    }
                    VirtualTarget::DE => {
                        let new_value = self.dec16("de");
                        self.registers.set_de(new_value);
                    }
                    VirtualTarget::HL => {
                        let new_value = self.dec16("hl");
                        self.registers.set_hl(new_value);
                    }
                }
            }

            Instruction::CCF() => {
                self.ccf();
            }

            Instruction::SCF() => {
                self.scf();
            }

            Instruction::RRA() => {
                let new_value = self.rra();
                self.registers.a = new_value;
            }

            Instruction::RLA() => {
                let new_value = self.rla();
                self.registers.a = new_value;
            }

            Instruction::RRCA() => {
                let new_value = self.rrca();
                self.registers.a = new_value;
            }

            Instruction::RRLA() => {
                let new_value = self.rrla();
                self.registers.a = new_value;
            }

            Instruction::CPL() => {
                let new_value = self.cpl();
                self.registers.a = new_value;
            }

            Instruction::BIT(target, bit_idx) => {
                match target {
                    ArithmeticTarget::A => {
                        self.bit('a', bit_idx);
                    }
                    ArithmeticTarget::B => {
                        self.bit('b', bit_idx);
                    }
                    ArithmeticTarget::C => {
                        self.bit('c', bit_idx);
                    }
                    ArithmeticTarget::D => {
                        self.bit('d', bit_idx);
                    }
                    ArithmeticTarget::E => {
                        self.bit('e', bit_idx);
                    }
                    ArithmeticTarget::H => {
                        self.bit('h', bit_idx);
                    }
                    ArithmeticTarget::L => {
                        self.bit('l', bit_idx);
                    }
                }
            }

            Instruction::RESET(target, bit_idx) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.reset('a', bit_idx);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.reset('b', bit_idx);
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.reset('c', bit_idx);
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.reset('d', bit_idx);
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.reset('e', bit_idx);
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.reset('h', bit_idx);
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.reset('l', bit_idx);
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::SET(target, bit_idx) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.reset('a', bit_idx);
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.reset('b', bit_idx);
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.reset('c', bit_idx);
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.reset('d', bit_idx);
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.reset('e', bit_idx);
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.reset('h', bit_idx);
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.reset('l', bit_idx);
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::SRL(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.srl('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.srl('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.srl('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.srl('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.srl('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.srl('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.srl('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::RR(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.rr('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.rr('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.rr('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.rr('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.rr('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.rr('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.rr('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::RL(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.rl('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.rl('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.rl('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.rl('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.rl('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.rl('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.rl('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::RRC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.rrc('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.rrc('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.rrc('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.rrc('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.rrc('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.rrc('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.rrc('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::RLC(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.rlc('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.rlc('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.rlc('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.rlc('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.rlc('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.rlc('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.rlc('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::SRA(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.sra('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.sra('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.sra('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.sra('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.sra('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.sra('h');

                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.sra('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::SLA(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.sla('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.sla('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.sla('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.sla('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.sla('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.sla('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.sla('l');
                        self.registers.l = new_value;
                    }
                }
            }

            Instruction::SWAP(target) => {
                match target {
                    ArithmeticTarget::A => {
                        let new_value = self.swap('a');
                        self.registers.a = new_value;
                    }
                    ArithmeticTarget::B => {
                        let new_value = self.swap('b');
                        self.registers.b = new_value;
                    }
                    ArithmeticTarget::C => {
                        let new_value = self.swap('c');
                        self.registers.c = new_value;
                    }
                    ArithmeticTarget::D => {
                        let new_value = self.swap('d');
                        self.registers.d = new_value;
                    }
                    ArithmeticTarget::E => {
                        let new_value = self.swap('e');
                        self.registers.e = new_value;
                    }
                    ArithmeticTarget::H => {
                        let new_value = self.swap('h');
                        self.registers.h = new_value;
                    }
                    ArithmeticTarget::L => {
                        let new_value = self.swap('l');
                        self.registers.l = new_value;
                    }
                }
            }

        }
    }

    //add function
    //adds target register to A
    fn add(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.a.overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value
    }


    //addde function
    //adds target register to DE
    fn addde(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.get_de().overflowing_add(value.into());

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value.try_into().unwrap()
    }

    //addbc function
    //adds target register to BC
    fn addbc(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.get_bc().overflowing_add(value.into());

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value.try_into().unwrap()
    }

    //addaf function
    //adds target register to AF
    fn addaf(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.get_af().overflowing_add(value.into());

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value.try_into().unwrap()
    }

    //addhl function
    //adds tatget register to HL
    fn addhl(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value.into());

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value.try_into().unwrap()
    }

    //adc function
    //adds target register to A, then adds carry flag value
    fn adc(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.a.overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        if self.registers.f.carry == true {
            let new_value = self.registers.a.overflowing_add(1);
        }

        new_value
    }

    //sub function
    //subtracts target register from a
    fn sub(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        //check if the result of subtracting the lower 4 bits of the result of subtracting A and value is greater than 0xF
        self.registers.f.half_carry = false;    //unsure

        new_value
    }

    //sbc function
    //subtracts target register with A, then subtracts carry flag value
    fn sbc(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        //check if the result of subtracting the lower 4 bits of the result of subtracting A and value is greater than 0xF
        self.registers.f.half_carry = false;    //unsure

        if self.registers.f.carry == true {
            let new_value = self.registers.a.overflowing_sub(1);
        }

        new_value
    }

    //and function
    //bitwise AND on target register and A
    fn and(&mut self, value: u8) -> u8 {
        let new_value = self.registers.a & value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        new_value
    }

//or function
//bitwise OR on target register and A
    fn or(&mut self, value: u8) -> u8 {
        let new_value = self.registers.a | value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        new_value
    }

//xor function
//bitwise XOR on target register and A
    fn xor(&mut self, value: u8) -> u8 {
        let new_value = self.registers.a ^ value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        new_value
    }

    //cp function
    //compares target register to A by subtraction
    fn cp(&mut self, value: u8) -> u8 {
        let (new_value, did_overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = false;    //unsure

        new_value
    }

    //inc function
    //increments target register by 1
    fn inc(&mut self, target_register: char) -> u8 {
        let value: u8 = 1;

        let (new_value, did_overflow) = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        }.overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding target registrer and value (1) is greater than 0xF
        self.registers.f.half_carry = (new_value & 0xF) + (1 & 0xF) > 0xF;

        new_value
    }

    //inc16 function
    //increments target virtual register by 1
    fn inc16(&mut self, target_register: &str) -> u16 {
        let value: u16 = 1;

        let (new_value, did_overflow) = match target_register{
            "af" => self.registers.get_af(),
            "bc" => self.registers.get_bc(),
            "de" => self.registers.get_de(),
            "hl" => self.registers.get_hl(),
            _ => panic!("unknown register value")
        }.overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 8 bits of the result of adding target registrer and value (1) is greater than 0xFF
        self.registers.f.half_carry = (new_value & 0xFF) + (1 & 0xFF) > 0xFF;

        new_value
    }

    //dec function
    //decrements target register by 1
    fn dec(&mut self, target_register: char) -> u8 {
        let value: u8 = 1;

        let (new_value, did_overflow) = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        }.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = false;

        new_value
    }

    //dec16 function
    //decrements target virtual register by 1
    fn dec16(&mut self, target_register: &str) -> u16 {
        let value: u16 = 1;

        let (new_value, did_overflow) = match target_register{
            "af" => self.registers.get_af(),
            "bc" => self.registers.get_bc(),
            "de" => self.registers.get_de(),
            "hl" => self.registers.get_hl(),
            _ => panic!("unknown register value")
        }.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = false;

        new_value
    }

    //ccf function
    //switches carry flag
    fn ccf(&mut self) {
        self.registers.f.carry = self.registers.f.carry == false;
    }

    //scf function
    //sets carry flag to true
    fn scf(&mut self) {
        self.registers.f.carry = true;
    }

    //rra function
    //rotates A right through carry flag
    fn rra(&mut self) -> u8 {
        let original = self.registers.a;

        let new_value = self.registers.a.rotate_right(1);

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (original <= 0xF) && (new_value > 0xF);

        new_value
    }

    //rla function
    //rotates A left through carry flag
    fn rla(&mut self) -> u8 {
        let original = self.registers.a;

        let new_value = self.registers.a.rotate_right(1);

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (original <= 0xF) && (new_value > 0xF);

        new_value
    }

    //rrca function
    //rotates A right (not through carry flag)
    fn rrca(&mut self) -> u8 {
        let original = self.registers.a;

        let out_bit_set: bool = (self.registers.a & 0x01) != 0; // true if last bit != 0

        let new_value = self.registers.a.rotate_right(1);

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (original <= 0xF) & (new_value > 0xF);

        match out_bit_set {
            true =>
                new_value | 0x80,
            false =>
                new_value,
        }

    }

    //rrla function
    //rotates A left (not through carry flag)
    fn rrla(&mut self) -> u8 {
        let original = self.registers.a;

        let out_bit_set: bool = (self.registers.a & 0x80) != 0; // true if first bit != 0

        let new_value = self.registers.a.rotate_left(1);

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (original <= 0xF) & (new_value > 0xF);

        match out_bit_set {
            true =>
                new_value | 0x01,
            false =>
                new_value,
        }

    }

    //cpl function
    //toggles each bit in A
    fn cpl(&mut self) -> u8 {
        let original = self.registers.a;

        let new_value: u8 = !self.registers.a;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (original <= 0xF) & (self.registers.a > 0xF);

        new_value
    }

    //bit function
    //checks if a bit is set
    fn bit(&mut self, target_register: char, bit_idx: u8) {
        let mask: u8 = 1 << bit_idx;

        let  byte_to_check = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        self.registers.f.zero = (byte_to_check & mask) != 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;
    }

    //reset function
    //resets a given bit to 0
    fn reset(&mut self, target_register: char, bit_idx: u8) -> u8 {
        let mask: u8 = 0b1111_1110 << bit_idx;

        let byte_to_reset = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let bit_reset = byte_to_reset & mask;

        self.registers.f.zero = bit_reset == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;

        bit_reset
    }

    //set function
    //sets a given bit to 1
    fn set(&mut self, target_register: char, bit_idx: u8) -> u8 {
        let mask: u8 = 1 << bit_idx;

        let byte_to_set = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let bit_set = byte_to_set | mask;

        self.registers.f.zero = false;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (byte_to_set <= 0xF) && (bit_set > 0xF);

        bit_set
    }

    //srl function
    //shifts a given register right by 1
    fn srl(&mut self, target_register: char) -> u8 {
        let byte_to_shift = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let byte_shifted = byte_to_shift >> 1;

        self.registers.f.zero = byte_shifted == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (byte_to_shift <= 0xF) && (byte_shifted > 0xF);

        byte_shifted
    }

    //rr function
    //rotates given register right through carry flag
    fn rr(&mut self, target_register: char) -> u8 {
        let register_to_rotate = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };


        let new_value = register_to_rotate.rotate_right(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (register_to_rotate <= 0xF) && (new_value > 0xF);

        new_value
    }

    //rl function
    //rotates given register left through carry flag
    fn rl(&mut self, target_register: char) -> u8 {
        let register_to_rotate = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };


        let new_value = register_to_rotate.rotate_left(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (register_to_rotate <= 0xF) && (new_value > 0xF);

        new_value
    }

    //rrc function
    //rotates a given register right (not through carry flag)
    fn rrc(&mut self, target_register: char) -> u8 {
        let register_to_rotate = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let out_bit_set: bool = (register_to_rotate & 0x01) != 0; // true if last bit != 0

        let new_value = register_to_rotate.rotate_right(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (register_to_rotate <= 0xF) & (new_value > 0xF);

        match out_bit_set {
            true =>
                new_value | 0x80,
            false =>
                new_value,
        }
    }

    //rlc function
    //rotates a given register left (not through carry flag)
    fn rlc(&mut self, target_register: char) -> u8 {
        let register_to_rotate = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let out_bit_set: bool = (register_to_rotate & 0x80) != 0; // true if last bit != 0

        let new_value = register_to_rotate.rotate_left(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (register_to_rotate <= 0xF) & (new_value > 0xF);

        match out_bit_set {
            true =>
                new_value | 0x01,
            false =>
                new_value,
        }
    }

    //sra function
    //Arithmetic shift right
    fn sra(&mut self, target_register: char) -> u8 {
        let target = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let mask = target & 0x80;

        let new_value = (target >> 1) | mask;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (target <= 0xF) & (new_value > 0xF);

        new_value
    }

    //sla function
    //Arithmetic shift left
    fn sla(&mut self, target_register: char) -> u8 {
        let target = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let mask = target & 0x80;

        let new_value = (target << 1) & mask;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (target <= 0xF) & (new_value > 0xF);

        new_value
    }

    //swap function
    //Swap nibbles in a byte
    fn swap(&mut self, target_register: char) -> u8 {
        let target = match target_register{
            'a' => self.registers.a,
            'b' => self.registers.b,
            'c' => self.registers.c,
            'd' => self.registers.d,
            'e' => self.registers.e,
            'h' => self.registers.h,
            'l' => self.registers.l,
            _ => panic!("unknown register value")
        };

        let new_value = (target & 0x0F) << 4 | (target & 0xF0) >> 4;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = (target <= 0xF) & (new_value > 0xF);

        new_value
    }

}

fn main() {

}
