struct CPU {
    registers: Registers,
    //pc: u16,
    //sp: u16,
    //bus: MemoryBus,
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
    ADDHL(ArithmeticTarget),
    ADC(ArithmeticTarget),
    SUB(ArithmeticTarget),
    SBC(ArithmeticTarget),
    AND(ArithmeticTarget),
    OR(ArithmeticTarget),
    XOR(ArithmeticTarget),
    CP(ArithmeticTarget),
    INC(ArithmeticTarget),
    DEC(ArithmeticTarget),
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
}

enum ArithmeticTarget {
    A, B, C, D, E, H, L,
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

            Instruction::DEC(target) => {
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
        self.registers.f.half_carry = (self.registers.a & 0xF) + (1 & 0xF) > 0xF;

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
        self.registers.f.half_carry = (original <= 0xF) && (self.registers.a > 0xF);

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
        self.registers.f.half_carry = (original <= 0xF) && (self.registers.a > 0xF);

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
        self.registers.f.half_carry = (original <= 0xF) & (self.registers.a > 0xF);

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
        self.registers.f.half_carry = (original <= 0xF) & (self.registers.a > 0xF);

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


}

fn main() {

}
