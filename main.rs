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
    ADDHL(VirtualTarget),
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
            // 8-bit Load Instructions
            0x7F => Some(Instruction::ADD(ArithmeticTarget::A)), // LD A,A
            0x78 => Some(Instruction::ADD(ArithmeticTarget::B)), // LD A,B
            0x79 => Some(Instruction::ADD(ArithmeticTarget::C)), // LD A,C
            0x7A => Some(Instruction::ADD(ArithmeticTarget::D)), // LD A,D
            0x7B => Some(Instruction::ADD(ArithmeticTarget::E)), // LD A,E
            0x7C => Some(Instruction::ADD(ArithmeticTarget::H)), // LD A,H
            0x7D => Some(Instruction::ADD(ArithmeticTarget::L)), // LD A,L
            
            // 8-bit Arithmetic Instructions
            0x80 => Some(Instruction::ADD(ArithmeticTarget::B)), // ADD A,B
            0x81 => Some(Instruction::ADD(ArithmeticTarget::C)), // ADD A,C
            0x82 => Some(Instruction::ADD(ArithmeticTarget::D)), // ADD A,D
            0x83 => Some(Instruction::ADD(ArithmeticTarget::E)), // ADD A,E
            0x84 => Some(Instruction::ADD(ArithmeticTarget::H)), // ADD A,H
            0x85 => Some(Instruction::ADD(ArithmeticTarget::L)), // ADD A,L
            0x87 => Some(Instruction::ADD(ArithmeticTarget::A)), // ADD A,A
            
            0x88 => Some(Instruction::ADC(ArithmeticTarget::B)), // ADC A,B
            0x89 => Some(Instruction::ADC(ArithmeticTarget::C)), // ADC A,C
            0x8A => Some(Instruction::ADC(ArithmeticTarget::D)), // ADC A,D
            0x8B => Some(Instruction::ADC(ArithmeticTarget::E)), // ADC A,E
            0x8C => Some(Instruction::ADC(ArithmeticTarget::H)), // ADC A,H
            0x8D => Some(Instruction::ADC(ArithmeticTarget::L)), // ADC A,L
            0x8F => Some(Instruction::ADC(ArithmeticTarget::A)), // ADC A,A
            
            0x90 => Some(Instruction::SUB(ArithmeticTarget::B)), // SUB A,B
            0x91 => Some(Instruction::SUB(ArithmeticTarget::C)), // SUB A,C
            0x92 => Some(Instruction::SUB(ArithmeticTarget::D)), // SUB A,D
            0x93 => Some(Instruction::SUB(ArithmeticTarget::E)), // SUB A,E
            0x94 => Some(Instruction::SUB(ArithmeticTarget::H)), // SUB A,H
            0x95 => Some(Instruction::SUB(ArithmeticTarget::L)), // SUB A,L
            0x97 => Some(Instruction::SUB(ArithmeticTarget::A)), // SUB A,A
            
            0x98 => Some(Instruction::SBC(ArithmeticTarget::B)), // SBC A,B
            0x99 => Some(Instruction::SBC(ArithmeticTarget::C)), // SBC A,C
            0x9A => Some(Instruction::SBC(ArithmeticTarget::D)), // SBC A,D
            0x9B => Some(Instruction::SBC(ArithmeticTarget::E)), // SBC A,E
            0x9C => Some(Instruction::SBC(ArithmeticTarget::H)), // SBC A,H
            0x9D => Some(Instruction::SBC(ArithmeticTarget::L)), // SBC A,L
            0x9F => Some(Instruction::SBC(ArithmeticTarget::A)), // SBC A,A
            
            0xA0 => Some(Instruction::AND(ArithmeticTarget::B)), // AND A,B
            0xA1 => Some(Instruction::AND(ArithmeticTarget::C)), // AND A,C
            0xA2 => Some(Instruction::AND(ArithmeticTarget::D)), // AND A,D
            0xA3 => Some(Instruction::AND(ArithmeticTarget::E)), // AND A,E
            0xA4 => Some(Instruction::AND(ArithmeticTarget::H)), // AND A,H
            0xA5 => Some(Instruction::AND(ArithmeticTarget::L)), // AND A,L
            0xA7 => Some(Instruction::AND(ArithmeticTarget::A)), // AND A,A
            
            0xB0 => Some(Instruction::OR(ArithmeticTarget::B)),  // OR A,B
            0xB1 => Some(Instruction::OR(ArithmeticTarget::C)),  // OR A,C
            0xB2 => Some(Instruction::OR(ArithmeticTarget::D)),  // OR A,D
            0xB3 => Some(Instruction::OR(ArithmeticTarget::E)),  // OR A,E
            0xB4 => Some(Instruction::OR(ArithmeticTarget::H)),  // OR A,H
            0xB5 => Some(Instruction::OR(ArithmeticTarget::L)),  // OR A,L
            0xB7 => Some(Instruction::OR(ArithmeticTarget::A)),  // OR A,A
            
            0xA8 => Some(Instruction::XOR(ArithmeticTarget::B)), // XOR A,B
            0xA9 => Some(Instruction::XOR(ArithmeticTarget::C)), // XOR A,C
            0xAA => Some(Instruction::XOR(ArithmeticTarget::D)), // XOR A,D
            0xAB => Some(Instruction::XOR(ArithmeticTarget::E)), // XOR A,E
            0xAC => Some(Instruction::XOR(ArithmeticTarget::H)), // XOR A,H
            0xAD => Some(Instruction::XOR(ArithmeticTarget::L)), // XOR A,L
            0xAF => Some(Instruction::XOR(ArithmeticTarget::A)), // XOR A,A
            
            0xB8 => Some(Instruction::CP(ArithmeticTarget::B)),  // CP A,B
            0xB9 => Some(Instruction::CP(ArithmeticTarget::C)),  // CP A,C
            0xBA => Some(Instruction::CP(ArithmeticTarget::D)),  // CP A,D
            0xBB => Some(Instruction::CP(ArithmeticTarget::E)),  // CP A,E
            0xBC => Some(Instruction::CP(ArithmeticTarget::H)),  // CP A,H
            0xBD => Some(Instruction::CP(ArithmeticTarget::L)),  // CP A,L
            0xBF => Some(Instruction::CP(ArithmeticTarget::A)),  // CP A,A
            
            // Increment/Decrement Instructions
            0x3C => Some(Instruction::INC(ArithmeticTarget::A)), // INC A
            0x04 => Some(Instruction::INC(ArithmeticTarget::B)), // INC B
            0x0C => Some(Instruction::INC(ArithmeticTarget::C)), // INC C
            0x14 => Some(Instruction::INC(ArithmeticTarget::D)), // INC D
            0x1C => Some(Instruction::INC(ArithmeticTarget::E)), // INC E
            0x24 => Some(Instruction::INC(ArithmeticTarget::H)), // INC H
            0x2C => Some(Instruction::INC(ArithmeticTarget::L)), // INC L
            
            0x3D => Some(Instruction::DEC(ArithmeticTarget::A)), // DEC A
            0x05 => Some(Instruction::DEC(ArithmeticTarget::B)), // DEC B
            0x0D => Some(Instruction::DEC(ArithmeticTarget::C)), // DEC C
            0x15 => Some(Instruction::DEC(ArithmeticTarget::D)), // DEC D
            0x1D => Some(Instruction::DEC(ArithmeticTarget::E)), // DEC E
            0x25 => Some(Instruction::DEC(ArithmeticTarget::H)), // DEC H
            0x2D => Some(Instruction::DEC(ArithmeticTarget::L)), // DEC L
            
            // 16-bit Arithmetic Instructions
            0x09 => Some(Instruction::ADDHL(VirtualTarget::BC)), // ADD HL,BC
            0x19 => Some(Instruction::ADDHL(VirtualTarget::DE)), // ADD HL,DE
            0x29 => Some(Instruction::ADDHL(VirtualTarget::HL)), // ADD HL,HL
            0x39 => Some(Instruction::ADDHL(VirtualTarget::AF)), // ADD HL,SP
            
            0x03 => Some(Instruction::INC16(VirtualTarget::BC)), // INC BC
            0x13 => Some(Instruction::INC16(VirtualTarget::DE)), // INC DE
            0x23 => Some(Instruction::INC16(VirtualTarget::HL)), // INC HL
            0x33 => Some(Instruction::INC16(VirtualTarget::AF)), // INC SP
            
            0x0B => Some(Instruction::DEC16(VirtualTarget::BC)), // DEC BC
            0x1B => Some(Instruction::DEC16(VirtualTarget::DE)), // DEC DE
            0x2B => Some(Instruction::DEC16(VirtualTarget::HL)), // DEC HL
            0x3B => Some(Instruction::DEC16(VirtualTarget::AF)), // DEC SP
            
            // Flag Instructions
            0x3F => Some(Instruction::CCF()), // CCF
            0x37 => Some(Instruction::SCF()), // SCF
            0x2F => Some(Instruction::CPL()), // CPL
            
            // Rotate Instructions
            0x1F => Some(Instruction::RRA()), // RRA
            0x17 => Some(Instruction::RLA()), // RLA
            0x0F => Some(Instruction::RRCA()), // RRCA
            0x07 => Some(Instruction::RRLA()), // RRLA
            
            // Bit Operations (CB prefix instructions would need to be handled separately)
            // For now, we'll include some common ones as examples
            0xCB => {
                // This would need to read the next byte for CB prefixed instructions
                // For now, return None to indicate we need to handle this case
                None
            }
            
            // Shift and Rotate Instructions (CB prefixed)
            // These would be handled when we see 0xCB followed by another byte
            // For now, we'll include some examples:
            0x38 => Some(Instruction::SRL(ArithmeticTarget::B)), // SRL B (CB 38)
            0x39 => Some(Instruction::SRL(ArithmeticTarget::C)), // SRL C (CB 39)
            0x3A => Some(Instruction::SRL(ArithmeticTarget::D)), // SRL D (CB 3A)
            0x3B => Some(Instruction::SRL(ArithmeticTarget::E)), // SRL E (CB 3B)
            0x3C => Some(Instruction::SRL(ArithmeticTarget::H)), // SRL H (CB 3C)
            0x3D => Some(Instruction::SRL(ArithmeticTarget::L)), // SRL L (CB 3D)
            0x3F => Some(Instruction::SRL(ArithmeticTarget::A)), // SRL A (CB 3F)
            
            0x18 => Some(Instruction::RR(ArithmeticTarget::B)),  // RR B (CB 18)
            0x19 => Some(Instruction::RR(ArithmeticTarget::C)),  // RR C (CB 19)
            0x1A => Some(Instruction::RR(ArithmeticTarget::D)),  // RR D (CB 1A)
            0x1B => Some(Instruction::RR(ArithmeticTarget::E)),  // RR E (CB 1B)
            0x1C => Some(Instruction::RR(ArithmeticTarget::H)),  // RR H (CB 1C)
            0x1D => Some(Instruction::RR(ArithmeticTarget::L)),  // RR L (CB 1D)
            0x1F => Some(Instruction::RR(ArithmeticTarget::A)),  // RR A (CB 1F)
            
            0x10 => Some(Instruction::RL(ArithmeticTarget::B)),  // RL B (CB 10)
            0x11 => Some(Instruction::RL(ArithmeticTarget::C)),  // RL C (CB 11)
            0x12 => Some(Instruction::RL(ArithmeticTarget::D)),  // RL D (CB 12)
            0x13 => Some(Instruction::RL(ArithmeticTarget::E)),  // RL E (CB 13)
            0x14 => Some(Instruction::RL(ArithmeticTarget::H)),  // RL H (CB 14)
            0x15 => Some(Instruction::RL(ArithmeticTarget::L)),  // RL L (CB 15)
            0x17 => Some(Instruction::RL(ArithmeticTarget::A)),  // RL A (CB 17)
            
            0x08 => Some(Instruction::RRC(ArithmeticTarget::B)), // RRC B (CB 08)
            0x09 => Some(Instruction::RRC(ArithmeticTarget::C)), // RRC C (CB 09)
            0x0A => Some(Instruction::RRC(ArithmeticTarget::D)), // RRC D (CB 0A)
            0x0B => Some(Instruction::RRC(ArithmeticTarget::E)), // RRC E (CB 0B)
            0x0C => Some(Instruction::RRC(ArithmeticTarget::H)), // RRC H (CB 0C)
            0x0D => Some(Instruction::RRC(ArithmeticTarget::L)), // RRC L (CB 0D)
            0x0F => Some(Instruction::RRC(ArithmeticTarget::A)), // RRC A (CB 0F)
            
            0x00 => Some(Instruction::RLC(ArithmeticTarget::B)), // RLC B (CB 00)
            0x01 => Some(Instruction::RLC(ArithmeticTarget::C)), // RLC C (CB 01)
            0x02 => Some(Instruction::RLC(ArithmeticTarget::D)), // RLC D (CB 02)
            0x03 => Some(Instruction::RLC(ArithmeticTarget::E)), // RLC E (CB 03)
            0x04 => Some(Instruction::RLC(ArithmeticTarget::H)), // RLC H (CB 04)
            0x05 => Some(Instruction::RLC(ArithmeticTarget::L)), // RLC L (CB 05)
            0x07 => Some(Instruction::RLC(ArithmeticTarget::A)), // RLC A (CB 07)
            
            0x28 => Some(Instruction::SRA(ArithmeticTarget::B)), // SRA B (CB 28)
            0x29 => Some(Instruction::SRA(ArithmeticTarget::C)), // SRA C (CB 29)
            0x2A => Some(Instruction::SRA(ArithmeticTarget::D)), // SRA D (CB 2A)
            0x2B => Some(Instruction::SRA(ArithmeticTarget::E)), // SRA E (CB 2B)
            0x2C => Some(Instruction::SRA(ArithmeticTarget::H)), // SRA H (CB 2C)
            0x2D => Some(Instruction::SRA(ArithmeticTarget::L)), // SRA L (CB 2D)
            0x2F => Some(Instruction::SRA(ArithmeticTarget::A)), // SRA A (CB 2F)
            
            0x20 => Some(Instruction::SLA(ArithmeticTarget::B)), // SLA B (CB 20)
            0x21 => Some(Instruction::SLA(ArithmeticTarget::C)), // SLA C (CB 21)
            0x22 => Some(Instruction::SLA(ArithmeticTarget::D)), // SLA D (CB 22)
            0x23 => Some(Instruction::SLA(ArithmeticTarget::E)), // SLA E (CB 23)
            0x24 => Some(Instruction::SLA(ArithmeticTarget::H)), // SLA H (CB 24)
            0x25 => Some(Instruction::SLA(ArithmeticTarget::L)), // SLA L (CB 25)
            0x27 => Some(Instruction::SLA(ArithmeticTarget::A)), // SLA A (CB 27)
            
            0x30 => Some(Instruction::SWAP(ArithmeticTarget::B)), // SWAP B (CB 30)
            0x31 => Some(Instruction::SWAP(ArithmeticTarget::C)), // SWAP C (CB 31)
            0x32 => Some(Instruction::SWAP(ArithmeticTarget::D)), // SWAP D (CB 32)
            0x33 => Some(Instruction::SWAP(ArithmeticTarget::E)), // SWAP E (CB 33)
            0x34 => Some(Instruction::SWAP(ArithmeticTarget::H)), // SWAP H (CB 34)
            0x35 => Some(Instruction::SWAP(ArithmeticTarget::L)), // SWAP L (CB 35)
            0x37 => Some(Instruction::SWAP(ArithmeticTarget::A)), // SWAP A (CB 37)
            
            // Bit Test Instructions (CB prefixed)
            0x40 => Some(Instruction::BIT(ArithmeticTarget::B, 0)), // BIT 0,B (CB 40)
            0x41 => Some(Instruction::BIT(ArithmeticTarget::C, 0)), // BIT 0,C (CB 41)
            0x42 => Some(Instruction::BIT(ArithmeticTarget::D, 0)), // BIT 0,D (CB 42)
            0x43 => Some(Instruction::BIT(ArithmeticTarget::E, 0)), // BIT 0,E (CB 43)
            0x44 => Some(Instruction::BIT(ArithmeticTarget::H, 0)), // BIT 0,H (CB 44)
            0x45 => Some(Instruction::BIT(ArithmeticTarget::L, 0)), // BIT 0,L (CB 45)
            0x47 => Some(Instruction::BIT(ArithmeticTarget::A, 0)), // BIT 0,A (CB 47)
            
            0x48 => Some(Instruction::BIT(ArithmeticTarget::B, 1)), // BIT 1,B (CB 48)
            0x49 => Some(Instruction::BIT(ArithmeticTarget::C, 1)), // BIT 1,C (CB 49)
            0x4A => Some(Instruction::BIT(ArithmeticTarget::D, 1)), // BIT 1,D (CB 4A)
            0x4B => Some(Instruction::BIT(ArithmeticTarget::E, 1)), // BIT 1,E (CB 4B)
            0x4C => Some(Instruction::BIT(ArithmeticTarget::H, 1)), // BIT 1,H (CB 4C)
            0x4D => Some(Instruction::BIT(ArithmeticTarget::L, 1)), // BIT 1,L (CB 4D)
            0x4F => Some(Instruction::BIT(ArithmeticTarget::A, 1)), // BIT 1,A (CB 4F)
            
            0x50 => Some(Instruction::BIT(ArithmeticTarget::B, 2)), // BIT 2,B (CB 50)
            0x51 => Some(Instruction::BIT(ArithmeticTarget::C, 2)), // BIT 2,C (CB 51)
            0x52 => Some(Instruction::BIT(ArithmeticTarget::D, 2)), // BIT 2,D (CB 52)
            0x53 => Some(Instruction::BIT(ArithmeticTarget::E, 2)), // BIT 2,E (CB 53)
            0x54 => Some(Instruction::BIT(ArithmeticTarget::H, 2)), // BIT 2,H (CB 54)
            0x55 => Some(Instruction::BIT(ArithmeticTarget::L, 2)), // BIT 2,L (CB 55)
            0x57 => Some(Instruction::BIT(ArithmeticTarget::A, 2)), // BIT 2,A (CB 57)
            
            0x58 => Some(Instruction::BIT(ArithmeticTarget::B, 3)), // BIT 3,B (CB 58)
            0x59 => Some(Instruction::BIT(ArithmeticTarget::C, 3)), // BIT 3,C (CB 59)
            0x5A => Some(Instruction::BIT(ArithmeticTarget::D, 3)), // BIT 3,D (CB 5A)
            0x5B => Some(Instruction::BIT(ArithmeticTarget::E, 3)), // BIT 3,E (CB 5B)
            0x5C => Some(Instruction::BIT(ArithmeticTarget::H, 3)), // BIT 3,H (CB 5C)
            0x5D => Some(Instruction::BIT(ArithmeticTarget::L, 3)), // BIT 3,L (CB 5D)
            0x5F => Some(Instruction::BIT(ArithmeticTarget::A, 3)), // BIT 3,A (CB 5F)
            
            0x60 => Some(Instruction::BIT(ArithmeticTarget::B, 4)), // BIT 4,B (CB 60)
            0x61 => Some(Instruction::BIT(ArithmeticTarget::C, 4)), // BIT 4,C (CB 61)
            0x62 => Some(Instruction::BIT(ArithmeticTarget::D, 4)), // BIT 4,D (CB 62)
            0x63 => Some(Instruction::BIT(ArithmeticTarget::E, 4)), // BIT 4,E (CB 63)
            0x64 => Some(Instruction::BIT(ArithmeticTarget::H, 4)), // BIT 4,H (CB 64)
            0x65 => Some(Instruction::BIT(ArithmeticTarget::L, 4)), // BIT 4,L (CB 65)
            0x67 => Some(Instruction::BIT(ArithmeticTarget::A, 4)), // BIT 4,A (CB 67)
            
            0x68 => Some(Instruction::BIT(ArithmeticTarget::B, 5)), // BIT 5,B (CB 68)
            0x69 => Some(Instruction::BIT(ArithmeticTarget::C, 5)), // BIT 5,C (CB 69)
            0x6A => Some(Instruction::BIT(ArithmeticTarget::D, 5)), // BIT 5,D (CB 6A)
            0x6B => Some(Instruction::BIT(ArithmeticTarget::E, 5)), // BIT 5,E (CB 6B)
            0x6C => Some(Instruction::BIT(ArithmeticTarget::H, 5)), // BIT 5,H (CB 6C)
            0x6D => Some(Instruction::BIT(ArithmeticTarget::L, 5)), // BIT 5,L (CB 6D)
            0x6F => Some(Instruction::BIT(ArithmeticTarget::A, 5)), // BIT 5,A (CB 6F)
            
            0x70 => Some(Instruction::BIT(ArithmeticTarget::B, 6)), // BIT 6,B (CB 70)
            0x71 => Some(Instruction::BIT(ArithmeticTarget::C, 6)), // BIT 6,C (CB 71)
            0x72 => Some(Instruction::BIT(ArithmeticTarget::D, 6)), // BIT 6,D (CB 72)
            0x73 => Some(Instruction::BIT(ArithmeticTarget::E, 6)), // BIT 6,E (CB 73)
            0x74 => Some(Instruction::BIT(ArithmeticTarget::H, 6)), // BIT 6,H (CB 74)
            0x75 => Some(Instruction::BIT(ArithmeticTarget::L, 6)), // BIT 6,L (CB 75)
            0x77 => Some(Instruction::BIT(ArithmeticTarget::A, 6)), // BIT 6,A (CB 77)
            
            0x78 => Some(Instruction::BIT(ArithmeticTarget::B, 7)), // BIT 7,B (CB 78)
            0x79 => Some(Instruction::BIT(ArithmeticTarget::C, 7)), // BIT 7,C (CB 79)
            0x7A => Some(Instruction::BIT(ArithmeticTarget::D, 7)), // BIT 7,D (CB 7A)
            0x7B => Some(Instruction::BIT(ArithmeticTarget::E, 7)), // BIT 7,E (CB 7B)
            0x7C => Some(Instruction::BIT(ArithmeticTarget::H, 7)), // BIT 7,H (CB 7C)
            0x7D => Some(Instruction::BIT(ArithmeticTarget::L, 7)), // BIT 7,L (CB 7D)
            0x7F => Some(Instruction::BIT(ArithmeticTarget::A, 7)), // BIT 7,A (CB 7F)
            
            // Bit Reset Instructions (CB prefixed)
            0x80 => Some(Instruction::RESET(ArithmeticTarget::B, 0)), // RES 0,B (CB 80)
            0x81 => Some(Instruction::RESET(ArithmeticTarget::C, 0)), // RES 0,C (CB 81)
            0x82 => Some(Instruction::RESET(ArithmeticTarget::D, 0)), // RES 0,D (CB 82)
            0x83 => Some(Instruction::RESET(ArithmeticTarget::E, 0)), // RES 0,E (CB 83)
            0x84 => Some(Instruction::RESET(ArithmeticTarget::H, 0)), // RES 0,H (CB 84)
            0x85 => Some(Instruction::RESET(ArithmeticTarget::L, 0)), // RES 0,L (CB 85)
            0x87 => Some(Instruction::RESET(ArithmeticTarget::A, 0)), // RES 0,A (CB 87)
            
            0x88 => Some(Instruction::RESET(ArithmeticTarget::B, 1)), // RES 1,B (CB 88)
            0x89 => Some(Instruction::RESET(ArithmeticTarget::C, 1)), // RES 1,C (CB 89)
            0x8A => Some(Instruction::RESET(ArithmeticTarget::D, 1)), // RES 1,D (CB 8A)
            0x8B => Some(Instruction::RESET(ArithmeticTarget::E, 1)), // RES 1,E (CB 8B)
            0x8C => Some(Instruction::RESET(ArithmeticTarget::H, 1)), // RES 1,H (CB 8C)
            0x8D => Some(Instruction::RESET(ArithmeticTarget::L, 1)), // RES 1,L (CB 8D)
            0x8F => Some(Instruction::RESET(ArithmeticTarget::A, 1)), // RES 1,A (CB 8F)

            0x90 => Some(Instruction::RESET(ArithmeticTarget::B, 2)), // RES 2,B (CB 90)
            
            0x91 => Some(Instruction::RESET(ArithmeticTarget::C, 2)), // RES 2,C (CB 91)
            
            0x92 => Some(Instruction::RESET(ArithmeticTarget::D, 2)), // RES 2,D (CB 92)
            0x93 => Some(Instruction::RESET(ArithmeticTarget::E, 2)), // RES 2,E (CB 93)
            0x94 => Some(Instruction::RESET(ArithmeticTarget::H, 2)), // RES 2,H (CB 94)
            0x95 => Some(Instruction::RESET(ArithmeticTarget::L, 2)), // RES 2,L (CB 95)
            0x97 => Some(Instruction::RESET(ArithmeticTarget::A, 2)), // RES 2,A (CB 97)
            
            0x98 => Some(Instruction::RESET(ArithmeticTarget::B, 3)), // RES 3,B (CB 98)
            0x99 => Some(Instruction::RESET(ArithmeticTarget::C, 3)), // RES 3,C (CB 99)
            0x9A => Some(Instruction::RESET(ArithmeticTarget::D, 3)), // RES 3,D (CB 9A)
            
            0xA2 => Some(Instruction::RESET(ArithmeticTarget::D, 4)), // RES 4,D (CB A2)
            0xA3 => Some(Instruction::RESET(ArithmeticTarget::E, 4)), // RES 4,E (CB A3)
            0xA4 => Some(Instruction::RESET(ArithmeticTarget::H, 4)), // RES 4,H (CB A4)
            0xA5 => Some(Instruction::RESET(ArithmeticTarget::L, 4)), // RES 4,L (CB A5)
            0xA7 => Some(Instruction::RESET(ArithmeticTarget::A, 4)), // RES 4,A (CB A7)
            
            0xA8 => Some(Instruction::RESET(ArithmeticTarget::B, 5)), // RES 5,B (CB A8)
            0xA9 => Some(Instruction::RESET(ArithmeticTarget::C, 5)), // RES 5,C (CB A9)
            0xAA => Some(Instruction::RESET(ArithmeticTarget::D, 5)), // RES 5,D (CB AA)
            0xAB => Some(Instruction::RESET(ArithmeticTarget::E, 5)), // RES 5,E (CB AB)
            0xAC => Some(Instruction::RESET(ArithmeticTarget::H, 5)), // RES 5,H (CB AC)
            0xAD => Some(Instruction::RESET(ArithmeticTarget::L, 5)), // RES 5,L (CB AD)
            0xAF => Some(Instruction::RESET(ArithmeticTarget::A, 5)), // RES 5,A (CB AF)
            
            0xB0 => Some(Instruction::RESET(ArithmeticTarget::B, 6)), // RES 6,B (CB B0)
            0xB1 => Some(Instruction::RESET(ArithmeticTarget::C, 6)), // RES 6,C (CB B1)
            0xB2 => Some(Instruction::RESET(ArithmeticTarget::D, 6)), // RES 6,D (CB B2)
            0xB3 => Some(Instruction::RESET(ArithmeticTarget::E, 6)), // RES 6,E (CB B3)
            0xB4 => Some(Instruction::RESET(ArithmeticTarget::H, 6)), // RES 6,H (CB B4)
            0xB5 => Some(Instruction::RESET(ArithmeticTarget::L, 6)), // RES 6,L (CB B5)
            0xB7 => Some(Instruction::RESET(ArithmeticTarget::A, 6)), // RES 6,A (CB B7)
            
            0xB8 => Some(Instruction::RESET(ArithmeticTarget::B, 7)), // RES 7,B (CB B8)
            0xB9 => Some(Instruction::RESET(ArithmeticTarget::C, 7)), // RES 7,C (CB B9)
            0xBA => Some(Instruction::RESET(ArithmeticTarget::D, 7)), // RES 7,D (CB BA)
            0xBB => Some(Instruction::RESET(ArithmeticTarget::E, 7)), // RES 7,E (CB BB)
            0xBC => Some(Instruction::RESET(ArithmeticTarget::H, 7)), // RES 7,H (CB BC)
            0xBD => Some(Instruction::RESET(ArithmeticTarget::L, 7)), // RES 7,L (CB BD)
            0xBF => Some(Instruction::RESET(ArithmeticTarget::A, 7)), // RES 7,A (CB BF)
            
            // Bit Set Instructions (CB prefixed)
            0xC0 => Some(Instruction::SET(ArithmeticTarget::B, 0)), // SET 0,B (CB C0)
            0xC1 => Some(Instruction::SET(ArithmeticTarget::C, 0)), // SET 0,C (CB C1)
            0xC2 => Some(Instruction::SET(ArithmeticTarget::D, 0)), // SET 0,D (CB C2)
            0xC3 => Some(Instruction::SET(ArithmeticTarget::E, 0)), // SET 0,E (CB C3)
            0xC4 => Some(Instruction::SET(ArithmeticTarget::H, 0)), // SET 0,H (CB C4)
            0xC5 => Some(Instruction::SET(ArithmeticTarget::L, 0)), // SET 0,L (CB C5)
            0xC7 => Some(Instruction::SET(ArithmeticTarget::A, 0)), // SET 0,A (CB C7)
            
            0xC8 => Some(Instruction::SET(ArithmeticTarget::B, 1)), // SET 1,B (CB C8)
            0xC9 => Some(Instruction::SET(ArithmeticTarget::C, 1)), // SET 1,C (CB C9)
            0xCA => Some(Instruction::SET(ArithmeticTarget::D, 1)), // SET 1,D (CB CA)
            0xCB => Some(Instruction::SET(ArithmeticTarget::E, 1)), // SET 1,E (CB CB)
            0xCC => Some(Instruction::SET(ArithmeticTarget::H, 1)), // SET 1,H (CB CC)
            0xCD => Some(Instruction::SET(ArithmeticTarget::L, 1)), // SET 1,L (CB CD)
            0xCF => Some(Instruction::SET(ArithmeticTarget::A, 1)), // SET 1,A (CB CF)
            
            0xD0 => Some(Instruction::SET(ArithmeticTarget::B, 2)), // SET 2,B (CB D0)
            0xD1 => Some(Instruction::SET(ArithmeticTarget::C, 2)), // SET 2,C (CB D1)
            0xD2 => Some(Instruction::SET(ArithmeticTarget::D, 2)), // SET 2,D (CB D2)
            0xD3 => Some(Instruction::SET(ArithmeticTarget::E, 2)), // SET 2,E (CB D3)
            0xD4 => Some(Instruction::SET(ArithmeticTarget::H, 2)), // SET 2,H (CB D4)
            0xD5 => Some(Instruction::SET(ArithmeticTarget::L, 2)), // SET 2,L (CB D5)
            0xD7 => Some(Instruction::SET(ArithmeticTarget::A, 2)), // SET 2,A (CB D7)
            
            0xD8 => Some(Instruction::SET(ArithmeticTarget::B, 3)), // SET 3,B (CB D8)
            0xD9 => Some(Instruction::SET(ArithmeticTarget::C, 3)), // SET 3,C (CB D9)
            0xDA => Some(Instruction::SET(ArithmeticTarget::D, 3)), // SET 3,D (CB DA)
            0xDB => Some(Instruction::SET(ArithmeticTarget::E, 3)), // SET 3,E (CB DB)
            0xDC => Some(Instruction::SET(ArithmeticTarget::H, 3)), // SET 3,H (CB DC)
            0xDD => Some(Instruction::SET(ArithmeticTarget::L, 3)), // SET 3,L (CB DD)
            0xDF => Some(Instruction::SET(ArithmeticTarget::A, 3)), // SET 3,A (CB DF)
            
            0xE0 => Some(Instruction::SET(ArithmeticTarget::B, 4)), // SET 4,B (CB E0)
            0xE1 => Some(Instruction::SET(ArithmeticTarget::C, 4)), // SET 4,C (CB E1)
            0xE2 => Some(Instruction::SET(ArithmeticTarget::D, 4)), // SET 4,D (CB E2)
            0xE3 => Some(Instruction::SET(ArithmeticTarget::E, 4)), // SET 4,E (CB E3)
            0xE4 => Some(Instruction::SET(ArithmeticTarget::H, 4)), // SET 4,H (CB E4)
            0xE5 => Some(Instruction::SET(ArithmeticTarget::L, 4)), // SET 4,L (CB E5)
            0xE7 => Some(Instruction::SET(ArithmeticTarget::A, 4)), // SET 4,A (CB E7)
            
            0xE8 => Some(Instruction::SET(ArithmeticTarget::B, 5)), // SET 5,B (CB E8)
            0xE9 => Some(Instruction::SET(ArithmeticTarget::C, 5)), // SET 5,C (CB E9)
            0xEA => Some(Instruction::SET(ArithmeticTarget::D, 5)), // SET 5,D (CB EA)
            0xEB => Some(Instruction::SET(ArithmeticTarget::E, 5)), // SET 5,E (CB EB)
            0xEC => Some(Instruction::SET(ArithmeticTarget::H, 5)), // SET 5,H (CB EC)
            0xED => Some(Instruction::SET(ArithmeticTarget::L, 5)), // SET 5,L (CB ED)
            0xEF => Some(Instruction::SET(ArithmeticTarget::A, 5)), // SET 5,A (CB EF)
            
            0xF0 => Some(Instruction::SET(ArithmeticTarget::B, 6)), // SET 6,B (CB F0)
            0xF1 => Some(Instruction::SET(ArithmeticTarget::C, 6)), // SET 6,C (CB F1)
            0xF2 => Some(Instruction::SET(ArithmeticTarget::D, 6)), // SET 6,D (CB F2)
            0xF3 => Some(Instruction::SET(ArithmeticTarget::E, 6)), // SET 6,E (CB F3)
            0xF4 => Some(Instruction::SET(ArithmeticTarget::H, 6)), // SET 6,H (CB F4)
            0xF5 => Some(Instruction::SET(ArithmeticTarget::L, 6)), // SET 6,L (CB F5)
            0xF7 => Some(Instruction::SET(ArithmeticTarget::A, 6)), // SET 6,A (CB F7)
            
            0xF8 => Some(Instruction::SET(ArithmeticTarget::B, 7)), // SET 7,B (CB F8)
            0xF9 => Some(Instruction::SET(ArithmeticTarget::C, 7)), // SET 7,C (CB F9)
            0xFA => Some(Instruction::SET(ArithmeticTarget::D, 7)), // SET 7,D (CB FA)
            0xFB => Some(Instruction::SET(ArithmeticTarget::E, 7)), // SET 7,E (CB FB)
            0xFC => Some(Instruction::SET(ArithmeticTarget::H, 7)), // SET 7,H (CB FC)
            0xFD => Some(Instruction::SET(ArithmeticTarget::L, 7)), // SET 7,L (CB FD)
            0xFF => Some(Instruction::SET(ArithmeticTarget::A, 7)), // SET 7,A (CB FF)
            
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

            Instruction::ADDHL(target) => {
                match target {
                    VirtualTarget::AF => {
                        let value = self.registers.get_af();
                        let new_value = self.addhl(value);
                        self.registers.set_af(new_value);
                    }
                    VirtualTarget::BC => {
                        let value = self.registers.get_bc();
                        let new_value = self.addhl(value);
                        self.registers.set_bc(new_value);
                    }
                    VirtualTarget::DE => {
                        let value = self.registers.get_de();
                        let new_value = self.addhl(value);
                        self.registers.set_de(new_value);
                    }
                    VirtualTarget::HL => {
                        let value = self.registers.get_hl();
                        let new_value = self.addhl(value);
                        self.registers.set_hl(new_value);
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

    //addhl function
    //adds tatget register to HL
    fn addhl(&mut self, value: u16) -> u16 {
        let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        //check if the result of adding the lower 4 bits of the result of adding A and value is greater than 0xF
        self.registers.f.half_carry = (self.registers.get_hl() & 0x7FFF) + (value & 0x7FFF) > 0x7FFF;

        new_value
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
