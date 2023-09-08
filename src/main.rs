use std::{io::{ BufReader, BufRead }, fs::File, env };

/**

    register:
        16 slots, with 8bit numbers
    data tape:
        64 slots, with 8bit numbers
    code tape:
        64 slots with instructions
    format:
        16 characters, first 4 are instruction, the rest are arguments -- handled by the instruction i guess
    instructions
        0000 = hlt = halt
        0001 = srv register value -- sets value to register
        0010 = add register1 register2 register3
        0011 = sub register1 register2 register3
        0100 = mul register1 register2 register3
        0101 = div register1 register2 register3       
        0110 = oor register1 register2 register3      
        0111 = and register1 register2 register3      
        1000 = xor register1 register2 register3       register is 4 bits
        1001 = jmp position position is 6 bits
        1010 = jrz register position
        1011 = set position register position 6 bits, register 4 bits
        1100 = read position register
        1101 = out register
        1110 = cht register

         
        the trick is to >> 12 the instruction to get instruction id


*/

enum Type {
    Register,
    Position,
    Value,
}

fn load_instructions<R: BufRead>(reader: R) -> Vec<u16> {
    let mut instructions: Vec<u16> = vec![];
    let mut instruction: u16 = 0;
    let mut index = 4;
    let mut size = 0;

    for byte in reader.bytes() {
        if size == 64 {
            panic!("instruction tape overflow");
        }
        if index == 0 {
            index = 4;
            instructions.push(instruction);
            instruction = 0;
            size += 1;
        }
        index -= 1;
        instruction += (byte.unwrap() as u16) << (4 * index);
    }

    instructions.push(instruction);
    
    instructions
}

fn parse_instruction(instruction: &u16) -> (u16, u16) {
    let id = instruction >> 12; 
    (id, instruction & 4095)
}

fn parse_args(_args: u16, types: Vec<Type>) -> Vec<u16> {
    let mut result: Vec<u16> = vec![];
    let mut size = 12;
    let mut args = _args;
    for _type in types {
        if size == 0 {
            panic!("unable to process another type");
        }
        match _type {
            Type::Register => {
                size -= 4;
                if size >= 4 {
                    result.push(args >> size);
                } else {
                    result.push(args);
                }
                let base: u16 = 2;
                args &= base.pow(size) - 1;
            },
            Type::Value|Type::Position => {
                if size < 8 {
                    panic!("number to smol to be value or position");
                }
                
                size -= 8;
                result.push(args >> size);
            }
        }
    }
    result
}

fn machine(instructions: Vec<u16>) {
    let mut registers: Vec<u8> = vec![0; 16];
    let mut action: usize = 0;
    let mut storage: Vec<u8> = vec![0; 64];

    loop {
        let instruction: &u16 = &instructions[action];
        let (id, args) = parse_instruction(instruction);
        match id {
            0 => {
                break;
            }
            1 => { // srv
                let values = parse_args(args, vec![Type::Register, Type::Value]);
                registers[values[0] as usize] = values[1] as u8;
            },
            2 => { // add
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] + registers[values[1] as usize];
            },
            3 => { // sub 
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] - registers[values[1] as usize];
            },
            4 => { // mul 
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] * registers[values[1] as usize];
            },
            5 => { // div
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] / registers[values[1] as usize];
            },
            6 => { // oor 
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] | registers[values[1] as usize];
            },
            7 => { // and
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] & registers[values[1] as usize];
            },
            8 => { // xor
                let values = parse_args(args, vec![Type::Register, Type::Register, Type::Register]); 
                registers[values[2] as usize] = registers[values[0] as usize] ^ registers[values[1] as usize];
            },
            9 => { // jmp
                let values = parse_args(args, vec![Type::Position]);
                action = values[0] as usize;
                continue;
            },
            10 => { // jrz
                let values = parse_args(args, vec![Type::Register, Type::Position]);
                if registers[values[0] as usize] == 0 {
                    action = values[1] as usize;
                    continue;
                }
            },
            11 => { // set
                let values = parse_args(args, vec![Type::Register, Type::Register]);
                storage[registers[values[0] as usize] as usize] = registers[values[1] as usize];
            },
            12 => { // get
                let values = parse_args(args, vec![Type::Register, Type::Register]);
                registers[values[1] as usize] = storage[registers[values[0] as usize] as usize];
            },
            13 => { // out
                let values = parse_args(args, vec![Type::Register]);
                print!("{}", registers[values[0] as usize]);
            },
            14 => { // cht
                let values = parse_args(args, vec![Type::Register]);
                print!("{}", char::from_u32(registers[values[0] as usize] as u32).unwrap());
            },
            _ => {
                panic!("not implemented");
            }
        }
        action += 1;
    }

    //println!("{:?}", registers);
}

fn main() {
    let mut args = env::args();
    let filename = args.nth(1).unwrap();
    let file = File::open(&filename).expect("parek");
    let reader = BufReader::new(file);
    let instructions = load_instructions(reader);
    machine(instructions);
}
