//
//  CPU.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/1/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

struct Flags: OptionSet {
    let rawValue: UInt8
    
    static let fullCarry = Flags(rawValue: 1 << 4)
    static let halfCarry = Flags(rawValue: 1 << 5)
    static let subtract = Flags(rawValue: 1 << 6)
    static let zero = Flags(rawValue: 1 << 7)
}

extension UInt16 {
    mutating func incr(by: UInt16 = 1) {
        // convert to 32 bit, add 1, then mask to 16 bit
        self = UInt16((UInt32(self) + UInt32(by)) & 0xFFFF)
    }
}

struct Registers {
    var a: UInt8 = 0
    var b: UInt8 = 0
    var c: UInt8 = 0
    var d: UInt8 = 0
    var e: UInt8 = 0
    var h: UInt8 = 0
    var l: UInt8 = 0
    var flags: Flags = []
    var sp: UInt16 = 0 // stack pointer
    var pc: UInt16 = 0 // program counter
}

extension Registers {
    var af: UInt16 {
        get { return (UInt16(a) << 8) | UInt16(flags.rawValue) }
        set {
            a = UInt8(newValue >> 8)
            flags = Flags(rawValue: UInt8(newValue & 0xFF))
        }
    }
    
    var bc: UInt16 {
        get { return (UInt16(b) << 8) | UInt16(c) }
        set {
            b = UInt8(newValue >> 8)
            c = UInt8(newValue & 0xFF)
        }
    }
    
    var de: UInt16 {
        get { return (UInt16(d) << 8) | UInt16(e) }
        set {
            d = UInt8(newValue >> 8)
            e = UInt8(newValue & 0xFF)
        }
    }
    var hl: UInt16 {
        get { return (UInt16(h) << 8) | UInt16(l) }
        set {
            h = UInt8(newValue >> 8)
            l = UInt8(newValue & 0xFF)
        }
    }
}

struct Operation {
    let name: String
    let instruction: (cpu: inout CPU) -> ()
}

struct CPU {
    
    var registers = Registers()
    var mmu = MMU()
    var clock: UInt32 = 0
    
    private(set) var isHalted = false
    private(set) var isStopped = false
    private(set) var interruptsEnabled = true
    
    mutating func exec() {
        let opCode = mmu.readByte(address: registers.pc)
        registers.pc.incr()
        
        let op = CPU.operations[Int(opCode)]
        op.instruction(cpu: &self)
    }
    
    mutating func reset() {
        registers = Registers()
        clock = 0
    }
    
    // MARK: Static
    
    static let operations = [
        //0x0n
        Operation(name: "NOP") { $0.nop() },
        Operation(name: "LD BC,nn") { $0.loadPC(toReg: &$0.registers.bc) },
        Operation(name: "LD (BC),A") { $0.load(toAddr: $0.registers.bc, fromReg: $0.registers.a) },
        Operation(name: "INC BC") { $0.inc(reg: &$0.registers.bc) },
        Operation(name: "INC B") { $0.inc(reg: &$0.registers.b) },
        Operation(name: "DEC B") { $0.dec(reg: &$0.registers.b) },
        Operation(name: "LD B,n") { $0.loadPC(toReg: &$0.registers.b) },
        Operation(name: "RLC A") { $0.rlc(reg: &$0.registers.a, clockMod: 1) },
        Operation(name: "LD (nn),SP") { $0.loadPCAddr(fromReg: $0.registers.sp) },
        Operation(name: "ADD HL,BC") { $0.add(toReg: &$0.registers.hl, fromReg: $0.registers.bc) },
        Operation(name: "LD A,(BC)") { $0.load(toReg: &$0.registers.a, fromAddr: $0.registers.bc) },
        Operation(name: "DEC BC") { $0.dec(reg: &$0.registers.bc) },
        Operation(name: "INC C") { $0.inc(reg: &$0.registers.c) },
        Operation(name: "DEC C") { $0.dec(reg: &$0.registers.c) },
        Operation(name: "LD C,n") { $0.loadPC(toReg: &$0.registers.c) },
        Operation(name: "RRC A") { $0.rrc(reg: &$0.registers.a, clockMod: 1) },
        //0x1n
        Operation(name: "STOP") { $0.stop() },
        Operation(name: "LD DE,nn") { $0.loadPC(toReg: &$0.registers.de) },
        Operation(name: "LD (DE),A") { $0.load(toAddr: $0.registers.de, fromReg: $0.registers.a) },
        Operation(name: "INC DE") { $0.inc(reg: &$0.registers.de) },
        Operation(name: "INC D") { $0.inc(reg: &$0.registers.d) },
        Operation(name: "DEC D") { $0.dec(reg: &$0.registers.d) },
        Operation(name: "LD D,n") { $0.loadPC(toReg: &$0.registers.d) },
        Operation(name: "RL A") { $0.rl(reg: &$0.registers.a, clockMod: 1) },
        Operation(name: "JR e") { $0.jr() },
        Operation(name: "ADD HL,DE") { $0.add(toReg: &$0.registers.hl, fromReg: $0.registers.de) },
        Operation(name: "LD A,(DE)") { $0.load(toReg: &$0.registers.a, fromAddr: $0.registers.de) },
        Operation(name: "DEC DE") { $0.dec(reg: &$0.registers.de) },
        Operation(name: "INC E") { $0.inc(reg: &$0.registers.e) },
        Operation(name: "DEC E") { $0.dec(reg: &$0.registers.e) },
        Operation(name: "LD E,n") { $0.loadPC(toReg: &$0.registers.e) },
        Operation(name: "RR A") { $0.rr(reg: &$0.registers.a, clockMod: 1) },
        //0x2n
        Operation(name: "JR NZ,e") { $0.jr(condition: !$0.registers.flags.contains(.zero)) },
        Operation(name: "LD HL,nn") { $0.loadPC(toReg: &$0.registers.hl) },
        Operation(name: "LDI (HL),A") { $0.loadAndIncr(toRegAddr: &$0.registers.hl, fromReg: $0.registers.a) },
        Operation(name: "INC HL") { $0.inc(reg: &$0.registers.hl) },
        Operation(name: "INC H") { $0.inc(reg: &$0.registers.h) },
        Operation(name: "DEC H") { $0.dec(reg: &$0.registers.h) },
        Operation(name: "LD H,n") { $0.loadPC(toReg: &$0.registers.h) },
        Operation(name: "DAA") { $0.daa() },
        Operation(name: "JR Z,e") { $0.jr(condition: $0.registers.flags.contains(.zero)) },
        Operation(name: "ADD HL,HL") { $0.add(toReg: &$0.registers.hl, fromReg: $0.registers.hl) },
        Operation(name: "LDI A,(HL)") { $0.loadAndIncr(toReg: &$0.registers.a, fromRegAddr: &$0.registers.hl) },
        Operation(name: "DEC HL") { $0.dec(reg: &$0.registers.hl) },
        Operation(name: "INC L") { $0.inc(reg: &$0.registers.l) },
        Operation(name: "DEC L") { $0.dec(reg: &$0.registers.l) },
        Operation(name: "LD L,n") { $0.loadPC(toReg: &$0.registers.l) },
        Operation(name: "CPL") { $0.cpl() },
        //0x3n
        Operation(name: "JR NC,e") { $0.jr(condition: !$0.registers.flags.contains(.fullCarry)) },
        Operation(name: "LD SP,nn") { $0.loadPC(toReg: &$0.registers.sp) },
        Operation(name: "LDD (HL),A") { $0.loadAndDecr(toRegAddr: &$0.registers.hl, fromReg: $0.registers.a) },
        Operation(name: "INC SP") { $0.inc(reg: &$0.registers.sp) },
        Operation(name: "INC (HL)") { $0.inc(withAddr: $0.registers.hl) },
        Operation(name: "DEC (HL)") { $0.dec(withAddr: $0.registers.hl) },
        Operation(name: "LD (HL),n") { $0.loadPC(toAddr: $0.registers.hl) },
        Operation(name: "SCF") { $0.scf() },
        Operation(name: "JR C,e") { $0.jr(condition: $0.registers.flags.contains(.fullCarry)) },
        Operation(name: "ADD HL,SP") { $0.add(toReg: &$0.registers.hl, fromReg: $0.registers.sp) },
        Operation(name: "LDD A,(HL)") { $0.load(toReg: &$0.registers.a, fromAddr: $0.registers.hl) },
        Operation(name: "DEC SP") { $0.dec(reg: &$0.registers.sp) },
        Operation(name: "INC A") { $0.inc(reg: &$0.registers.a) },
        Operation(name: "DEC A") { $0.dec(reg: &$0.registers.a) },
        Operation(name: "LD A,n") { $0.loadPC(toReg: &$0.registers.a) },
        Operation(name: "CCF") { $0.ccf() },
        //0x4n
        Operation(name: "LD B,B") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.b) },
        Operation(name: "LD B,C") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.c) },
        Operation(name: "LD B,D") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.d) },
        Operation(name: "LD B,E") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.e) },
        Operation(name: "LD B,H") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.h) },
        Operation(name: "LD B,L") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.l) },
        Operation(name: "LD B,(HL)") { $0.load(toReg: &$0.registers.b, fromAddr: $0.registers.hl) },
        Operation(name: "LD B,A") { $0.load(toReg: &$0.registers.b, fromReg: $0.registers.a) },
        Operation(name: "LD C,B") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.b) },
        Operation(name: "LD C,C") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.c) },
        Operation(name: "LD C,D") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.d) },
        Operation(name: "LD C,E") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.e) },
        Operation(name: "LD C,H") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.h) },
        Operation(name: "LD C,L") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.l) },
        Operation(name: "LD C,(HL)") { $0.load(toReg: &$0.registers.c, fromAddr: $0.registers.hl) },
        Operation(name: "LD C,A") { $0.load(toReg: &$0.registers.c, fromReg: $0.registers.a) },
        //0x5n
        Operation(name: "LD D,B") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.b) },
        Operation(name: "LD D,C") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.c) },
        Operation(name: "LD D,D") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.d) },
        Operation(name: "LD D,E") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.e) },
        Operation(name: "LD D,H") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.h) },
        Operation(name: "LD D,L") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.l) },
        Operation(name: "LD D,(HL)") { $0.load(toReg: &$0.registers.d, fromAddr: $0.registers.hl) },
        Operation(name: "LD D,A") { $0.load(toReg: &$0.registers.d, fromReg: $0.registers.a) },
        Operation(name: "LD E,B") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.b) },
        Operation(name: "LD E,C") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.c) },
        Operation(name: "LD E,D") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.d) },
        Operation(name: "LD E,E") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.e) },
        Operation(name: "LD E,H") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.h) },
        Operation(name: "LD E,L") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.l) },
        Operation(name: "LD E,(HL)") { $0.load(toReg: &$0.registers.e, fromAddr: $0.registers.hl) },
        Operation(name: "LD E,A") { $0.load(toReg: &$0.registers.e, fromReg: $0.registers.a) },
        //0x6n
        Operation(name: "LD H,B") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.b) },
        Operation(name: "LD H,C") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.c) },
        Operation(name: "LD H,D") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.d) },
        Operation(name: "LD H,E") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.e) },
        Operation(name: "LD H,H") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.h) },
        Operation(name: "LD H,L") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.l) },
        Operation(name: "LD H,(HL)") { $0.load(toReg: &$0.registers.h, fromAddr: $0.registers.hl) },
        Operation(name: "LD H,A") { $0.load(toReg: &$0.registers.h, fromReg: $0.registers.a) },
        Operation(name: "LD L,B") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.b) },
        Operation(name: "LD L,C") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.c) },
        Operation(name: "LD L,D") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.d) },
        Operation(name: "LD L,E") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.e) },
        Operation(name: "LD L,H") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.h) },
        Operation(name: "LD L,L") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.l) },
        Operation(name: "LD L,(HL)") { $0.load(toReg: &$0.registers.l, fromAddr: $0.registers.hl) },
        Operation(name: "LD L,A") { $0.load(toReg: &$0.registers.l, fromReg: $0.registers.a) },
        //0x7n
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.b) },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.c) },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.d) },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.e) },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.h) },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.l) },
        Operation(name: "HALT") { $0.halt() },
        Operation(name: "LD (HL),B") { $0.load(toAddr: $0.registers.hl, fromReg: $0.registers.a) },
        Operation(name: "LD A,B") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.b) },
        Operation(name: "LD A,C") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.c) },
        Operation(name: "LD A,D") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.d) },
        Operation(name: "LD A,E") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.e) },
        Operation(name: "LD A,H") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.h) },
        Operation(name: "LD A,L") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.l) },
        Operation(name: "LD A,(HL)") { $0.load(toReg: &$0.registers.a, fromAddr: $0.registers.hl) },
        Operation(name: "LD A,A") { $0.load(toReg: &$0.registers.a, fromReg: $0.registers.a) },
        //0x8n
        Operation(name: "ADD A,B") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.b) },
        Operation(name: "ADD A,C") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.c) },
        Operation(name: "ADD A,D") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.d) },
        Operation(name: "ADD A,E") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.e) },
        Operation(name: "ADD A,H") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.h) },
        Operation(name: "ADD A,L") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.l) },
        Operation(name: "ADD A,(HL)") { $0.add(toReg: &$0.registers.a, fromAddr: $0.registers.hl) },
        Operation(name: "ADD A,A") { $0.add(toReg: &$0.registers.a, fromReg: $0.registers.a) },
        Operation(name: "ADC A,B") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.b) },
        Operation(name: "ADC A,C") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.c) },
        Operation(name: "ADC A,D") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.d) },
        Operation(name: "ADC A,E") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.e) },
        Operation(name: "ADC A,H") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.h) },
        Operation(name: "ADC A,L") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.l) },
        Operation(name: "ADC A,(HL)") { $0.addCarry(toReg: &$0.registers.a, fromAddr: $0.registers.hl) },
        Operation(name: "ADC A,A") { $0.addCarry(toReg: &$0.registers.a, fromReg: $0.registers.a) },
        //0x9n
        Operation(name: "SUB A,B") { $0.sub(fromReg: $0.registers.b) },
        Operation(name: "SUB A,C") { $0.sub(fromReg: $0.registers.c) },
        Operation(name: "SUB A,D") { $0.sub(fromReg: $0.registers.d) },
        Operation(name: "SUB A,E") { $0.sub(fromReg: $0.registers.e) },
        Operation(name: "SUB A,H") { $0.sub(fromReg: $0.registers.h) },
        Operation(name: "SUB A,L") { $0.sub(fromReg: $0.registers.l) },
        Operation(name: "SUB A,(HL)") { $0.sub(fromAddr: $0.registers.hl) },
        Operation(name: "SUB A,A") { $0.sub(fromReg: $0.registers.a) },
        Operation(name: "SBC A,B") { $0.subCarry(fromReg: $0.registers.b) },
        Operation(name: "SBC A,C") { $0.subCarry(fromReg: $0.registers.c) },
        Operation(name: "SBC A,D") { $0.subCarry(fromReg: $0.registers.d) },
        Operation(name: "SBC A,E") { $0.subCarry(fromReg: $0.registers.e) },
        Operation(name: "SBC A,H") { $0.subCarry(fromReg: $0.registers.h) },
        Operation(name: "SBC A,L") { $0.subCarry(fromReg: $0.registers.l) },
        Operation(name: "SBC A,(HL)") { $0.subCarry(fromAddr: $0.registers.hl) },
        Operation(name: "SBC A,A") { $0.subCarry(fromReg: $0.registers.a) },
        //0xAn
        Operation(name: "AND B") { $0.and(fromReg: $0.registers.b) },
        Operation(name: "AND C") { $0.and(fromReg: $0.registers.c) },
        Operation(name: "AND D") { $0.and(fromReg: $0.registers.d) },
        Operation(name: "AND E") { $0.and(fromReg: $0.registers.e) },
        Operation(name: "AND H") { $0.and(fromReg: $0.registers.h) },
        Operation(name: "AND L") { $0.and(fromReg: $0.registers.l) },
        Operation(name: "AND (HL)") { $0.and(fromAddr: $0.registers.hl) },
        Operation(name: "AND A") { $0.and(fromReg: $0.registers.a) },
        Operation(name: "XOR B") { $0.xor(fromReg: $0.registers.b) },
        Operation(name: "XOR C") { $0.xor(fromReg: $0.registers.c) },
        Operation(name: "XOR D") { $0.xor(fromReg: $0.registers.d) },
        Operation(name: "XOR E") { $0.xor(fromReg: $0.registers.e) },
        Operation(name: "XOR H") { $0.xor(fromReg: $0.registers.h) },
        Operation(name: "XOR L") { $0.xor(fromReg: $0.registers.l) },
        Operation(name: "XOR (HL)") { $0.xor(fromAddr: $0.registers.hl) },
        Operation(name: "XOR A") { $0.xor(fromReg: $0.registers.a) },
        //0xBn
        Operation(name: "OR B") { $0.or(fromReg: $0.registers.b) },
        Operation(name: "OR C") { $0.or(fromReg: $0.registers.c) },
        Operation(name: "OR D") { $0.or(fromReg: $0.registers.d) },
        Operation(name: "OR E") { $0.or(fromReg: $0.registers.e) },
        Operation(name: "OR H") { $0.or(fromReg: $0.registers.h) },
        Operation(name: "OR L") { $0.or(fromReg: $0.registers.l) },
        Operation(name: "OR (HL)") { $0.or(fromAddr: $0.registers.hl) },
        Operation(name: "OR A") { $0.or(fromReg: $0.registers.a) },
        Operation(name: "CP B") { $0.cp(withReg: $0.registers.b) },
        Operation(name: "CP C") { $0.cp(withReg: $0.registers.c) },
        Operation(name: "CP D") { $0.cp(withReg: $0.registers.d) },
        Operation(name: "CP E") { $0.cp(withReg: $0.registers.e) },
        Operation(name: "CP H") { $0.cp(withReg: $0.registers.h) },
        Operation(name: "CP L") { $0.cp(withReg: $0.registers.l) },
        Operation(name: "CP (HL)") { $0.cp(withAddr: $0.registers.hl) },
        Operation(name: "CP A") { $0.cp(withReg: $0.registers.a) },
        //0xCn
        Operation(name: "RET NZ") { $0.ret(condition: !$0.registers.flags.contains(.zero)) },
        Operation(name: "POP BC") { $0.pop(toReg: &$0.registers.bc) },
        Operation(name: "JP NZ,nn") { $0.jp(condition: !$0.registers.flags.contains(.zero)) },
        Operation(name: "JP nn") { $0.jp() },
        Operation(name: "CALL NZ,nn") { $0.call(condition: !$0.registers.flags.contains(.zero)) },
        Operation(name: "PUSH BC") { $0.push(fromReg: $0.registers.bc) },
        Operation(name: "ADD A,n") { $0.addPC(toReg: &$0.registers.a) },
        Operation(name: "RST 0") { $0.rst(n: 0x00) },
        Operation(name: "RET Z") { $0.ret(condition: $0.registers.flags.contains(.zero)) },
        Operation(name: "RET") { $0.ret() },
        Operation(name: "JP Z,nn") { $0.jp(condition: $0.registers.flags.contains(.zero)) },
        Operation(name: "EXP OPS") { $0.extOps() },
        Operation(name: "CALL Z,nn") { $0.call(condition: $0.registers.flags.contains(.zero)) },
        Operation(name: "CALL nn") { $0.call() },
        Operation(name: "ADC A,n") { $0.addCarryPC(toReg: &$0.registers.a) },
        Operation(name: "RST 8") { $0.rst(n: 0x08) },
        //0xDn
        Operation(name: "RET NC") { $0.ret(condition: !$0.registers.flags.contains(.fullCarry)) },
        Operation(name: "POP DE") { $0.pop(toReg: &$0.registers.de) },
        Operation(name: "JP NC,nn") { $0.jp(condition: !$0.registers.flags.contains(.fullCarry)) },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "CALL NC,nn") { $0.call(condition: !$0.registers.flags.contains(.fullCarry)) },
        Operation(name: "PUSH DE") { $0.push(fromReg: $0.registers.de) },
        Operation(name: "SUB A,n") { $0.subPC() },
        Operation(name: "RST 10") { $0.rst(n: 0x10) },
        Operation(name: "RET C") { $0.ret(condition: $0.registers.flags.contains(.fullCarry)) },
        Operation(name: "RETI") { $0.reti() },
        Operation(name: "JP C,nn") { $0.jp(condition: $0.registers.flags.contains(.fullCarry)) },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "CALL c,nn") { $0.call(condition: $0.registers.flags.contains(.fullCarry)) },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "SBC A,n") { $0.subCarryPC() },
        Operation(name: "RST 18") { $0.rst(n: 0x18) },
        //0xEn
        Operation(name: "LDH (n),A") { $0.loadPCAddrNibble(fromReg: &$0.registers.a) },
        Operation(name: "POP HL") { $0.pop(toReg: &$0.registers.hl) },
        Operation(name: "LDH (C),A") { $0.loadToCAddr(fromReg: $0.registers.a) },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "PUSH HL") { $0.push(fromReg: $0.registers.hl) },
        Operation(name: "AND n") { $0.andPC() },
        Operation(name: "RST 20") { $0.rst(n: 0x20) },
        Operation(name: "ADD SP,e") { $0.addPC(toReg: &$0.registers.sp) },
        Operation(name: "JP (HL)") { $0.jpHL() },
        Operation(name: "LD (nn),A") { $0.loadPCAddr(fromReg: $0.registers.a) },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "XOR n") { $0.xorPC() },
        Operation(name: "RST 28") { $0.rst(n: 0x28) },
        //0xFn
        Operation(name: "LDH A,(n)") { $0.loadPCAddrNibble(toReg: &$0.registers.a) },
        Operation(name: "POP AF") { $0.pop(toReg: &$0.registers.af) },
        Operation(name: "LDH A,(C)") { $0.loadCAddr(toReg: &$0.registers.a) },
        Operation(name: "DI") { $0.di() },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "PUSH AF") { $0.push(fromReg: $0.registers.af) },
        Operation(name: "OR n") { $0.orPC() },
        Operation(name: "RST 30") { $0.rst(n: 0x30) },
        Operation(name: "LDHL SP,e") { $0.loadSPN(toReg: &$0.registers.hl) },
        Operation(name: "LD SP,HL") { $0.loadToSP(fromReg: $0.registers.hl) },
        Operation(name: "LD A,(NN)") { $0.loadPCAddr(toReg: &$0.registers.a) },
        Operation(name: "EI") { $0.ei() },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "XX") { _ in /*nop*/ },
        Operation(name: "CP n") { $0.cpPC() },
        Operation(name: "RST 38") { $0.rst(n: 0x38) }
    ]
    
    static let extOperations = [
        //0x0n
        Operation(name: "RLC B") { $0.rlc(reg: &$0.registers.b, clockMod: 2) },
        Operation(name: "RLC C") { $0.rlc(reg: &$0.registers.c, clockMod: 2) },
        Operation(name: "RLC D") { $0.rlc(reg: &$0.registers.d, clockMod: 2) },
        Operation(name: "RLC E") { $0.rlc(reg: &$0.registers.e, clockMod: 2) },
        Operation(name: "RLC H") { $0.rlc(reg: &$0.registers.h, clockMod: 2) },
        Operation(name: "RLC L") { $0.rlc(reg: &$0.registers.l, clockMod: 2) },
        Operation(name: "RLC (HL)") { $0.rlc(addr: $0.registers.hl) },
        Operation(name: "RLC A") { $0.rlc(reg: &$0.registers.a, clockMod: 2) },
        Operation(name: "RRC B") { $0.rrc(reg: &$0.registers.b, clockMod: 2) },
        Operation(name: "RRC C") { $0.rrc(reg: &$0.registers.c, clockMod: 2) },
        Operation(name: "RRC D") { $0.rrc(reg: &$0.registers.d, clockMod: 2) },
        Operation(name: "RRC E") { $0.rrc(reg: &$0.registers.e, clockMod: 2) },
        Operation(name: "RRC H") { $0.rrc(reg: &$0.registers.h, clockMod: 2) },
        Operation(name: "RRC L") { $0.rrc(reg: &$0.registers.l, clockMod: 2) },
        Operation(name: "RRC (HL)") { $0.rrc(addr: $0.registers.hl) },
        Operation(name: "RRC A") { $0.rrc(reg: &$0.registers.a, clockMod: 2) },
        //0x1n
        Operation(name: "RL B") { $0.rl(reg: &$0.registers.b, clockMod: 2) },
        Operation(name: "RL C") { $0.rl(reg: &$0.registers.c, clockMod: 2) },
        Operation(name: "RL D") { $0.rl(reg: &$0.registers.d, clockMod: 2) },
        Operation(name: "RL E") { $0.rl(reg: &$0.registers.e, clockMod: 2) },
        Operation(name: "RL H") { $0.rl(reg: &$0.registers.h, clockMod: 2) },
        Operation(name: "RL L") { $0.rl(reg: &$0.registers.l, clockMod: 2) },
        Operation(name: "RL (HL)") { $0.rl(addr: $0.registers.hl) },
        Operation(name: "RL A") { $0.rl(reg: &$0.registers.a, clockMod: 2) },
        Operation(name: "RR B") { $0.rr(reg: &$0.registers.b, clockMod: 2) },
        Operation(name: "RR C") { $0.rr(reg: &$0.registers.c, clockMod: 2) },
        Operation(name: "RR D") { $0.rr(reg: &$0.registers.d, clockMod: 2) },
        Operation(name: "RR E") { $0.rr(reg: &$0.registers.e, clockMod: 2) },
        Operation(name: "RR H") { $0.rr(reg: &$0.registers.h, clockMod: 2) },
        Operation(name: "RR L") { $0.rr(reg: &$0.registers.l, clockMod: 2) },
        Operation(name: "RR (HL)") { $0.rr(addr: $0.registers.hl) },
        Operation(name: "RR A") { $0.rr(reg: &$0.registers.a, clockMod: 2) },
        //0x2n
        Operation(name: "SLA B") { $0.sla(reg: &$0.registers.b) },
        Operation(name: "SLA C") { $0.sla(reg: &$0.registers.c) },
        Operation(name: "SLA D") { $0.sla(reg: &$0.registers.d) },
        Operation(name: "SLA E") { $0.sla(reg: &$0.registers.e) },
        Operation(name: "SLA H") { $0.sla(reg: &$0.registers.h) },
        Operation(name: "SLA L") { $0.sla(reg: &$0.registers.l) },
        Operation(name: "SLA (HL)") { $0.sla(addr: $0.registers.hl) },
        Operation(name: "SLA A") { $0.sla(reg: &$0.registers.a) },
        Operation(name: "SRA B") { $0.sra(reg: &$0.registers.b) },
        Operation(name: "SRA C") { $0.sra(reg: &$0.registers.c) },
        Operation(name: "SRA D") { $0.sra(reg: &$0.registers.d) },
        Operation(name: "SRA E") { $0.sra(reg: &$0.registers.e) },
        Operation(name: "SRA H") { $0.sra(reg: &$0.registers.h) },
        Operation(name: "SRA L") { $0.sra(reg: &$0.registers.l) },
        Operation(name: "SRA (HL)") { $0.sra(addr: $0.registers.hl) },
        Operation(name: "SRA A") { $0.sra(reg: &$0.registers.a) },
        //0x3n
        Operation(name: "SWAP B") { $0.swap(reg: &$0.registers.b) },
        Operation(name: "SWAP C") { $0.swap(reg: &$0.registers.c) },
        Operation(name: "SWAP D") { $0.swap(reg: &$0.registers.d) },
        Operation(name: "SWAP E") { $0.swap(reg: &$0.registers.e) },
        Operation(name: "SWAP H") { $0.swap(reg: &$0.registers.h) },
        Operation(name: "SWAP L") { $0.swap(reg: &$0.registers.l) },
        Operation(name: "SWAP (HL)") { $0.swap(addr: $0.registers.hl) },
        Operation(name: "SWAP A") { $0.swap(reg: &$0.registers.a) },
        Operation(name: "SRL B") { $0.srl(reg: &$0.registers.b) },
        Operation(name: "SRL C") { $0.srl(reg: &$0.registers.c) },
        Operation(name: "SRL D") { $0.srl(reg: &$0.registers.d) },
        Operation(name: "SRL E") { $0.srl(reg: &$0.registers.e) },
        Operation(name: "SRL H") { $0.srl(reg: &$0.registers.h) },
        Operation(name: "SRL L") { $0.srl(reg: &$0.registers.l) },
        Operation(name: "SRL (HL)") { $0.srl(addr: $0.registers.hl) },
        Operation(name: "SRL A") { $0.srl(reg: &$0.registers.a) },
        //0x4n
        Operation(name: "BIT 0,B") { $0.bit(bitToCheck: 0, reg: $0.registers.b) },
        Operation(name: "BIT 0,C") { $0.bit(bitToCheck: 0, reg: $0.registers.c) },
        Operation(name: "BIT 0,D") { $0.bit(bitToCheck: 0, reg: $0.registers.d) },
        Operation(name: "BIT 0,E") { $0.bit(bitToCheck: 0, reg: $0.registers.e) },
        Operation(name: "BIT 0,H") { $0.bit(bitToCheck: 0, reg: $0.registers.h) },
        Operation(name: "BIT 0,L") { $0.bit(bitToCheck: 0, reg: $0.registers.l) },
        Operation(name: "BIT 0,(HL)") { $0.bit(bitToCheck: 0, addr: $0.registers.hl) },
        Operation(name: "BIT 0,A") { $0.bit(bitToCheck: 0, reg: $0.registers.a) },
        Operation(name: "BIT 1,B") { $0.bit(bitToCheck: 1, reg: $0.registers.b) },
        Operation(name: "BIT 1,C") { $0.bit(bitToCheck: 1, reg: $0.registers.c) },
        Operation(name: "BIT 1,D") { $0.bit(bitToCheck: 1, reg: $0.registers.d) },
        Operation(name: "BIT 1,E") { $0.bit(bitToCheck: 1, reg: $0.registers.e) },
        Operation(name: "BIT 1,H") { $0.bit(bitToCheck: 1, reg: $0.registers.h) },
        Operation(name: "BIT 1,L") { $0.bit(bitToCheck: 1, reg: $0.registers.l) },
        Operation(name: "BIT 1,(HL)") { $0.bit(bitToCheck: 1, addr: $0.registers.hl) },
        Operation(name: "BIT 1,A") { $0.bit(bitToCheck: 1, reg: $0.registers.a) },
        //0x5n
        Operation(name: "BIT 2,B") { $0.bit(bitToCheck: 2, reg: $0.registers.b) },
        Operation(name: "BIT 2,C") { $0.bit(bitToCheck: 2, reg: $0.registers.c) },
        Operation(name: "BIT 2,D") { $0.bit(bitToCheck: 2, reg: $0.registers.d) },
        Operation(name: "BIT 2,E") { $0.bit(bitToCheck: 2, reg: $0.registers.e) },
        Operation(name: "BIT 2,H") { $0.bit(bitToCheck: 2, reg: $0.registers.h) },
        Operation(name: "BIT 2,L") { $0.bit(bitToCheck: 2, reg: $0.registers.l) },
        Operation(name: "BIT 2,(HL)") { $0.bit(bitToCheck: 2, addr: $0.registers.hl) },
        Operation(name: "BIT 2,A") { $0.bit(bitToCheck: 2, reg: $0.registers.a) },
        Operation(name: "BIT 3,B") { $0.bit(bitToCheck: 3, reg: $0.registers.b) },
        Operation(name: "BIT 3,C") { $0.bit(bitToCheck: 3, reg: $0.registers.c) },
        Operation(name: "BIT 3,D") { $0.bit(bitToCheck: 3, reg: $0.registers.d) },
        Operation(name: "BIT 3,E") { $0.bit(bitToCheck: 3, reg: $0.registers.e) },
        Operation(name: "BIT 3,H") { $0.bit(bitToCheck: 3, reg: $0.registers.h) },
        Operation(name: "BIT 3,L") { $0.bit(bitToCheck: 3, reg: $0.registers.l) },
        Operation(name: "BIT 3,(HL)") { $0.bit(bitToCheck: 3, addr: $0.registers.hl) },
        Operation(name: "BIT 3,A") { $0.bit(bitToCheck: 3, reg: $0.registers.a) },
        //0x6n
        Operation(name: "BIT 4,B") { $0.bit(bitToCheck: 4, reg: $0.registers.b) },
        Operation(name: "BIT 4,C") { $0.bit(bitToCheck: 4, reg: $0.registers.c) },
        Operation(name: "BIT 4,D") { $0.bit(bitToCheck: 4, reg: $0.registers.d) },
        Operation(name: "BIT 4,E") { $0.bit(bitToCheck: 4, reg: $0.registers.e) },
        Operation(name: "BIT 4,H") { $0.bit(bitToCheck: 4, reg: $0.registers.h) },
        Operation(name: "BIT 4,L") { $0.bit(bitToCheck: 4, reg: $0.registers.l) },
        Operation(name: "BIT 4,(HL)") { $0.bit(bitToCheck: 4, addr: $0.registers.hl) },
        Operation(name: "BIT 4,A") { $0.bit(bitToCheck: 4, reg: $0.registers.a) },
        Operation(name: "BIT 5,B") { $0.bit(bitToCheck: 5, reg: $0.registers.b) },
        Operation(name: "BIT 5,C") { $0.bit(bitToCheck: 5, reg: $0.registers.c) },
        Operation(name: "BIT 5,D") { $0.bit(bitToCheck: 5, reg: $0.registers.d) },
        Operation(name: "BIT 5,E") { $0.bit(bitToCheck: 5, reg: $0.registers.e) },
        Operation(name: "BIT 5,H") { $0.bit(bitToCheck: 5, reg: $0.registers.h) },
        Operation(name: "BIT 5,L") { $0.bit(bitToCheck: 5, reg: $0.registers.l) },
        Operation(name: "BIT 5,(HL)") { $0.bit(bitToCheck: 5, addr: $0.registers.hl) },
        Operation(name: "BIT 5,A") { $0.bit(bitToCheck: 5, reg: $0.registers.a) },
        //0x7n
        Operation(name: "BIT 6,B") { $0.bit(bitToCheck: 6, reg: $0.registers.b) },
        Operation(name: "BIT 6,C") { $0.bit(bitToCheck: 6, reg: $0.registers.c) },
        Operation(name: "BIT 6,D") { $0.bit(bitToCheck: 6, reg: $0.registers.d) },
        Operation(name: "BIT 6,E") { $0.bit(bitToCheck: 6, reg: $0.registers.e) },
        Operation(name: "BIT 6,H") { $0.bit(bitToCheck: 6, reg: $0.registers.h) },
        Operation(name: "BIT 6,L") { $0.bit(bitToCheck: 6, reg: $0.registers.l) },
        Operation(name: "BIT 6,(HL)") { $0.bit(bitToCheck: 6, addr: $0.registers.hl) },
        Operation(name: "BIT 6,A") { $0.bit(bitToCheck: 6, reg: $0.registers.a) },
        Operation(name: "BIT 7,B") { $0.bit(bitToCheck: 7, reg: $0.registers.b) },
        Operation(name: "BIT 7,C") { $0.bit(bitToCheck: 7, reg: $0.registers.c) },
        Operation(name: "BIT 7,D") { $0.bit(bitToCheck: 7, reg: $0.registers.d) },
        Operation(name: "BIT 7,E") { $0.bit(bitToCheck: 7, reg: $0.registers.e) },
        Operation(name: "BIT 7,H") { $0.bit(bitToCheck: 7, reg: $0.registers.h) },
        Operation(name: "BIT 7,L") { $0.bit(bitToCheck: 7, reg: $0.registers.l) },
        Operation(name: "BIT 7,(HL)") { $0.bit(bitToCheck: 7, addr: $0.registers.hl) },
        Operation(name: "BIT 7,A") { $0.bit(bitToCheck: 7, reg: $0.registers.a) },
        //0x8n
        Operation(name: "RES 0,B") { $0.res(bitToReset: 0, reg: &$0.registers.b) },
        Operation(name: "RES 0,C") { $0.res(bitToReset: 0, reg: &$0.registers.c) },
        Operation(name: "RES 0,D") { $0.res(bitToReset: 0, reg: &$0.registers.d) },
        Operation(name: "RES 0,E") { $0.res(bitToReset: 0, reg: &$0.registers.e) },
        Operation(name: "RES 0,H") { $0.res(bitToReset: 0, reg: &$0.registers.h) },
        Operation(name: "RES 0,L") { $0.res(bitToReset: 0, reg: &$0.registers.l) },
        Operation(name: "RES 0,(HL)") { $0.res(bitToReset: 0, addr: $0.registers.hl) },
        Operation(name: "RES 0,A") { $0.res(bitToReset: 0, reg: &$0.registers.a) },
        Operation(name: "RES 1,B") { $0.res(bitToReset: 1, reg: &$0.registers.b) },
        Operation(name: "RES 1,C") { $0.res(bitToReset: 1, reg: &$0.registers.c) },
        Operation(name: "RES 1,D") { $0.res(bitToReset: 1, reg: &$0.registers.d) },
        Operation(name: "RES 1,E") { $0.res(bitToReset: 1, reg: &$0.registers.e) },
        Operation(name: "RES 1,H") { $0.res(bitToReset: 1, reg: &$0.registers.h) },
        Operation(name: "RES 1,L") { $0.res(bitToReset: 1, reg: &$0.registers.l) },
        Operation(name: "RES 1,(HL)") { $0.res(bitToReset: 1, addr: $0.registers.hl) },
        Operation(name: "RES 1,A") { $0.res(bitToReset: 1, reg: &$0.registers.a) },
        //0x9n
        Operation(name: "RES 2,B") { $0.res(bitToReset: 2, reg: &$0.registers.b) },
        Operation(name: "RES 2,C") { $0.res(bitToReset: 2, reg: &$0.registers.c) },
        Operation(name: "RES 2,D") { $0.res(bitToReset: 2, reg: &$0.registers.d) },
        Operation(name: "RES 2,E") { $0.res(bitToReset: 2, reg: &$0.registers.e) },
        Operation(name: "RES 2,H") { $0.res(bitToReset: 2, reg: &$0.registers.h) },
        Operation(name: "RES 2,L") { $0.res(bitToReset: 2, reg: &$0.registers.l) },
        Operation(name: "RES 2,(HL)") { $0.res(bitToReset: 2, addr: $0.registers.hl) },
        Operation(name: "RES 2,A") { $0.res(bitToReset: 2, reg: &$0.registers.a) },
        Operation(name: "RES 3,B") { $0.res(bitToReset: 3, reg: &$0.registers.b) },
        Operation(name: "RES 3,C") { $0.res(bitToReset: 3, reg: &$0.registers.c) },
        Operation(name: "RES 3,D") { $0.res(bitToReset: 3, reg: &$0.registers.d) },
        Operation(name: "RES 3,E") { $0.res(bitToReset: 3, reg: &$0.registers.e) },
        Operation(name: "RES 3,H") { $0.res(bitToReset: 3, reg: &$0.registers.h) },
        Operation(name: "RES 3,L") { $0.res(bitToReset: 3, reg: &$0.registers.l) },
        Operation(name: "RES 3,(HL)") { $0.res(bitToReset: 3, addr: $0.registers.hl) },
        Operation(name: "RES 3,A") { $0.res(bitToReset: 3, reg: &$0.registers.a) },
        //0xAn
        Operation(name: "RES 4,B") { $0.res(bitToReset: 4, reg: &$0.registers.b) },
        Operation(name: "RES 4,C") { $0.res(bitToReset: 4, reg: &$0.registers.c) },
        Operation(name: "RES 4,D") { $0.res(bitToReset: 4, reg: &$0.registers.d) },
        Operation(name: "RES 4,E") { $0.res(bitToReset: 4, reg: &$0.registers.e) },
        Operation(name: "RES 4,H") { $0.res(bitToReset: 4, reg: &$0.registers.h) },
        Operation(name: "RES 4,L") { $0.res(bitToReset: 4, reg: &$0.registers.l) },
        Operation(name: "RES 4,(HL)") { $0.res(bitToReset: 4, addr: $0.registers.hl) },
        Operation(name: "RES 4,A") { $0.res(bitToReset: 4, reg: &$0.registers.a) },
        Operation(name: "RES 5,B") { $0.res(bitToReset: 5, reg: &$0.registers.b) },
        Operation(name: "RES 5,C") { $0.res(bitToReset: 5, reg: &$0.registers.c) },
        Operation(name: "RES 5,D") { $0.res(bitToReset: 5, reg: &$0.registers.d) },
        Operation(name: "RES 5,E") { $0.res(bitToReset: 5, reg: &$0.registers.e) },
        Operation(name: "RES 5,H") { $0.res(bitToReset: 5, reg: &$0.registers.h) },
        Operation(name: "RES 5,L") { $0.res(bitToReset: 5, reg: &$0.registers.l) },
        Operation(name: "RES 5,(HL)") { $0.res(bitToReset: 5, addr: $0.registers.hl) },
        Operation(name: "RES 5,A") { $0.res(bitToReset: 5, reg: &$0.registers.a) },
        //0xBn
        Operation(name: "RES 6,B") { $0.res(bitToReset: 6, reg: &$0.registers.b) },
        Operation(name: "RES 6,C") { $0.res(bitToReset: 6, reg: &$0.registers.c) },
        Operation(name: "RES 6,D") { $0.res(bitToReset: 6, reg: &$0.registers.d) },
        Operation(name: "RES 6,E") { $0.res(bitToReset: 6, reg: &$0.registers.e) },
        Operation(name: "RES 6,H") { $0.res(bitToReset: 6, reg: &$0.registers.h) },
        Operation(name: "RES 6,L") { $0.res(bitToReset: 6, reg: &$0.registers.l) },
        Operation(name: "RES 6,(HL)") { $0.res(bitToReset: 6, addr: $0.registers.hl) },
        Operation(name: "RES 6,A") { $0.res(bitToReset: 6, reg: &$0.registers.a) },
        Operation(name: "RES 7,B") { $0.res(bitToReset: 7, reg: &$0.registers.b) },
        Operation(name: "RES 7,C") { $0.res(bitToReset: 7, reg: &$0.registers.c) },
        Operation(name: "RES 7,D") { $0.res(bitToReset: 7, reg: &$0.registers.d) },
        Operation(name: "RES 7,E") { $0.res(bitToReset: 7, reg: &$0.registers.e) },
        Operation(name: "RES 7,H") { $0.res(bitToReset: 7, reg: &$0.registers.h) },
        Operation(name: "RES 7,L") { $0.res(bitToReset: 7, reg: &$0.registers.l) },
        Operation(name: "RES 7,(HL)") { $0.res(bitToReset: 7, addr: $0.registers.hl) },
        Operation(name: "RES 7,A") { $0.res(bitToReset: 7, reg: &$0.registers.a) },
        //0xCn
        Operation(name: "SET 0,B") { $0.set(bitToSet: 0, reg: &$0.registers.b) },
        Operation(name: "SET 0,C") { $0.set(bitToSet: 0, reg: &$0.registers.c) },
        Operation(name: "SET 0,D") { $0.set(bitToSet: 0, reg: &$0.registers.d) },
        Operation(name: "SET 0,E") { $0.set(bitToSet: 0, reg: &$0.registers.e) },
        Operation(name: "SET 0,H") { $0.set(bitToSet: 0, reg: &$0.registers.h) },
        Operation(name: "SET 0,L") { $0.set(bitToSet: 0, reg: &$0.registers.l) },
        Operation(name: "SET 0,(HL)") { $0.set(bitToSet: 0, addr: $0.registers.hl) },
        Operation(name: "SET 0,A") { $0.set(bitToSet: 0, reg: &$0.registers.a) },
        Operation(name: "SET 1,B") { $0.set(bitToSet: 1, reg: &$0.registers.b) },
        Operation(name: "SET 1,C") { $0.set(bitToSet: 1, reg: &$0.registers.c) },
        Operation(name: "SET 1,D") { $0.set(bitToSet: 1, reg: &$0.registers.d) },
        Operation(name: "SET 1,E") { $0.set(bitToSet: 1, reg: &$0.registers.e) },
        Operation(name: "SET 1,H") { $0.set(bitToSet: 1, reg: &$0.registers.h) },
        Operation(name: "SET 1,L") { $0.set(bitToSet: 1, reg: &$0.registers.l) },
        Operation(name: "SET 1,(HL)") { $0.set(bitToSet: 1, addr: $0.registers.hl) },
        Operation(name: "SET 1,A") { $0.set(bitToSet: 1, reg: &$0.registers.a) },
        //0xDn
        Operation(name: "SET 2,B") { $0.set(bitToSet: 2, reg: &$0.registers.b) },
        Operation(name: "SET 2,C") { $0.set(bitToSet: 2, reg: &$0.registers.c) },
        Operation(name: "SET 2,D") { $0.set(bitToSet: 2, reg: &$0.registers.d) },
        Operation(name: "SET 2,E") { $0.set(bitToSet: 2, reg: &$0.registers.e) },
        Operation(name: "SET 2,H") { $0.set(bitToSet: 2, reg: &$0.registers.h) },
        Operation(name: "SET 2,L") { $0.set(bitToSet: 2, reg: &$0.registers.l) },
        Operation(name: "SET 2,(HL)") { $0.set(bitToSet: 2, addr: $0.registers.hl) },
        Operation(name: "SET 2,A") { $0.set(bitToSet: 2, reg: &$0.registers.a) },
        Operation(name: "SET 3,B") { $0.set(bitToSet: 3, reg: &$0.registers.b) },
        Operation(name: "SET 3,C") { $0.set(bitToSet: 3, reg: &$0.registers.c) },
        Operation(name: "SET 3,D") { $0.set(bitToSet: 3, reg: &$0.registers.d) },
        Operation(name: "SET 3,E") { $0.set(bitToSet: 3, reg: &$0.registers.e) },
        Operation(name: "SET 3,H") { $0.set(bitToSet: 3, reg: &$0.registers.h) },
        Operation(name: "SET 3,L") { $0.set(bitToSet: 3, reg: &$0.registers.l) },
        Operation(name: "SET 3,(HL)") { $0.set(bitToSet: 3, addr: $0.registers.hl) },
        Operation(name: "SET 3,A") { $0.set(bitToSet: 3, reg: &$0.registers.a) },
        //0xEn
        Operation(name: "SET 4,B") { $0.set(bitToSet: 4, reg: &$0.registers.b) },
        Operation(name: "SET 4,C") { $0.set(bitToSet: 4, reg: &$0.registers.c) },
        Operation(name: "SET 4,D") { $0.set(bitToSet: 4, reg: &$0.registers.d) },
        Operation(name: "SET 4,E") { $0.set(bitToSet: 4, reg: &$0.registers.e) },
        Operation(name: "SET 4,H") { $0.set(bitToSet: 4, reg: &$0.registers.h) },
        Operation(name: "SET 4,L") { $0.set(bitToSet: 4, reg: &$0.registers.l) },
        Operation(name: "SET 4,(HL)") { $0.set(bitToSet: 4, addr: $0.registers.hl) },
        Operation(name: "SET 4,A") { $0.set(bitToSet: 4, reg: &$0.registers.a) },
        Operation(name: "SET 5,B") { $0.set(bitToSet: 5, reg: &$0.registers.b) },
        Operation(name: "SET 5,C") { $0.set(bitToSet: 5, reg: &$0.registers.c) },
        Operation(name: "SET 5,D") { $0.set(bitToSet: 5, reg: &$0.registers.d) },
        Operation(name: "SET 5,E") { $0.set(bitToSet: 5, reg: &$0.registers.e) },
        Operation(name: "SET 5,H") { $0.set(bitToSet: 5, reg: &$0.registers.h) },
        Operation(name: "SET 5,L") { $0.set(bitToSet: 5, reg: &$0.registers.l) },
        Operation(name: "SET 5,(HL)") { $0.set(bitToSet: 5, addr: $0.registers.hl) },
        Operation(name: "SET 5,A") { $0.set(bitToSet: 5, reg: &$0.registers.a) },
        //0xFn
        Operation(name: "SET 6,B") { $0.set(bitToSet: 6, reg: &$0.registers.b) },
        Operation(name: "SET 6,C") { $0.set(bitToSet: 6, reg: &$0.registers.c) },
        Operation(name: "SET 6,D") { $0.set(bitToSet: 6, reg: &$0.registers.d) },
        Operation(name: "SET 6,E") { $0.set(bitToSet: 6, reg: &$0.registers.e) },
        Operation(name: "SET 6,H") { $0.set(bitToSet: 6, reg: &$0.registers.h) },
        Operation(name: "SET 6,L") { $0.set(bitToSet: 6, reg: &$0.registers.l) },
        Operation(name: "SET 6,(HL)") { $0.set(bitToSet: 6, addr: $0.registers.hl) },
        Operation(name: "SET 6,A") { $0.set(bitToSet: 6, reg: &$0.registers.a) },
        Operation(name: "SET 7,B") { $0.set(bitToSet: 7, reg: &$0.registers.b) },
        Operation(name: "SET 7,C") { $0.set(bitToSet: 7, reg: &$0.registers.c) },
        Operation(name: "SET 7,D") { $0.set(bitToSet: 7, reg: &$0.registers.d) },
        Operation(name: "SET 7,E") { $0.set(bitToSet: 7, reg: &$0.registers.e) },
        Operation(name: "SET 7,H") { $0.set(bitToSet: 7, reg: &$0.registers.h) },
        Operation(name: "SET 7,L") { $0.set(bitToSet: 7, reg: &$0.registers.l) },
        Operation(name: "SET 7,(HL)") { $0.set(bitToSet: 7, addr: $0.registers.hl) },
        Operation(name: "SET 7,A") { $0.set(bitToSet: 7, reg: &$0.registers.a) }
    ]
}

//MARK: Instructions

extension CPU {
    
    //MARK: 8-bit loads
    
    //LD r1,n
    private mutating func loadPC(toReg: inout UInt8) {
        toReg = mmu.readByte(address: registers.pc)
        registers.pc.incr()
        clock += 2
    }
    
    //LD r,(nn)
    private mutating func loadPCAddr(toReg: inout UInt8) {
        let addr = mmu.readWord(address: registers.pc)
        toReg = mmu.readByte(address: addr)
        registers.pc.incr(by: 2)
        clock += 4
    }
    
    //LD (rr),n
    private mutating func loadPC(toAddr: UInt16) {
        let val = mmu.readByte(address: registers.pc)
        mmu.write(byte: val, to: toAddr)
        registers.pc.incr()
        clock += 3
    }
    
    //LD (nn),rr
    private mutating func loadPCAddr(fromReg: UInt8) {
        let addr = mmu.readWord(address: registers.pc)
        mmu.write(byte: fromReg, to: addr)
        registers.pc.incr(by: 2)
        clock += 4
    }
    
    //LD r1,r2
    private mutating func load(toReg: inout UInt8, fromReg: UInt8) {
        toReg = fromReg
        clock += 1
    }
    
    //LD r,(rr)
    private mutating func load(toReg: inout UInt8, fromAddr: UInt16) {
        toReg = mmu.readByte(address: fromAddr)
        clock += 2
    }
    
    //LD (rr),r
    private mutating func load(toAddr: UInt16, fromReg: UInt8) {
        mmu.write(byte: fromReg, to: toAddr)
        clock += 2
    }
    
    //LD r,(C)
    private mutating func loadCAddr(toReg: inout UInt8) {
        let addr = UInt16(0xFF00 | UInt16(registers.c))
        toReg = mmu.readByte(address: addr)
        clock += 2
    }
    
    //LD (C),r
    private mutating func loadToCAddr(fromReg: UInt8) {
        let addr = UInt16(0xFF00 | UInt16(registers.c))
        mmu.write(byte: fromReg, to: addr)
        clock += 2
    }
    
    //LDD (rr),r
    private mutating func loadAndDecr(toRegAddr: inout UInt16, fromReg: UInt8) {
        mmu.write(byte: fromReg, to: toRegAddr)
        toRegAddr -= 1
        clock += 2
    }
    
    //LDI r,(rr)
    private mutating func loadAndIncr(toReg: inout UInt8, fromRegAddr: inout UInt16) {
        toReg = mmu.readByte(address: fromRegAddr)
        fromRegAddr += 1
        clock += 2
    }
    
    //LDI (rr),r
    private mutating func loadAndIncr(toRegAddr: inout UInt16, fromReg: UInt8) {
        mmu.write(byte: fromReg, to: toRegAddr)
        toRegAddr += 1
        clock += 2
    }
    
    //LDH (n),r
    private mutating func loadPCAddrNibble(fromReg: inout UInt8) {
        let nibble = mmu.readByte(address: registers.pc)
        let addr = 0xFF00 | UInt16(nibble)
        mmu.write(byte: fromReg, to: addr)
        registers.pc.incr()
        clock += 3
    }
    
    //LDH r,(n)
    private mutating func loadPCAddrNibble(toReg: inout UInt8) {
        let addr = mmu.readByte(address: registers.pc)
        toReg = mmu.readByte(address: UInt16(addr) | 0xFF00)
        registers.pc.incr()
        clock += 3
    }
    
    //MARK: 16-bit loads
    
    //LD rr,(nn)
    private mutating func loadPC(toReg: inout UInt16) {
        toReg = mmu.readWord(address: registers.pc)
        registers.pc.incr(by: 2)
        clock += 3
    }
    
    //LD SP,(rr)
    private mutating func loadToSP(fromReg: UInt16) {
        registers.sp = fromReg
        clock += 2
    }
    
    //LD rr,(SP+n)
    private mutating func loadSPN(toReg: inout UInt16) {
        let n = Int32(Int8(bitPattern: mmu.readByte(address: registers.pc)))
        let sp = Int32(registers.sp)
        let result = sp + n
        toReg = UInt16(truncatingBitPattern: result)
        
        registers.flags = []
        if (sp ^ n ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (sp ^ n ^ result) & 0x100 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        registers.pc.incr()
        clock += 3
    }
    
    //LD (nn),rr
    private mutating func loadPCAddr(fromReg: UInt16) {
        let addr = mmu.readWord(address: registers.pc)
        mmu.write(word: fromReg, to: addr)
        registers.pc.incr(by: 2)
        clock += 5
    }
    
    //MARK: Push / Pop
    
    //PUSH rr
    private mutating func push(fromReg: UInt16) {
        registers.sp -= 2
        mmu.write(word: fromReg, to: registers.sp)
        clock += 4
    }
    
    //POP rr
    private mutating func pop(toReg: inout UInt16) {
        toReg = mmu.readWord(address: registers.sp)
        registers.sp += 2
        clock += 3
    }
    
    //MARK: 8-bit Arithmetic
    
    //ADD r,r
    private mutating func add(toReg: inout UInt8, fromReg: UInt8) {
        let to16 = UInt16(toReg)
        let from16 = UInt16(fromReg)
        let result = to16 + from16
        
        registers.flags = []
        if (to16 ^ from16 ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (to16 ^ from16 ^ result) & 0x100 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        toReg = UInt8(truncatingBitPattern: result)
        if toReg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 1
    }
    
    //ADD r,(rr)
    private mutating func add(toReg: inout UInt8, fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        add(toReg: &toReg, fromReg: val)
        clock += 1
    }
    
    //ADD r,n
    private mutating func addPC(toReg: inout UInt8) {
        add(toReg: &toReg, fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //ADC r,r
    private mutating func addCarry(toReg: inout UInt8, fromReg: UInt8) {
        let mod: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        add(toReg: &toReg, fromReg: fromReg + mod)
    }
    
    //ADC r,(rr)
    private mutating func addCarry(toReg: inout UInt8, fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        let mod: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        add(toReg: &toReg, fromReg: val + mod)
        clock += 1
    }
    
    //ADC r,n
    private mutating func addCarryPC(toReg: inout UInt8) {
        addCarry(toReg: &toReg, fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //SUB r
    private mutating func sub(fromReg: UInt8) {
        let a16 = UInt16(registers.a)
        let r16 = UInt16(fromReg)
        let result = a16 &- r16
        
        registers.flags = .subtract
        if (a16 ^ r16 ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (a16 ^ r16 ^ result) & 0x100 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        registers.a = UInt8(truncatingBitPattern: result)
        if registers.a == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 1
    }
    
    //SUB (rr)
    private mutating func sub(fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        sub(fromReg: val)
        clock += 1
    }
    
    //SUB n
    private mutating func subPC() {
        sub(fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //SBC r
    private mutating func subCarry(fromReg: UInt8) {
        let mod: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        sub(fromReg: fromReg + mod)
    }
    
    //SBC (rr)
    private mutating func subCarry(fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        subCarry(fromReg: val)
        clock += 1
    }
    
    //SBC n
    private mutating func subCarryPC() {
        subCarry(fromAddr: registers.pc)
        registers.pc.incr(by: 2)
    }
    
    //MARK: Bitwise
    
    //AND r
    private mutating func and(fromReg: UInt8) {
        registers.a &= fromReg
        registers.flags = registers.a == 0 ? [.halfCarry, .zero] : .halfCarry
        clock += 1
    }
    
    //AND (rr)
    private mutating func and(fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        and(fromReg: val)
        clock += 1
    }
    
    //AND n
    private mutating func andPC() {
        and(fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //OR r
    private mutating func or(fromReg: UInt8) {
        registers.a |= fromReg
        registers.flags = registers.a == 0 ? .zero : []
        clock += 1
    }
    
    //OR (rr)
    private mutating func or(fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        or(fromReg: val)
        clock += 1
    }
    
    //OR n
    private mutating func orPC() {
        or(fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //XOR r
    private mutating func xor(fromReg: UInt8) {
        registers.a ^= fromReg
        registers.flags = registers.a == 0 ? .zero : []
        clock += 1
    }
    
    //XOR (rr)
    private mutating func xor(fromAddr: UInt16) {
        let val = mmu.readByte(address: fromAddr)
        xor(fromReg: val)
        clock += 1
    }
    
    //XOR n
    private mutating func xorPC() {
        xor(fromAddr: registers.pc)
        registers.pc.incr()
    }
    
    //CP r
    private mutating func cp(withReg: UInt8) {
        let a16 = UInt16(registers.a)
        let r16 = UInt16(withReg)
        let result = a16 &- r16
        
        registers.flags = .subtract
        if (a16 ^ r16 ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (a16 ^ r16 ^ result) & 0x100 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        if result == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 1
    }
    
    //CP (rr)
    private mutating func cp(withAddr: UInt16) {
        let val = mmu.readByte(address: withAddr)
        cp(withReg: val)
        clock += 1
    }
    
    //CP n
    private mutating func cpPC() {
        cp(withAddr: registers.pc)
        registers.pc.incr()
    }
    
    //INC r
    private mutating func inc(reg: inout UInt8) {
        let old = reg
        reg = old &+ 1
        registers.flags.formIntersection(.fullCarry)
        if (old ^ 1 ^ reg) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        clock += 1
    }
    
    //INC (rr)
    private mutating func inc(withAddr: UInt16) {
        let val = mmu.readByte(address: withAddr)
        let result = val &+ 1
        registers.flags.formIntersection(.fullCarry)
        if (val ^ 1 ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if result == 0 {
            registers.flags.formUnion(.zero)
        }
        mmu.write(byte: result, to: withAddr)
        clock += 3
    }
    
    //DEC r
    private mutating func dec(reg: inout UInt8) {
        let old = reg
        reg = old &- 1
        registers.flags.formIntersection(.fullCarry)
        registers.flags.formUnion(.subtract)
        if (old ^ 1 ^ reg) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        clock += 1
    }
    
    //DEC (rr)
    private mutating func dec(withAddr: UInt16) {
        let val = mmu.readByte(address: withAddr)
        let result = val &- 1
        registers.flags.formIntersection(.fullCarry)
        registers.flags.formUnion(.subtract)
        if (val ^ 1 ^ result) & 0x10 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if result == 0 {
            registers.flags.formUnion(.zero)
        }
        mmu.write(byte: result, to: withAddr)
        clock += 3
    }
    
    //MARK: 16-bit Arithmetic
    
    //ADD rr,rr
    private mutating func add(toReg: inout UInt16, fromReg: UInt16) {
        let to32 = UInt32(toReg)
        let from32 = UInt32(fromReg)
        let result = to32 + from32
        
        registers.flags.formIntersection(.zero)
        if (to32 ^ from32 ^ result) & 0x1000 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (to32 ^ from32 ^ result) & 0x10000 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        toReg = UInt16(truncatingBitPattern: result)
        clock += 2
    }
    
    //ADD SP,n
    private mutating func addPC(toReg: inout UInt16) {
        let old = UInt32(toReg)
        let val = UInt32(mmu.readByte(address: registers.pc))
        let result = old &+ val
        
        registers.flags = []
        if (old ^ val ^ result) & 0x1000 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (old ^ val ^ result) & 0x10000 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        toReg = UInt16(truncatingBitPattern: result)
        registers.pc.incr()
        clock += 4
    }
    
    //INC rr
    private mutating func inc(reg: inout UInt16) {
        reg = reg &+ 1
        clock += 2
    }
    
    //DEC rr
    private mutating func dec(reg: inout UInt16) {
        reg = reg &- 1
        clock += 2
    }
    
    //MARK: Misc Instructions
    
    //SWAP r
    private mutating func swap(reg: inout UInt8) {
        reg = (reg >> 4) | (reg << 4)
        registers.flags = reg == 0 ? .zero : []
        clock += 2
    }
    
    //SWAP (rr)
    private mutating func swap(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        val = (val >> 4) | (val << 4)
        registers.flags = val == 0 ? .zero : []
        mmu.write(byte: val, to: addr)
        clock += 4
    }
    
    //DAA
    private mutating func daa() {
        var regA = UInt16(registers.a)
        
        if !registers.flags.contains(.subtract) {
            if registers.flags.contains(.halfCarry) || regA & 0x0F > 9 {
                regA = regA &+ 0x06
            }
            if registers.flags.contains(.fullCarry) || regA > 0x9F {
                regA = regA &+ 0x60
            }
        } else {
            if registers.flags.contains(.halfCarry) {
                regA = (regA &- 0x06) & 0xFF
            }
            if registers.flags.contains(.fullCarry) {
                regA = regA &- 0x60
            }
        }
        
        registers.flags.subtract([.halfCarry, .zero])
        
        if regA & 0x100 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        
        regA &= 0xFF
        
        if regA == 0 {
            registers.flags.formUnion(.zero)
        }
        
        registers.a = UInt8(regA)
        clock += 1
    }
    
    //CPL
    private mutating func cpl() {
        registers.a = ~registers.a
        registers.flags.formUnion([.subtract, .halfCarry])
        clock += 1
    }
    
    //CCF
    private mutating func ccf() {
        registers.flags.subtract([.subtract, .halfCarry])
        if registers.flags.contains(.fullCarry) {
            registers.flags.subtract(.fullCarry)
        } else {
            registers.flags.formUnion(.fullCarry)
        }
        clock += 1
    }
    
    //SCF
    private mutating func scf() {
        registers.flags.formIntersection(.zero)
        registers.flags.formUnion(.fullCarry)
        clock += 1
    }
    
    //NOP
    private mutating func nop() {
        clock += 1
    }
    
    //HALT
    private mutating func halt() {
        isHalted = true
        clock += 1
    }
    
    //STOP
    private mutating func stop() {
        isStopped = true
        clock += 1
    }
    
    //DI
    private mutating func di() {
        interruptsEnabled = false
        clock += 1
    }
    
    //EI
    private mutating func ei() {
        interruptsEnabled = true
        clock += 1
    }
    
    //RLC r
    // rotate left, left bit wraps to right
    private mutating func rlc(reg: inout UInt8, clockMod: UInt32) {
        let carry = (reg >> 7) & 1
        reg = (reg << 1) | carry
        
        registers.flags = []
        if carry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += clockMod
    }
    
    //RL r
    // 9-bit rotate left, left bit -> carry -> right bit
    private mutating func rl(reg: inout UInt8, clockMod: UInt32) {
        let newCarry = (reg >> 7) & 1
        let oldCarry: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        reg = (reg << 1) | oldCarry
        
        registers.flags = []
        if newCarry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += clockMod
    }
    
    //RLC (rr)
    private mutating func rlc(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        rlc(reg: &val, clockMod: 4)
        mmu.write(byte: val, to: addr)
    }
    
    //RL (rr)
    private mutating func rl(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        rl(reg: &val, clockMod: 4)
        mmu.write(byte: val, to: addr)
        
    }
    
    //RRC r
    private mutating func rrc(reg: inout UInt8, clockMod: UInt32) {
        let carry = (reg & 1) << 7
        reg = (reg >> 1) | carry
        
        registers.flags = []
        if carry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += clockMod
    }
    
    //RR r
    private mutating func rr(reg: inout UInt8, clockMod: UInt32) {
        let newCarry = reg & 1
        let oldCarry: UInt8 = registers.flags.contains(.fullCarry) ? 0x80 : 0
        reg = (reg >> 1) | oldCarry
        
        registers.flags = []
        if newCarry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 && clockMod == 2 {
            registers.flags.formUnion(.zero)
        }
        
        clock += clockMod
    }
    
    //RRC (rr)
    private mutating func rrc(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        rrc(reg: &val, clockMod: 4)
        mmu.write(byte: val, to: addr)
    }
    
    //RR (rr)
    private mutating func rr(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        rr(reg: &val, clockMod: 4)
        mmu.write(byte: val, to: addr)
    }
    
    //SLA r
    private mutating func sla(reg: inout UInt8) {
        let carry = (reg >> 7) & 1
        reg = reg << 1
        
        registers.flags = []
        if carry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 2
    }
    
    //SLA (rr)
    private mutating func sla(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        sla(reg: &val)
        mmu.write(byte: val, to: addr)
        clock += 2
    }
    
    //SRA r
    private mutating func sra(reg: inout UInt8) {
        let carry = reg & 1
        let msb = reg & 0x80
        reg = (reg >> 1) | msb
        
        registers.flags = []
        if carry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 2
    }
    
    //SRA (rr)
    private mutating func sra(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        sra(reg: &val)
        mmu.write(byte: val, to: addr)
        clock += 2
    }
    
    //SRL r
    private mutating func srl(reg: inout UInt8) {
        let carry = reg & 1
        reg = reg >> 1
        
        registers.flags = []
        if carry != 0 {
            registers.flags.formUnion(.fullCarry)
        } else {
            registers.flags.subtract(.fullCarry)
        }
        if reg == 0 {
            registers.flags.formUnion(.zero)
        }
        
        clock += 2
    }
    
    //SRL (rr)
    private mutating func srl(addr: UInt16) {
        var val = mmu.readByte(address: addr)
        srl(reg: &val)
        mmu.write(byte: val, to: addr)
        clock += 2
    }
    
    //BIT b,r
    private mutating func bit(bitToCheck: UInt8, reg: UInt8) {
        if ((reg >> bitToCheck) & 1) == 0 {
            registers.flags.formUnion(.zero)
        } else {
            registers.flags.subtract(.zero)
        }
        registers.flags.formUnion(.halfCarry)
        registers.flags.subtract(.subtract)
        clock += 2
    }
    
    //BIT b,(rr)
    private mutating func bit(bitToCheck: UInt8, addr: UInt16) {
        let val = mmu.readByte(address: addr)
        bit(bitToCheck: bitToCheck, reg: val)
        clock += 2
    }
    
    //SET b,r
    private mutating func set(bitToSet: UInt8, reg: inout UInt8) {
        reg = reg | (1 << bitToSet)
        clock += 2
    }
    
    //SET b,(rr)
    private mutating func set(bitToSet: UInt8, addr: UInt16) {
        var val = mmu.readByte(address: addr)
        set(bitToSet: bitToSet, reg: &val)
        mmu.write(byte: val, to: addr)
        clock += 2
    }
    
    //RES b,r
    private mutating func res(bitToReset: UInt8, reg: inout UInt8) {
        reg = reg & ~(1 << bitToReset)
        clock += 2
    }
    
    //RES b,(rr)
    private mutating func res(bitToReset: UInt8, addr: UInt16) {
        var val = mmu.readByte(address: addr)
        res(bitToReset: bitToReset, reg: &val)
        mmu.write(byte: val, to: addr)
        clock += 2
    }
    
    //JP nn
    private mutating func jp() {
        registers.pc = mmu.readWord(address: registers.pc)
        clock += 4
    }
    
    //JP cc,nn
    private mutating func jp(condition: Bool) {
        if condition {
            jp()
        } else {
            registers.pc.incr(by: 2)
            clock += 3
        }
    }
    
    //JP (HL)
    private mutating func jpHL() {
        registers.pc = registers.hl
        clock += 1
    }
    
    //JR n
    private mutating func jr() {
        let val = Int32(Int8(bitPattern: mmu.readByte(address: registers.pc)))
        registers.pc = UInt16(truncatingBitPattern: Int32(registers.pc) + Int32(val))
        registers.pc.incr()
        clock += 3
    }
    
    //JR cc,n
    private mutating func jr(condition: Bool) {
        if condition {
            jr()
        } else {
            registers.pc.incr()
            clock += 2
        }
    }
    
    //CALL cc,nn
    private mutating func call() {
        let jump = mmu.readWord(address: registers.pc)
        mmu.write(word: registers.pc + 2, to: registers.sp - 2)
        registers.pc = jump
        registers.sp -= 2
        clock += 6
    }
    
    private mutating func call(condition: Bool) {
        if condition {
            call()
        } else {
            registers.pc.incr(by: 2)
            clock += 3
        }
    }
    
    //RST n
    private mutating func rst(n: UInt16) {
        mmu.write(word: registers.pc, to: registers.sp - 2)
        registers.sp -= 2
        registers.pc = n
        clock += 4
    }
    
    //RET
    private mutating func ret() {
        registers.pc = mmu.readWord(address: registers.sp)
        registers.sp += 2
        clock += 4
    }
    
    //RET cc
    private mutating func ret(condition: Bool) {
        if condition {
            ret()
            clock += 1
        } else {
            clock += 2
        }
    }
    
    //RETI
    private mutating func reti() {
        ret()
        interruptsEnabled = true
    }
    
    //MARK: Extra Opcodes
    private mutating func extOps() {
        let opCode = mmu.readByte(address: registers.pc)
        let op = CPU.extOperations[Int(opCode)]
        
        print("mine: (pc: \(String(registers.pc, radix: 16))) (\(String(opCode, radix: 16))) \(op.name) (clock: \(timer.mCounter))")
        registers.pc.incr()
        op.instruction(cpu: &self)
    }
}
