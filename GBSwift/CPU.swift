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
        //TODO: add correct operations
        
        //0x0n
        Operation(name: "NOP") { $0.nop() },
        Operation(name: "LD B,n") { $0.loadPC(toReg: &$0.registers.b) },
        Operation(name: "LD B,(HL)") { $0.load(toReg: &$0.registers.b, fromAddr: $0.registers.hl) }
    ]
    
}

//MARK: Instructions

extension CPU {

    //MARK: NOP
    
    private mutating func nop() {
        clock += 1
    }
    
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
        registers.pc.incr()
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
    private mutating func loadAndIncr(fromRegAddr: inout UInt16, toReg: inout UInt8) {
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
    
    //MARK: 16-bit loads
    
    //LD rr,(nn)
    private mutating func loadPC(toReg: inout UInt16) {
        toReg = mmu.readWord(address: registers.pc)
        registers.pc.incr()
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
        registers.pc.incr()
        clock += 5
    }
    
    //MARK: Push / Pop
    
    //PUSH rr
    private mutating func push(fromReg: UInt16) {
        registers.sp -= 2
        mmu.write(word: fromReg, to: registers.sp)
        clock += 2
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
        if result == 0 {
            registers.flags.formUnion(.zero)
        }
        
        toReg = UInt8(truncatingBitPattern: result)
        clock += 1
    }
    
    //ADD r,(rr)
    private mutating func add(fromAddr: UInt16, toReg: inout UInt8) {
        let val = mmu.readByte(address: fromAddr)
        add(toReg: &toReg, fromReg: val)
        clock += 1
    }
    
    //ADD r,n
    private mutating func addPC(toReg: inout UInt8) {
        add(fromAddr: registers.pc, toReg: &toReg)
        registers.pc.incr()
    }
    
    //ADC r,r
    private mutating func addCarry(fromReg: UInt8, toReg: inout UInt8) {
        let mod: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        add(toReg: &toReg, fromReg: fromReg + mod)
    }
    
    //ADC r,(rr)
    private mutating func addCarry(fromAddr: UInt16, toReg: inout UInt8) {
        let val = mmu.readByte(address: fromAddr)
        let mod: UInt8 = registers.flags.contains(.fullCarry) ? 1 : 0
        add(toReg: &toReg, fromReg: val + mod)
        clock += 1
    }
    
    //ADC r,n
    private mutating func addCarryPC(toReg: inout UInt8) {
        addCarry(fromAddr: registers.pc, toReg: &toReg)
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
        if result == 0 {
            registers.flags.formUnion(.zero)
        }
        
        registers.a = UInt8(truncatingBitPattern: result)
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
        let result = to32 &+ from32
        
        registers.flags = []
        if (to32 ^ from32 ^ result) & 0x1000 != 0 {
            registers.flags.formUnion(.halfCarry)
        }
        if (to32 ^ from32 ^ result) & 0x10000 != 0 {
            registers.flags.formUnion(.fullCarry)
        }
        if result == 0 {
            registers.flags.formUnion(.zero)
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
}
