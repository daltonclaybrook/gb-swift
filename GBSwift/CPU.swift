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
        registers.pc.incr()
        let opCode = mmu.readByte(address: registers.pc)
        let op = CPU.operations[Int(opCode)]
        op.instruction(cpu: &self)
    }
    
    mutating func reset() {
        registers = Registers()
        clock = 0
    }
    
    // MARK: Static
    
    static let operations: [Operation] = {
        let ops: [Operation] = [
            //TODO: add correct operations
            
            //0x0n
            Operation(name: "NOP") { $0.nop() },
            Operation(name: "LD B,n") { $0.load(toReg: &$0.registers.b) }
        ]
        return ops
    }()
    
}

//MARK: Instructions

extension CPU {

    //MARK: NOP
    
    private mutating func nop() {
        clock += 1
    }
    
    //MARK: 8-bit loads
    
    //LD r,n
    private mutating func load(toReg: inout UInt8) {
        toReg = mmu.readByte(address: registers.pc)
        registers.pc += 1
        clock += 2
    }
    
    //LD r,(nn)
    private mutating func loadAddr(toReg: inout UInt8) {
        let addr = mmu.readWord(address: registers.pc)
        toReg = mmu.readByte(address: addr)
        registers.pc.incr(by: 2)
        clock += 4
    }
    
    //LD r1,r2
    private mutating func load(toReg: inout UInt8, fromReg: UInt8) {
        toReg = fromReg
        clock += 1
    }
    
    //LD r,(rr)
    private mutating func load(toReg: inout UInt8, addr: UInt16) {
        toReg = mmu.readByte(address: addr)
        clock += 2
    }
}
