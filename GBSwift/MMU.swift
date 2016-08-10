//
//  MMU.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/1/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import Foundation

protocol MMUProtocol {
    mutating func write(byte: UInt8, to address: UInt16)
    mutating func write(word: UInt16, to address: UInt16)
    func readByte(address: UInt16) -> UInt8
    func readWord(address: UInt16) -> UInt16
    mutating func requestInterrupt(_ interrupt: UInt8)
}

struct MMU: MMUProtocol {
    
    private(set) var inBios = true
    private let gpu: GPU
    private var memory = Data(repeating: 0, count: 0x10000)
    private var rom: Data
    
    init(bios: Data, rom: Data, gpu: GPU) {
        self.gpu = gpu
        self.rom = MMU.trim(rom: rom, toLength: 0x8000)
        let trimmedBios = MMU.trim(rom: bios, toLength: 0x100)
        
        memory[0..<0x8000] = self.rom[0..<0x8000]
        memory[0..<0x100] = trimmedBios[0..<0x100]
    }
    
    mutating func swapBios() {
        guard inBios else { return }
        inBios = false
        memory[0..<0x100] = self.rom[0..<0x100]
    }
    
    //MARK: MMUProtocol
    
    mutating func write(byte: UInt8, to address: UInt16) {
        if (0xFF00..<0xFF80).contains(address) {
            gpu.write(byte: byte, address: address)
        } else {
            memory[Int(address)] = byte
        }
    }
    
    mutating func write(word: UInt16, to address: UInt16) {
        write(byte: UInt8(word >> 8), to: address + 1)
        write(byte: UInt8(truncatingBitPattern: word), to: address)
    }
    
    func readByte(address: UInt16) -> UInt8 {
        if (0xFF00..<0xFF80).contains(address) {
            return gpu.readByte(address: address)
        } else {
            return memory[Int(address)]
        }
    }
    
    func readWord(address: UInt16) -> UInt16 {
        return (UInt16(readByte(address: address+1)) << 8) | UInt16(readByte(address: address))
    }
    
    func requestInterrupt(_ interrupt: UInt8) {
        //no-op
    }
    
    //MARK: Private
    
    static private func trim(rom: Data, toLength: Int) -> Data {
        var trimmed = rom
        if rom.count > toLength {
            trimmed = Data(rom[0..<toLength])
        } else if rom.count < toLength {
            trimmed.append(contentsOf: (rom.count..<toLength).map({ _ in return 0 as UInt8 }))
        }
        return trimmed
    }
}
