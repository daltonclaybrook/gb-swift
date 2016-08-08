//
//  MMU.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/1/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

protocol MMUProtocol {
    mutating func write(byte: UInt8, to address: UInt16)
    mutating func write(word: UInt16, to address: UInt16)
    func readByte(address: UInt16) -> UInt8
    func readWord(address: UInt16) -> UInt16
    mutating func requestInterrupt(_ interrupt: UInt8)
}

struct MMU: MMUProtocol {
    
    var inBios = true
    private var bios: [UInt8]
    private var rom: [UInt8]
    private var wram = [UInt8](repeating: 0, count: 0x2000)
    private var eram = [UInt8](repeating: 0, count: 0x2000)
    private var zram = [UInt8](repeating: 0, count: 0x80)
    
    private(set) var vram = [UInt8](repeating: 0, count: 0x2000)
    private(set) var oam = [UInt8](repeating: 0, count: 0xA0)
    private(set) var memIO = [UInt8](repeating: 0, count: 0x80)
    
    init(bios: [UInt8], rom: [UInt8]) {
        self.bios = bios
        self.rom = rom
    }
    
    //MARK: MMUProtocol
    
    mutating func write(byte: UInt8, to address: UInt16) {
        switch address {
        case 0x0000..<0x8000:   // ROM 0 / ROM 1
            rom[Int(address)] = byte
        case 0x8000..<0xA000:   // VRAM
            vram[Int(address & 0x1FFF)] = byte
        case 0xA000..<0xC000:   // Cartridge (External) RAM
            eram[Int(address & 0x1FFF)] = byte
        case 0xC000..<0xFE00:   // Working RAM & Shadow
            wram[Int(address & 0x1FFF)] = byte
        case 0xFE00..<0xFEA0:   // CPU Object Attribute Memory
            oam[Int(address & 0xFF)] = byte
        case 0xFF00..<0xFF80:   // Memory-mapped I/O
            memIO[Int(address & 0x7F)] = byte
        case 0xFF80...0xFFFF:   // Zero-page RAM
            zram[Int(address & 0x7F)] = byte
        default:
            break
        }
    }
    
    mutating func write(word: UInt16, to address: UInt16) {
        write(byte: UInt8(word >> 8), to: address + 1)
        write(byte: UInt8(truncatingBitPattern: word), to: address)
    }
    
    func readByte(address: UInt16) -> UInt8 {
        switch address {
        case 0x0000..<0x1000:   // BIOS / ROM 0
            if inBios && address < 0x100 {
                return bios[Int(address)]
            } else {
                return rom[Int(address)]
            }
        case 0x1000..<0x8000:   // ROM 0 / ROM 1
            return rom[Int(address)]
        case 0x8000..<0xA000:   // VRAM
            return vram[Int(address & 0x1FFF)]
        case 0xA000..<0xC000:   // Cartridge (External) RAM
            return eram[Int(address & 0x1FFF)]
        case 0xC000..<0xFE00:   // Working RAM & Shadow
            return wram[Int(address & 0x1FFF)]
        case 0xFE00..<0xFEA0:   // CPU Object Attribute Memory
            return oam[Int(address & 0xFF)]
        case 0xFF00..<0xFF80:   // Memory-mapped I/O
            return memIO[Int(address & 0x7F)]
        case 0xFF80...0xFFFF:   // Zero-page RAM
            return zram[Int(address & 0x7F)]
        default:
            return 0
        }
    }
    
    func readWord(address: UInt16) -> UInt16 {
        return (UInt16(readByte(address: address+1)) << 8) | UInt16(readByte(address: address))
    }
    
    func requestInterrupt(_ interrupt: UInt8) {
        //no-op
    }
}
