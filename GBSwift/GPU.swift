//
//  GPU.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/8/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

enum GPUMode: UInt8 {
    case scanlineOAM = 2
    case scanlineVRAM = 3
    case hBlank = 0
    case vBlank = 1
}

struct GPU {
    static let scanlineOAMTime: UInt32 = 20
    static let scanlineVRAMTime: UInt32 = 43
    static let hBlankTime: UInt32 = 51
    static let vBlankTime: UInt32 = 114
    
    static let screenWidth: UInt8 = 160
    static let screenHeight: UInt8 = 144
    
    static let lineYAddress: UInt16 = 0xFF44
    static let statusAddress: UInt16 = 0xFF41
    
    var lastClock: UInt32 = 0
    var modeClock: UInt32 = 0
    
    init(mmu: inout MMU) {
        set(mode: .scanlineOAM, mmu: &mmu)
    }
}

extension GPU {
    
    //MARK: Public
    
    mutating func step(mmu: inout MMU, clock: UInt32) {
        let delta = clock - lastClock
        lastClock = clock
        modeClock += delta
        
        let mode = getMode(mmu: mmu)
        switch mode {
        case .scanlineOAM where modeClock >= GPU.scanlineOAMTime:
            modeClock %= GPU.scanlineOAMTime
            set(mode: .scanlineVRAM, mmu: &mmu)
        case .scanlineVRAM where modeClock >= GPU.scanlineVRAMTime:
            modeClock %= GPU.scanlineVRAMTime
            set(mode: .hBlank, mmu: &mmu)
            //TODO: write a scanline to the framebuffer
        case .hBlank where modeClock >= GPU.hBlankTime:
            modeClock %= GPU.hBlankTime
            let line = getLine(mmu: mmu) + 1
            set(line: line, mmu: &mmu)
            
            if line < GPU.screenHeight - 1 {
                set(mode: .scanlineOAM, mmu: &mmu)
            } else {
                set(mode: .vBlank, mmu: &mmu)
                //TODO: Draw the framebuffer to the screen
            }
        case .vBlank where modeClock >= GPU.vBlankTime:
            modeClock %= GPU.vBlankTime
            var line = getLine(mmu: mmu) + 1
            
            if line >= GPU.screenHeight + 10 {
                set(mode: .scanlineOAM, mmu: &mmu)
                line = 0
            }
            set(line: line, mmu: &mmu)
        default:
            break
        }
    }
    
    //MARK: Private
    
    private func set(line: UInt8, mmu: inout MMU) {
        mmu.write(byte: line, to: GPU.lineYAddress)
    }
    
    private func getLine(mmu: MMU) -> UInt8 {
        return mmu.readByte(address: GPU.lineYAddress)
    }
    
    private func set(mode: GPUMode, mmu: inout MMU) {
        var status = mmu.readByte(address: GPU.statusAddress)
        status = (status & 0xFC) | (mode.rawValue & 0x03)
        mmu.write(byte: status, to: GPU.statusAddress)
    }
    
    private func getMode(mmu: MMU) -> GPUMode {
        let mode = mmu.readByte(address: GPU.statusAddress) & 0x03
        return GPUMode(rawValue: mode)!
    }
}
