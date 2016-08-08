//
//  Machine.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/7/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import Foundation

struct Machine {
    
    func start() {
        var mmu = MMU(bios: loadBIOS(), rom: loadROM())
        var cpu = CPU(mmu: mmu)
        
        while true {
            if mmu.inBios && cpu.registers.pc == 0x100 {
                mmu.inBios = false
            }
            cpu.exec(mmu: &mmu)
        }
    }
    
    //MARK: Private
    
    private func loadBIOS() -> [UInt8] {
        return loadROM(named: "bios")
    }
    
    private func loadROM() -> [UInt8] {
        return loadROM(named: "pokemon")
        //return loadROM(named: "cpu_instrs")
    }
    
    private func loadROM(named: String) -> [UInt8] {
        let romURL = Bundle.main.url(forResource: named, withExtension: "gb")!
        let data = try! Data(contentsOf: romURL)
        var bytes = [UInt8](repeating: 0, count: data.count)
        data.copyBytes(to: &bytes, count: data.count)
        return bytes
    }
}
