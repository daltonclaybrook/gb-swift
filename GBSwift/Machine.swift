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
        let gpu = GPU()
        var mmu = MMU(bios: loadBIOS(), rom: loadROM(), gpu: gpu)
        var cpu = CPU(mmu: mmu)
        
        let startDate = Date()
        
        while true {
            if mmu.inBios && cpu.registers.pc == 0x100 {
                
                let interval = Date().timeIntervalSince(startDate)
                let perSec = Int(Double(cpu.clock * 4) / interval)
                print("per sec: \(perSec)")
                
                mmu.swapBios()
            }
            cpu.exec(mmu: &mmu)
            gpu.step(mmu: &mmu, clock: cpu.clock)
        }
    }
    
    //MARK: Private
    
    private func loadBIOS() -> Data {
        return loadROM(named: "bios")
    }
    
    private func loadROM() -> Data {
        return loadROM(named: "pokemon")
        //return loadROM(named: "cpu_instrs")
    }
    
    private func loadROM(named: String) -> Data {
        let romURL = Bundle.main.url(forResource: named, withExtension: "gb")!
        return try! Data(contentsOf: romURL)
    }
}
