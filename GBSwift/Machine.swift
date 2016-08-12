//
//  Machine.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/7/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import Foundation
import QuartzCore

class Machine {
    
    var displayLink: CADisplayLink?
    var gpu: GPU
    var mmu: MMU
    var cpu: CPU
    
    var firstTime: CFTimeInterval?
    var timesFired: UInt = 0
    
    init(renderable: Renderable) {
        gpu = GPU(renderable: renderable)
        mmu = MMU(bios: Machine.loadBIOS(), rom: Machine.loadROM(), gpu: gpu)
        cpu = CPU(mmu: mmu)
    }
    
    func start() {
        
        displayLink = CADisplayLink(target: self, selector: #selector(Machine.displayLinkFired(link:)))
        displayLink?.add(to: RunLoop.main, forMode: .defaultRunLoopMode)
        
        //let startDate = Date()
        
        //while true {
            
        //}
    }
    
    @objc func displayLinkFired(link: CADisplayLink) {
        //print("time: \(link.timestamp - lastTime)")
        if firstTime == nil {
            firstTime = link.timestamp
        }
        timesFired += 1
        
        //var clock = 17477
        var clock = 5000
        //var clock = 200
        
        while clock >= 0 {
            clock -= 1
            if self.mmu.inBios && self.cpu.registers.pc == 0x100 {
                
                //let interval = Date().timeIntervalSince(startDate)
                //let perSec = Int(Double(cpu.clock * 4) / interval)
                //print("per sec: \(perSec)")
                
                self.mmu.swapBios()
            }
            self.cpu.exec(mmu: &self.mmu)
            self.gpu.step(mmu: &self.mmu, clock: self.cpu.clock)
        }
    }
    
    //MARK: Private
    
    private static func loadBIOS() -> Data {
        return loadROM(named: "bios")
    }
    
    private static func loadROM() -> Data {
        return loadROM(named: "pokemon")
        //return loadROM(named: "cpu_instrs")
    }
    
    private static func loadROM(named: String) -> Data {
        let romURL = Bundle.main.url(forResource: named, withExtension: "gb")!
        return try! Data(contentsOf: romURL)
    }
}
