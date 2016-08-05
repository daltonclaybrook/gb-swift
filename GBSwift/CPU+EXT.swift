//
//  CPU+EXT.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/3/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

extension CPU {
    
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
}
