//
//  ScreenView.swift
//  GLTest
//
//  Created by Dalton Claybrook on 8/11/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import UIKit
import CoreGraphics

class ScreenView: UIView {
    
    static let imageWidth = 160
    static let imageHeight = 144
    
    var framebuffer = Data(repeating: 0, count: ScreenView.imageWidth * ScreenView.imageHeight * 3)
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        guard let context = UIGraphicsGetCurrentContext(),
            let dataProvider = CGDataProvider(data: framebuffer) else { return }
    
        let bitsPerComponent = 8
        let bitsPerPixel = bitsPerComponent * 3
        let bytesPerRow = ScreenView.imageWidth * 3
        let colorSpace = CGColorSpaceCreateDeviceRGB()
        let bitmapInfo = CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue)
        
        if let image = CGImage(width: ScreenView.imageWidth,
                            height: ScreenView.imageHeight,
                            bitsPerComponent: bitsPerComponent,
                            bitsPerPixel: bitsPerPixel,
                            bytesPerRow: bytesPerRow,
                            space: colorSpace,
                            bitmapInfo: bitmapInfo,
                            provider: dataProvider,
                            decode: nil,
                            shouldInterpolate: false,
                            intent: .defaultIntent) {
            
            context.draw(in: bounds, image: image)
        }
    }
}

extension ScreenView: Renderable {
    
    func update(framebuffer: Data) {
        self.framebuffer = framebuffer
        setNeedsDisplay()
    }
}
