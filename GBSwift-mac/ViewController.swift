//
//  ViewController.swift
//  GBSwift-mac
//
//  Created by Dalton Claybrook on 8/8/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {

    override func viewDidLoad() {
        super.viewDidLoad()

        let queue = DispatchQueue(label: "fetch-decode-execute loop")
        queue.async {
            let machine = Machine()
            machine.start()
        }
    }

    override var representedObject: AnyObject? {
        didSet {
        // Update the view, if already loaded.
        }
    }


}

