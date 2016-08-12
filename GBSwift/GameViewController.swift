//
//  GameViewController.swift
//  GBSwift
//
//  Created by Dalton Claybrook on 8/12/16.
//  Copyright © 2016 Claybrook Software. All rights reserved.
//

import UIKit

class GameViewController: UIViewController {
    
    @IBOutlet weak var screenView: ScreenView!
    private var machine: Machine!
    
    //MARK: Public
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        machine = Machine(renderable: screenView)
        machine.start()
    }
}
