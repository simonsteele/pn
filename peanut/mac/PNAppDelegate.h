//
//  PNAppDelegate.h
//  PeanutEditor
//
//  Created by Simon Steele on 11/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

//#import <Scintilla.h>
//#import "PNScintilla.h"
#import <Cocoa/Cocoa.h>
#import "ScintillaView.h"
#import "InfoBar.h"

@interface PNAppDelegate : NSObject <NSApplicationDelegate>

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSBox *editBox;

@end
