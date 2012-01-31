//
//  PNAppDelegate.m
//  PeanutEditor
//
//  Created by Simon Steele on 11/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "PNAppDelegate.h"
#import "PNScintilla.h"

@implementation PNAppDelegate

@synthesize window = _window;
@synthesize editBox = _editBox;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Insert code here to initialize your application
    // Manually set up the scintilla editor. Create an instance and dock it to our edit host.
    // Leave some free space around the new view to avoid overlapping with the box borders.
    NSRect newFrame = _editBox.frame;
    // newFrame.origin.y += 10;
    // newFrame.size.width -= 2 * newFrame.origin.x;
    // newFrame.size.height -= 3 * newFrame.origin.y;
    
    ScintillaView* editor = [[[ScintillaView alloc] initWithFrame: newFrame] autorelease];
    
    PNScintilla* wrapper = new PNScintilla(editor.backend);// [[[PNScintilla alloc] scintilla: editor->backend] autorelease];
    
    [_window.contentView addSubview: editor];
    //[_editBox.contentView addSubview: editor];
    //[_window setAutoresizesSubviews: YES];
    //[_window setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];

}

@end
