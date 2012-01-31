//
//  main.m
//  PeanutEditor
//
//  Created by Simon Steele on 11/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include "../../pnwtl/l10n.h"
#include "../../pnwtl/extapp.h"
#include "../../libpeanut/mac/core/osxstringloader.h"

int main(int argc, char *argv[])
{
    L10N::StringLoader::SetLoader(new PN::L10N::OsxStringLoader());
    
    App app;
    app.Init();
    
    return NSApplicationMain(argc, (const char **)argv);
}
