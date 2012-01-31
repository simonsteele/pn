//
//  PNScintilla.h
//  PeanutEditor
//
//  Created by Simon Steele on 12/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef PeanutEditor_PNScintilla_h
#define PeanutEditor_PNScintilla_h

#include "../../pnwtl/ScintillaImpl.h"
#import "ScintillaView.h"

/**
 * Implement CScintillaImpl in terms of the Cocoa Scintilla implementation.
 */
class PNScintilla : public CScintillaImpl
{
public:
    PNScintilla(Scintilla::ScintillaCocoa* scintilla) : m_scintilla(scintilla) {}
    
    virtual long SPerform(long Msg, WPARAM wParam=0, LPARAM lParam=0)
    {
        return m_scintilla->WndProc(Msg, wParam, lParam);
    }
    
private:
    Scintilla::ScintillaCocoa* m_scintilla;
};

#endif
