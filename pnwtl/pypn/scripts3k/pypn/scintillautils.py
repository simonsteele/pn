import scintilla

def __SetTarget(self, start, end):
    self.TargetStart = start
    self.TargetEnd = end

def __ReplaceAll(self, pattern, replacement, flags = 0):
    self.ReplaceAllInRange(pattern, replacement, 0, self.Length, flags)

def __ReplaceAllInRange(self, pattern, replacement, start, end, flags = 0):
    self.BeginUndoAction()
    
    self.SetTarget(start, end)
    self.SearchFlags = 0
    self.BeginUndoAction()
    
    patternLength = len(pattern)
    replacementLength = len(replacement)
    pos = self.SearchInTarget(patternLength, pattern)
    
    while(pos != -1):
        l1 = self.TargetEnd - self.TargetStart
        self.ReplaceTarget(replacementLength, replacement)
        
        # adjust doc length
        end = end + replacementLength - l1
        start = pos + replacementLength
        
        if start >= end:
            pos = -1
        else:
            self.SetTarget(start, end)
            pos = self.SearchInTarget(patternLength, pattern)
    
    self.EndUndoAction()

def MonkeyPatch():
    scintilla.Scintilla.SetTarget = __SetTarget
    scintilla.Scintilla.ReplaceAll = __ReplaceAll
    scintilla.Scintilla.ReplaceAllInRange = __ReplaceAllInRange