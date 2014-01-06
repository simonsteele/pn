###############################################################################
## Some basic script functions shipped with Programmer's Notepad by various
## authors including:
## Simon Steele
## Scott (wischeese)
## Benoit

import pn
import scintilla
import string
from pypn.decorators import script

@script("Sort Lines", "Text")
def SortLines():
	""" Sort Lines (By Scott (wischeese)) """
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	lsSelection = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	laLines = lsSelection.splitlines(0)
	laLines.sort()
	lsReplace = string.join(laLines, '\r\n' )
	editor.ReplaceSel(lsReplace)
	editor.EndUndoAction() 

def SetTarget(e, x, y):
	e.TargetStart = x
	e.TargetEnd = y
	
@script("Tabs to Spaces", "Text")
def TabsToSpaces():
	editor = scintilla.Scintilla(pn.CurrentDoc())
	
	tabSpaces = editor.TabWidth
	spaces = ""
	for x in range(tabSpaces):
		spaces = spaces + " "
	
	end = editor.Length
	
	SetTarget(editor, 0, end)
	editor.SearchFlags = 0
	editor.BeginUndoAction()
	
	pos = editor.SearchInTarget(1, "\t")
	
	while(pos != -1):
		l1 = editor.TargetEnd - editor.TargetStart
		editor.ReplaceTarget(tabSpaces, spaces)
		
		# adjust doc length
		end = end + tabSpaces - l1
		start = pos + tabSpaces
		
		if start >= end:
			pos = -1
		else:
			SetTarget(editor, start, end)
			pos = editor.SearchInTarget(1, "\t")
	
	editor.EndUndoAction()

@script("Upper case", "Text")
def UpperCase():
	""" Convert text to Upper Case by Benoit """
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	selText = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	selText = selText.upper()
	editor.ReplaceSel(selText)
	editor.EndUndoAction() 

@script("Lower case", "Text")
def LowerCase():
	""" Convert text to Lower Case by Benoit """
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	selText = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	selText = selText.lower()
	editor.ReplaceSel(selText)
	editor.EndUndoAction() 

@script("Capitalize case", "Text")
def Capitalize():
	""" Capitalise text by Benoit """
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	selText = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	selText = selText.capitalize()
	editor.ReplaceSel(selText)
	editor.EndUndoAction() 

@script("Title case", "Text")
def TitleCase():
	""" Title case text by Benoit """
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	selText = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	selText = selText.title()
	editor.ReplaceSel(selText)
	editor.EndUndoAction() 

@script("Invert case", "Text")
def InvertCase():
	editor = scintilla.Scintilla(pn.CurrentDoc())
	editor.BeginUndoAction()
	selText = editor.GetTextRange(editor.SelectionStart, editor.SelectionEnd)
	selText = selText.swapcase()
	editor.ReplaceSel(selText)
	editor.EndUndoAction()
