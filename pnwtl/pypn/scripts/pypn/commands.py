############################################################################
## Implements extensible VIM-style commands in PN 2.1
##
## Currently focusing on implementing most commands here: http://www.stanford.edu/~jacobm/vim.html

import pn, scintilla
import re

if pn.initializing:
	import glue
else:
	import pypn.glue

def evalCommand(text):
	""" This is just a very simple prototype of VI-style commands """
	slen = len(text)
	if slen == 0:
		return ""
	
	if slen > 1:
		# We're beyond single-letter commands
		
		if text[0] in ['c','d','y']:
			return _handleChangeCommand(text[0], text[1:])
		
		# check for repeated single-letter command:
		m = re.match("^([0-9]+)([^0-9])$", text)
		if m != None:
			return _handleCommand(m.group(2), int(m.group(1)))
			
	elif slen == 1:
		# Try for a single-letter command
		return _handleCommand(text, 1)
		
	return text

def evalCommandEnter(text):
	""" Handle a command that the user has pressed Enter on """
	slen = len(text)
	if slen == 0:
		return ""
	
	if text[0] == '/':
		# Search
		so = pn.SearchOptions()
		so.FindText = text[1:]
		pn.CurrentDoc().FindNext(so)
		return ""
	
	return ""

def _handleChangeCommand(command, text):
	""" Handle the c, d and y commands """
	m = re.match("^([0-9]+)?((i[w\\(\\[])|[w$ld]|t(.))?$", text)
	if m == None:
		return command + text
	
	s = scintilla.Scintilla(pn.CurrentDoc())
	
	target = _getTarget(s)
	
	try:
		return _handleChangeCommandInner(command, text, m, s)
	finally:
		_setTarget(s, target[0], target[1])

def _handleChangeCommandInner(command, text, m, s):
	""" Do all command handling, target restoration and undo block handled externally """
	pos = s.CurrentPos
	end = -1
	repetitions = 1
	if m.group(1) != None:
		repetitions = int(m.group(1))
	what = m.group(2)
	remainder = m.group(4)
	
	# Calculate our change target:
	if what == None:
		return command + text
	elif what == "w":
		end = s.WordEndPosition(pos, True)
		end = _findFurtherWordEnds(s, end, repetitions - 1)
	elif what == "$":
		lineAtEnd = s.LineFromPosition(s.Length)
		line = s.LineFromPosition(pos) + (repetitions - 1)
		if line > lineAtEnd:
			line = lineAtEnd
		end = s.GetLineEndPosition(line)
	elif what == "l":
		end = pos + repetitions
	elif what[0] == "t":
		end = pos
		for x in range(0, repetitions):
			pn.AddOutput("\nLooking for " + remainder + " at " + str(end))
			end = _findNext(s, remainder[0], end)
	elif what == "d":
		startLine = s.LineFromPosition(pos)
		pos = s.PositionFromLine(startLine)
		
		endLine = startLine + repetitions
		if endLine >= s.LineCount:
			endLine = s.LineCount - 1
			end = s.GetLineEndPosition(endLine)
		else:
			end = s.PositionFromLine(endLine)
	elif what == "iw":
		# In word
		end = s.WordEndPosition(pos, True)
		pos = s.WordStartPosition(pos, True)
		end = _findFurtherWordEnds(s, end, repetitions - 1)
	elif what == "i(":
		# In braces	
		pos, end = _getBraceRange(s, '(', ')')
	elif what == "i[":
		# In square braces
		pos, end = _getBraceRange(s, '[', ']')
	
	if pos == -1 or end == -1:
		pn.AddOutput("Failed to find the range to alter")
		# Give up, we couldn't get a good position set.
		return ""
	
	_setTarget(s, pos, end)
	
	if command == "d":
		# Delete
		s.ReplaceTarget(0, "")
		return ""
		
	elif command == "c":
		# Go back to the editor for overwriting the target
		return _overwriteTargetMode(s)
		
	elif command == "y":
		yankText = s.GetTextRange(pos, end)
		pn.SetClipboardText(yankText)
		s.ReplaceTarget(0, "")
		return ""

def _findFurtherWordEnds(s, pos, repetitions):
	for x in range(0, repetitions):
		pos = pos + 1
		if pos > s.Length:
			pos = s.Length
			break
		pos = s.WordEndPosition(pos, True)
	return pos

def _getBraceRange(s, startBrace, endBrace):
	""" Find the range between a set of braces """
	start = _findPrev(s, startBrace, s.CurrentPos)
	if start == -1:
		return (-1, 0)
	end = s.BraceMatch(start)
	if end == -1:
		return (0, -1)
	
	# start + 1 because we want the range inside the bracket, not outside
	return (start + 1, end)

def _insertMode(s):
	""" Put the focus back in the editor, e.g. insert mode """
	s.SetFocus()
	return ""

def _overwriteTargetMode(s):
	""" Begin overwriting the selected target, and put the focus back in the editor """
	s.BeginOverwriteTarget()
	s.SetFocus()
	return ""

def _getTarget(s):
	return (s.TargetStart, s.TargetEnd)

def _setTarget(s, start, end):
	s.TargetStart = start
	s.TargetEnd = end

def _findNext(s, char, pos):
	""" Find the next instance of a character, or -1 if not found """
	match = ord(char)
	end = s.Length
	pos = pos + 1
	while pos < end:
		if s.GetCharAt(pos) == match:
			return pos
		pos = pos + 1
	
	return -1

def _findPrev(s, char, pos):
	""" Find the previous instance of a character, or -1 if not found """
	match = ord(char)
	pos = pos - 1
	while pos >= 0:
		if s.GetCharAt(pos) == match:
			return pos
		pos = pos - 1
	
	return -1

def _handleCommand(text, repetitions):
	""" Handle a simple command """
	sci = scintilla.Scintilla(pn.CurrentDoc())
	
	for x in xrange(repetitions):
		if text == 'j':
			sci.LineDown()
		elif text == 'k':
			sci.LineUp()
		elif text == 'h':
			sci.CharLeft()
		elif text == 'l':
			sci.CharRight()
		elif text == '^':
			sci.Home()
		elif text == '$':
			sci.LineEnd()
		elif text == 'w':
			sci.WordRight()
		elif text == 'b':
			sci.WordLeft()
		elif text == 'u':
			sci.Undo()
		elif text == 'i':
			return _insertMode(sci)
		else:
			return text
	
	return ""

# Hook up our handlers:
glue.evalCommand = evalCommand;
glue.evalCommandEnter = evalCommandEnter;