import pn, scintilla

############################################################################
## These are parameter handlers, they're defined as [type]Handler where type
## is a type defined by the scintilla interface. We only handle the basics
## needed for the whitelist-supported set of commands.

def NoParamHandler(param):
	return None

def intParamHandler(param):
	return str(param)

def stringParamHandler(param):
	return repr(pn.StringFromPointer(param))

def positionParamHandler(param):
	return str(param)

def cellsParamHandler(param):
	return None
	
def textrangeParamHandler(param):
	return None

def findtextParamHandler(param):
	return None

def formatrangeParamHandler(param):
	return None

def stringresultParamHandler(param):
	return None

def colourParamHandler(param):
	return str(param)

def boolParamHandler(param):
	if param == 1:
		return "True"
	return "False"

def keymodParamHandler(param):
	return ""

############################################################################
## Map from scintilla message id to handlers and method names
## This is our whitelist of allowed and understood commands

handlers = {
	2001 : ("AddText", intParamHandler, stringParamHandler),
	2002 : ("AddStyledText", intParamHandler, cellsParamHandler),
	2003 : ("InsertText", positionParamHandler, stringParamHandler),
	2004 : ("ClearAll", NoParamHandler, NoParamHandler),
	2005 : ("ClearDocumentStyle", NoParamHandler, NoParamHandler),
	2011 : ("Redo", NoParamHandler, NoParamHandler),
	2013 : ("SelectAll", NoParamHandler, NoParamHandler),
	2014 : ("SetSavePoint", NoParamHandler, NoParamHandler),
	2015 : ("GetStyledText", NoParamHandler, textrangeParamHandler),
	2016 : ("CanRedo", NoParamHandler, NoParamHandler),
	2017 : ("MarkerLineFromHandle", intParamHandler, NoParamHandler),
	2018 : ("MarkerDeleteHandle", intParamHandler, NoParamHandler),
	2022 : ("PositionFromPoint", intParamHandler, intParamHandler),
	2023 : ("PositionFromPointClose", intParamHandler, intParamHandler),
	2024 : ("GotoLine", intParamHandler, NoParamHandler),
	2025 : ("GotoPos", positionParamHandler, NoParamHandler),
	2027 : ("GetCurLine", intParamHandler, stringresultParamHandler),
	2029 : ("ConvertEOLs", intParamHandler, NoParamHandler),
	2032 : ("StartStyling", positionParamHandler, intParamHandler),
	2033 : ("SetStyling", intParamHandler, intParamHandler),
	2040 : ("MarkerDefine", intParamHandler, intParamHandler),
	2041 : ("MarkerSetFore", intParamHandler, colourParamHandler),
	2042 : ("MarkerSetBack", intParamHandler, colourParamHandler),
	2043 : ("MarkerAdd", intParamHandler, intParamHandler),
	2044 : ("MarkerDelete", intParamHandler, intParamHandler),
	2045 : ("MarkerDeleteAll", intParamHandler, NoParamHandler),
	2046 : ("MarkerGet", intParamHandler, NoParamHandler),
	2047 : ("MarkerNext", intParamHandler, intParamHandler),
	2048 : ("MarkerPrevious", intParamHandler, intParamHandler),
	2049 : ("MarkerDefinePixmap", intParamHandler, stringParamHandler),
	2466 : ("MarkerAddSet", intParamHandler, intParamHandler),
	2476 : ("MarkerSetAlpha", intParamHandler, intParamHandler),
	2058 : ("StyleResetDefault", NoParamHandler, NoParamHandler),
	2486 : ("StyleGetFont", intParamHandler, stringresultParamHandler),
	2067 : ("SetSelFore", boolParamHandler, colourParamHandler),
	2068 : ("SetSelBack", boolParamHandler, colourParamHandler),
	2070 : ("AssignCmdKey", keymodParamHandler, intParamHandler),
	2071 : ("ClearCmdKey", keymodParamHandler, NoParamHandler),
	2072 : ("ClearAllCmdKeys", NoParamHandler, NoParamHandler),
	2073 : ("SetStylingEx", intParamHandler, stringParamHandler),
	2078 : ("BeginUndoAction", NoParamHandler, NoParamHandler),
	2079 : ("EndUndoAction", NoParamHandler, NoParamHandler),
	2084 : ("SetWhitespaceFore", boolParamHandler, colourParamHandler),
	2085 : ("SetWhitespaceBack", boolParamHandler, colourParamHandler),
	2100 : ("AutoCShow", intParamHandler, stringParamHandler),
	2101 : ("AutoCCancel", NoParamHandler, NoParamHandler),
	2102 : ("AutoCActive", NoParamHandler, NoParamHandler),
	2103 : ("AutoCPosStart", NoParamHandler, NoParamHandler),
	2104 : ("AutoCComplete", NoParamHandler, NoParamHandler),
	2105 : ("AutoCStops", NoParamHandler, stringParamHandler),
	2108 : ("AutoCSelect", NoParamHandler, stringParamHandler),
	2117 : ("UserListShow", intParamHandler, stringParamHandler),
	2405 : ("RegisterImage", intParamHandler, stringParamHandler),
	2408 : ("ClearRegisteredImages", NoParamHandler, NoParamHandler),
	2150 : ("FindText", intParamHandler, findtextParamHandler),
	2151 : ("FormatRange", boolParamHandler, formatrangeParamHandler),
	2153 : ("GetLine", intParamHandler, stringresultParamHandler),
	2160 : ("SetSel", positionParamHandler, positionParamHandler),
	2161 : ("GetSelText", NoParamHandler, stringresultParamHandler),
	2162 : ("GetTextRange", NoParamHandler, textrangeParamHandler),
	2163 : ("HideSelection", boolParamHandler, NoParamHandler),
	2164 : ("PointXFromPosition", NoParamHandler, positionParamHandler),
	2165 : ("PointYFromPosition", NoParamHandler, positionParamHandler),
	2166 : ("LineFromPosition", positionParamHandler, NoParamHandler),
	2167 : ("PositionFromLine", intParamHandler, NoParamHandler),
	2168 : ("LineScroll", intParamHandler, intParamHandler),
	2169 : ("ScrollCaret", NoParamHandler, NoParamHandler),
	2170 : ("ReplaceSel", NoParamHandler, stringParamHandler),
	2172 : ("Null", NoParamHandler, NoParamHandler),
	2173 : ("CanPaste", NoParamHandler, NoParamHandler),
	2174 : ("CanUndo", NoParamHandler, NoParamHandler),
	2175 : ("EmptyUndoBuffer", NoParamHandler, NoParamHandler),
	2176 : ("Undo", NoParamHandler, NoParamHandler),
	2177 : ("Cut", NoParamHandler, NoParamHandler),
	2178 : ("Copy", NoParamHandler, NoParamHandler),
	2179 : ("Paste", NoParamHandler, NoParamHandler),
	2180 : ("Clear", NoParamHandler, NoParamHandler),
	2181 : ("SetText", NoParamHandler, stringParamHandler),
	2182 : ("GetText", intParamHandler, stringresultParamHandler),
	2194 : ("ReplaceTarget", intParamHandler, stringParamHandler),
	2195 : ("ReplaceTargetRE", intParamHandler, stringParamHandler),
	2197 : ("SearchInTarget", intParamHandler, stringParamHandler),
	2200 : ("CallTipShow", positionParamHandler, stringParamHandler),
	2201 : ("CallTipCancel", NoParamHandler, NoParamHandler),
	2202 : ("CallTipActive", NoParamHandler, NoParamHandler),
	2203 : ("CallTipPosStart", NoParamHandler, NoParamHandler),
	2204 : ("CallTipSetHlt", intParamHandler, intParamHandler),
	2220 : ("VisibleFromDocLine", intParamHandler, NoParamHandler),
	2221 : ("DocLineFromVisible", intParamHandler, NoParamHandler),
	2235 : ("WrapCount", intParamHandler, NoParamHandler),
	2226 : ("ShowLines", intParamHandler, intParamHandler),
	2227 : ("HideLines", intParamHandler, intParamHandler),
	2231 : ("ToggleFold", intParamHandler, NoParamHandler),
	2232 : ("EnsureVisible", intParamHandler, NoParamHandler),
	2233 : ("SetFoldFlags", intParamHandler, NoParamHandler),
	2234 : ("EnsureVisibleEnforcePolicy", intParamHandler, NoParamHandler),
	2266 : ("WordStartPosition", positionParamHandler, boolParamHandler),
	2267 : ("WordEndPosition", positionParamHandler, boolParamHandler),
	2276 : ("TextWidth", intParamHandler, stringParamHandler),
	2279 : ("TextHeight", intParamHandler, NoParamHandler),
	2282 : ("AppendText", intParamHandler, stringParamHandler),
	2287 : ("TargetFromSelection", NoParamHandler, NoParamHandler),
	2288 : ("LinesJoin", NoParamHandler, NoParamHandler),
	2289 : ("LinesSplit", intParamHandler, NoParamHandler),
	2290 : ("SetFoldMarginColour", boolParamHandler, colourParamHandler),
	2291 : ("SetFoldMarginHiColour", boolParamHandler, colourParamHandler),
	2300 : ("LineDown", NoParamHandler, NoParamHandler),
	2301 : ("LineDownExtend", NoParamHandler, NoParamHandler),
	2302 : ("LineUp", NoParamHandler, NoParamHandler),
	2303 : ("LineUpExtend", NoParamHandler, NoParamHandler),
	2304 : ("CharLeft", NoParamHandler, NoParamHandler),
	2305 : ("CharLeftExtend", NoParamHandler, NoParamHandler),
	2306 : ("CharRight", NoParamHandler, NoParamHandler),
	2307 : ("CharRightExtend", NoParamHandler, NoParamHandler),
	2308 : ("WordLeft", NoParamHandler, NoParamHandler),
	2309 : ("WordLeftExtend", NoParamHandler, NoParamHandler),
	2310 : ("WordRight", NoParamHandler, NoParamHandler),
	2311 : ("WordRightExtend", NoParamHandler, NoParamHandler),
	2312 : ("Home", NoParamHandler, NoParamHandler),
	2313 : ("HomeExtend", NoParamHandler, NoParamHandler),
	2314 : ("LineEnd", NoParamHandler, NoParamHandler),
	2315 : ("LineEndExtend", NoParamHandler, NoParamHandler),
	2316 : ("DocumentStart", NoParamHandler, NoParamHandler),
	2317 : ("DocumentStartExtend", NoParamHandler, NoParamHandler),
	2318 : ("DocumentEnd", NoParamHandler, NoParamHandler),
	2319 : ("DocumentEndExtend", NoParamHandler, NoParamHandler),
	2320 : ("PageUp", NoParamHandler, NoParamHandler),
	2321 : ("PageUpExtend", NoParamHandler, NoParamHandler),
	2322 : ("PageDown", NoParamHandler, NoParamHandler),
	2323 : ("PageDownExtend", NoParamHandler, NoParamHandler),
	2324 : ("EditToggleOvertype", NoParamHandler, NoParamHandler),
	2325 : ("Cancel", NoParamHandler, NoParamHandler),
	2326 : ("DeleteBack", NoParamHandler, NoParamHandler),
	2327 : ("Tab", NoParamHandler, NoParamHandler),
	2328 : ("BackTab", NoParamHandler, NoParamHandler),
	2329 : ("NewLine", NoParamHandler, NoParamHandler),
	2330 : ("FormFeed", NoParamHandler, NoParamHandler),
	2331 : ("VCHome", NoParamHandler, NoParamHandler),
	2332 : ("VCHomeExtend", NoParamHandler, NoParamHandler),
	2333 : ("ZoomIn", NoParamHandler, NoParamHandler),
	2334 : ("ZoomOut", NoParamHandler, NoParamHandler),
	2335 : ("DelWordLeft", NoParamHandler, NoParamHandler),
	2336 : ("DelWordRight", NoParamHandler, NoParamHandler),
	2518 : ("DelWordRightEnd", NoParamHandler, NoParamHandler),
	2337 : ("LineCut", NoParamHandler, NoParamHandler),
	2338 : ("LineDelete", NoParamHandler, NoParamHandler),
	2339 : ("LineTranspose", NoParamHandler, NoParamHandler),
	2404 : ("LineDuplicate", NoParamHandler, NoParamHandler),
	2340 : ("LowerCase", NoParamHandler, NoParamHandler),
	2341 : ("UpperCase", NoParamHandler, NoParamHandler),
	2342 : ("LineScrollDown", NoParamHandler, NoParamHandler),
	2343 : ("LineScrollUp", NoParamHandler, NoParamHandler),
	2344 : ("DeleteBackNotLine", NoParamHandler, NoParamHandler),
	2345 : ("HomeDisplay", NoParamHandler, NoParamHandler),
	2346 : ("HomeDisplayExtend", NoParamHandler, NoParamHandler),
	2347 : ("LineEndDisplay", NoParamHandler, NoParamHandler),
	2348 : ("LineEndDisplayExtend", NoParamHandler, NoParamHandler),
	2349 : ("HomeWrap", NoParamHandler, NoParamHandler),
	2450 : ("HomeWrapExtend", NoParamHandler, NoParamHandler),
	2451 : ("LineEndWrap", NoParamHandler, NoParamHandler),
	2452 : ("LineEndWrapExtend", NoParamHandler, NoParamHandler),
	2453 : ("VCHomeWrap", NoParamHandler, NoParamHandler),
	2454 : ("VCHomeWrapExtend", NoParamHandler, NoParamHandler),
	2455 : ("LineCopy", NoParamHandler, NoParamHandler),
	2401 : ("MoveCaretInsideView", NoParamHandler, NoParamHandler),
	2350 : ("LineLength", intParamHandler, NoParamHandler),
	2351 : ("BraceHighlight", positionParamHandler, positionParamHandler),
	2352 : ("BraceBadLight", positionParamHandler, NoParamHandler),
	2353 : ("BraceMatch", positionParamHandler, NoParamHandler),
	2366 : ("SearchAnchor", NoParamHandler, NoParamHandler),
	2367 : ("SearchNext", intParamHandler, stringParamHandler),
	2368 : ("SearchPrev", intParamHandler, stringParamHandler),
	2371 : ("UsePopUp", boolParamHandler, NoParamHandler),
	2375 : ("CreateDocument", NoParamHandler, NoParamHandler),
	2376 : ("AddRefDocument", NoParamHandler, intParamHandler),
	2377 : ("ReleaseDocument", NoParamHandler, intParamHandler),
	2390 : ("WordPartLeft", NoParamHandler, NoParamHandler),
	2391 : ("WordPartLeftExtend", NoParamHandler, NoParamHandler),
	2392 : ("WordPartRight", NoParamHandler, NoParamHandler),
	2393 : ("WordPartRightExtend", NoParamHandler, NoParamHandler),
	2394 : ("SetVisiblePolicy", intParamHandler, intParamHandler),
	2395 : ("DelLineLeft", NoParamHandler, NoParamHandler),
	2396 : ("DelLineRight", NoParamHandler, NoParamHandler),
	2399 : ("ChooseCaretX", NoParamHandler, NoParamHandler),
	2400 : ("GrabFocus", NoParamHandler, NoParamHandler),
	2402 : ("SetXCaretPolicy", intParamHandler, intParamHandler),
	2403 : ("SetYCaretPolicy", intParamHandler, intParamHandler),
	2413 : ("ParaDown", NoParamHandler, NoParamHandler),
	2414 : ("ParaDownExtend", NoParamHandler, NoParamHandler),
	2415 : ("ParaUp", NoParamHandler, NoParamHandler),
	2416 : ("ParaUpExtend", NoParamHandler, NoParamHandler),
	2417 : ("PositionBefore", positionParamHandler, NoParamHandler),
	2418 : ("PositionAfter", positionParamHandler, NoParamHandler),
	2419 : ("CopyRange", positionParamHandler, positionParamHandler),
	2420 : ("CopyText", intParamHandler, stringParamHandler),
	2424 : ("GetLineSelStartPosition", intParamHandler, NoParamHandler),
	2425 : ("GetLineSelEndPosition", intParamHandler, NoParamHandler),
	2426 : ("LineDownRectExtend", NoParamHandler, NoParamHandler),
	2427 : ("LineUpRectExtend", NoParamHandler, NoParamHandler),
	2428 : ("CharLeftRectExtend", NoParamHandler, NoParamHandler),
	2429 : ("CharRightRectExtend", NoParamHandler, NoParamHandler),
	2430 : ("HomeRectExtend", NoParamHandler, NoParamHandler),
	2431 : ("VCHomeRectExtend", NoParamHandler, NoParamHandler),
	2432 : ("LineEndRectExtend", NoParamHandler, NoParamHandler),
	2433 : ("PageUpRectExtend", NoParamHandler, NoParamHandler),
	2434 : ("PageDownRectExtend", NoParamHandler, NoParamHandler),
	2435 : ("StutteredPageUp", NoParamHandler, NoParamHandler),
	2436 : ("StutteredPageUpExtend", NoParamHandler, NoParamHandler),
	2437 : ("StutteredPageDown", NoParamHandler, NoParamHandler),
	2438 : ("StutteredPageDownExtend", NoParamHandler, NoParamHandler),
	2439 : ("WordLeftEnd", NoParamHandler, NoParamHandler),
	2440 : ("WordLeftEndExtend", NoParamHandler, NoParamHandler),
	2441 : ("WordRightEnd", NoParamHandler, NoParamHandler),
	2442 : ("WordRightEndExtend", NoParamHandler, NoParamHandler),
	2444 : ("SetCharsDefault", NoParamHandler, NoParamHandler),
	2445 : ("AutoCGetCurrent", NoParamHandler, NoParamHandler),
	2446 : ("Allocate", intParamHandler, NoParamHandler),
	2447 : ("TargetAsUTF8", NoParamHandler, stringresultParamHandler),
	2448 : ("SetLengthForEncode", intParamHandler, NoParamHandler),
	2449 : ("EncodedFromUTF8", stringParamHandler, stringresultParamHandler),
	2456 : ("FindColumn", intParamHandler, intParamHandler),
	2459 : ("ToggleCaretSticky", NoParamHandler, NoParamHandler),
	2469 : ("SelectionDuplicate", NoParamHandler, NoParamHandler),
	2504 : ("IndicatorFillRange", intParamHandler, intParamHandler),
	2505 : ("IndicatorClearRange", intParamHandler, intParamHandler),
	2506 : ("IndicatorAllOnFor", intParamHandler, NoParamHandler),
	2507 : ("IndicatorValueAt", intParamHandler, intParamHandler),
	2508 : ("IndicatorStart", intParamHandler, intParamHandler),
	2509 : ("IndicatorEnd", intParamHandler, intParamHandler),
	2519 : ("CopyAllowLine", NoParamHandler, NoParamHandler),
	3001 : ("StartRecord", NoParamHandler, NoParamHandler),
	3002 : ("StopRecord", NoParamHandler, NoParamHandler),
	4003 : ("Colourise", positionParamHandler, positionParamHandler),
	4007 : ("LoadLexerLibrary", NoParamHandler, stringParamHandler),
	4008 : ("GetProperty", stringParamHandler, stringresultParamHandler),
	4009 : ("GetPropertyExpanded", stringParamHandler, stringresultParamHandler),
	2369 : ("SetCaretPolicy", intParamHandler, intParamHandler)
}

searchFields = {
	pn.SearchType.stFindNext : ["FindText", "MatchWholeWord", "MatchCase", "UseRegExp", "SearchBackwards", "LoopOK", "UseSlashes"],
	pn.SearchType.stReplace : ["FindText", "MatchWholeWord", "MatchCase", "UseRegExp", "SearchBackwards", "LoopOK", "UseSlashes", "ReplaceText", "ReplaceInSelection"],
	pn.SearchType.stReplaceAll : ["FindText", "MatchWholeWord", "MatchCase", "UseRegExp", "SearchBackwards", "LoopOK", "UseSlashes", "ReplaceText", "ReplaceInSelection"]
}

searchActions = {
	pn.SearchType.stFindNext : "FindNext",
	pn.SearchType.stReplace : "ReplaceOnce",
	pn.SearchType.stReplaceAll : "ReplaceAll"
}

############################################################################
## Script Recording

SCI_REPLACESEL = 2170

class Recorder:
	""" The recorder class essentially implements IRecorder in python code,
	the instance is passed to a shim in PyPN which calls the relevant python
	instance methods. """
	
	def __init__(self):
		""" A recording is starting, we set up the basic script text """
		self.script = "import pn, scintilla\r\n\r\n@script(\"New Script\", \"Recorded\")\r\ndef RecordedScript():\r\n\tdoc = pn.CurrentDoc()\r\n\tsci = scintilla.Scintilla(doc)\r\n"
		self.insertbuf = ""
		self.lineends = ['\r', '\n']

	def recordScintillaAction(self, message, wParam, lParam):
		""" The user performed an action in scintilla, we handle it here 
		and add it to the script """
		command = self._handleMessage(message, wParam, lParam)
		if command != None:
			self.script = self.script + command
	
	def recordSearchAction(self, type, options, result):
		""" The user performed a search action """
		self._flushbuf()
		self.script = self.script + "\topt = pn.GetUserSearchOptions()\r\n"
		for opt in searchFields[type]:
			self.script = self.script + "\topt." + opt + " = " + repr(getattr(options, opt)) + "\r\n"
		self.script = self.script + "\tdoc." + searchActions[type] + "(opt)\r\n"
		self.script = self.script + "\t# When recording, the result of this operation was: " + repr(result) + "\r\n"
	
	def stopRecording(self):
		""" The user wants to stop recording, or the document the recording 
		was made in is being closed. Read the document in as a script ready
		to run and cancel the recording settings. """
		self._flushbuf()
		doc = pn.NewDocument("python")
		sci = scintilla.Scintilla(doc)
		sci.ReplaceSel(self.script)
		pn.EvalDocument(doc)
		self.script = None

	def _handleMessage(self, message, wParam, lParam):
		""" Deal with a scintilla record message """
		if message == SCI_REPLACESEL:
			# Text insertion
			text = pn.StringFromPointer(lParam)
			
			# See if we need to flush the insertion buffer
			if len(self.insertbuf) > 0 and (len(text) == 0 or ((self.insertbuf[-1] in self.lineends) and (not text[0] in self.lineends))):
				self._flushbuf()
			
			# Store away the insert
			self.insertbuf = self.insertbuf + text
			
		elif message in handlers:
			# Other message
			self._flushbuf()
			(action, param1, param2) = handlers[message]
			p1 = param1(wParam)
			p2 = param2(lParam)
			action = "\tsci." + action + "(" 
			if p1 != None:
				action = action + p1 + ", "
			if p2 != None:
				action = action + p2
			return action + ")\r\n"
		# We don't have a handler for this message, or we buffered it so we 
		# don't script it
		return None
	
	def _flushbuf(self):
		""" Flush any built up character buffer """
		if len(self.insertbuf) > 0:
			self.script += "\tsci.ReplaceSel(" + repr(self.insertbuf) + ")\r\n"
			self.insertbuf = ""