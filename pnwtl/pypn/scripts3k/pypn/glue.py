import pn, debug, scintilla
from pypn import record
import sys, string

######################################################
## This stuff is all essential for pypn to work - that
## means don't mess with it junior!

schemes = {}
scripts = {}
oldstdout = None

class SchemeMapping:
	def __init__(self, name):
		self.name = name
		self.on_char_added = None
		self.indenter = None

class StdOutCapture:
	""" Simple output capturer """
	def __init__(self):
		self.output = ""
	
	def write(self, s):
		self.output = self.output + s
	
	def getOutput(self):
		return self.output

def registerScript(f, group, scriptName):
	""" This is called by the script decorator to register
	a script. """
	scripts[f.__name__] = f
	pn.RegisterScript(f.__name__, group, scriptName)

def runScript(name):
	""" This is called by PN to run a script by name. """
	try:
		scripts[name]()
	except KeyError:
		pass

def onCharAdded(c, doc):
	""" Method called when a character is added, default behaviour manages calling indenters
	and also calls any method registered with glue.schemes[scheme].on_char_added """
	if not doc.CurrentScheme in schemes:
		return
		
	scheme = schemes[doc.CurrentScheme]
	
	if scheme and scheme.on_char_added != None:
		scheme.on_char_added(c, doc)
		
	if not (c == '\n' or c == '\r'):
		return
	
	if scheme and scheme.indenter != None:
		scheme.indenter(c, doc)
	
def onDocLoad(doc):
	""" Method called when a document is loaded into PN """
	pass
	
def onDocSave(filename, doc):
	""" Method called when a document is about to be saved to a file """
	pass

def onDocSaved(doc):
	""" Method called when a document has been saved """
	pass

def onModifiedChanged(modified, doc):
	""" Method called when the modified flag is changed """
	pass

def onWriteProtectChanged(protected, doc):
	""" Method called when the read only status of a document is changed """
	pass

def getSchemeConfig(name):
	""" Get a pypn scheme configuration, used to hook up indenter 
	functions and character added handlers """
	if not name in schemes:
		schemes[name] = SchemeMapping(name)
	return schemes[name]

def startRecording():
	""" PN calls this to start recording a script, all recording actions
	are delegated to the Recorder class """
	return record.Recorder()

def startCapturingStdOut():
	""" PN calls this to start capturing stdout """
	global oldstdout
	oldstdout = sys.stdout
	sys.stdout = StdOutCapture()

def finishCapturingStdOut():
	""" PN calls this to finish stdout capture and get the output. """
	global oldstdout
	
	if (oldstdout != None):
		ret = sys.stdout.getOutput()
		sys.stdout = oldstdout
		oldstdout = None
		ret = string.rstrip(ret, "\r\n")
		return ret
	else:
		return ""

def evalScript(script):
	script = string.replace(script, "\r\n", "\n")
	script = string.replace(script, "\r", "\n")
	exec(script)
	return ""

def evalCommand():
	pass
	
def evalCommandEnter():
	pass
