import pn, debug

######################################################
## This stuff is all essential for pypn to work - that
## means don't mess with it junior!

schemes = {}
scripts = {}

class SchemeMapping:
	def __init__(self, name):
		self.name = name

def registerScript(f, group, scriptName):
	scripts[f.func_name] = f
	pn.RegisterScript(f.func_name, group, scriptName)

def runScript(name):
	try:
		debug.OutputDebugString( name )
		scripts[name]()
	except KeyError:
		pass

def onCharAdded(c, doc):
	if not (c == '\n' or c == '\r'):
		return
	
	if not schemes.has_key(doc.GetCurrentScheme()):
		return
	
	scheme = schemes[doc.GetCurrentScheme()]
	
	if scheme and scheme.indenter:
		scheme.indenter(c, doc)

def getSchemeConfig(name):
	if not schemes.has_key(name):
		schemes[name] = SchemeMapping(name)
	return schemes[name]