import pn, debug

######################################################
## This stuff is all essential for pypn to work - that
## means don't mess with it junior!

schemes = {}
scripts = {}

class SchemeMapping:
	def __init__(self, name):
		self.name = name
		self.on_char_added = None
		self.indenter = None

def registerScript(f, group, scriptName):
	debug.OutputDebugString("group: " + str(group))
	debug.OutputDebugString("name: " + str(scriptName))
	scripts[f.func_name] = f
	pn.RegisterScript(f.func_name, group, scriptName)

def runScript(name):
	try:
		debug.OutputDebugString( name )
		scripts[name]()
	except KeyError:
		pass

def onCharAdded(c, doc):
	if not schemes.has_key(doc.GetCurrentScheme()):
		return
		
	scheme = schemes[doc.GetCurrentScheme()]
	
	if scheme and scheme.on_char_added != None:
		scheme.on_char_added(c, doc)
		
	if not (c == '\n' or c == '\r'):
		return
	
	if scheme and scheme.indenter != None:
		scheme.indenter(c, doc)

def getSchemeConfig(name):
	if not schemes.has_key(name):
		schemes[name] = SchemeMapping(name)
	return schemes[name]