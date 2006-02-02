import glue

######################################################
## Experimental decorator things...
def indenter(scheme):
	def decorator(f):
		s = glue.getSchemeConfig(scheme)
		s.indenter = f
		return f
	return decorator

def script(name=None, group="Python"):
	def decorator(f):
		if name == None:
			scriptName = f.func_name
		else:
			scriptName = name
		glue.registerScript(f, group, scriptName)
		return f
	return decorator