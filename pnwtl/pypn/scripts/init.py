import pn, scintilla, debug

schemes = {}

class SchemeMapping:
	def __init__(self, name):
		self.name = name

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
		
######################################################
## Individual functions (eventually to be split into 
## other files.

def findPrevLineLastChar(p, sci):
	while p > 0:
		p = p - 1
		c = chr( sci.GetCharAt(p) )
		
		# Look for a non-whitespace character ending the previous line.
		if not c in ['\n','\r','\t',' ']:
			return c
	return None

def python_indent(c, doc):
	sci = scintilla.Scintilla(doc)
	if c == '\n' or c == '\r':
		pos = sci.CurrentPos
		line = sci.LineFromPosition( pos )
		
		lc = findPrevLineLastChar( pos, sci )
		
		# If the previous line ended with a colon, then indent
		if lc == ':':
			indent = sci.GetLineIndentation( line )
			
			# The DumbIndent system may already have indented this line...
			previndent = sci.GetLineIndentation( line - 1 )
			if indent == previndent or indent == 0:
				indent += 4
				sci.IndentLine( line, indent )

# Hook up the python indenter.
s = getSchemeConfig("python")
s.indenter = python_indent