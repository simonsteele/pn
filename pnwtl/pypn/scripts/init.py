# Standard PN Builtins
import pn, scintilla, debug

# Begin PyPN initialization, this flag can be checked
# by scripts to change behaviour during runtime for easy
# development
pn.initializing = True

# Import the rest of the pypn stuff
from pypn import *
from pypn.decorators import *
from pypn import scintillautils

scintillautils.MonkeyPatch()

######################################################
## Bring on the scripts!

import os

def import_libs(dir):
    """ Imports the libs, returns a list of the libraries. 
    Pass in dir to scan """
    
    library_list = [] 
    
    #debug.OutputDebugString(str(os.listdir(os.path.abspath(dir))))
    for f in os.listdir(os.path.abspath(dir)):       
        
        module_name, ext = os.path.splitext(f) # Handles no-extension files, etc.
        if ext == '.py': # Important, ignore .pyc/other files.
            try:
                module = __import__("scripts." + module_name, globals(), locals(), [])
                #debug.OutputDebugString( 'imported module: %s' % (module_name) )
                library_list.append(module)
            except Exception, ex:
                pn.AddOutput("Script Import Error with " + str(module_name) + ".py: " + str(ex))
    
    return library_list

import_libs(pn.AppPath() + "scripts")

# We're done with our pypn initialization stage
pn.initializing = False

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

@indenter("python")
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