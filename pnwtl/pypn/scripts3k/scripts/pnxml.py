import pn
import scintilla
from pypn.decorators import script
import xml.parsers.expat as expat

@script("Validate", "Xml")
def ValidateXml():
	editor = scintilla.Scintilla(pn.CurrentDoc())
	text = editor.GetText(editor.Length)

	parser = expat.ParserCreate()

	try:
		parser.Parse(text, True)
	except expat.ExpatError as ex:
		pn.ClearOutput()
		pn.AddOutput("Error: " + str(ex))