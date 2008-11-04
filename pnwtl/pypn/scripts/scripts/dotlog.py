import scintilla, pn, pypn.glue, time

oldDocLoad = pypn.glue.onDocLoad

def docLoad(doc):	
	""" docLoad handler to implement notepad .LOG functionality"""
	
	# Get the edit component:
	s = scintilla.Scintilla(doc)
	
	# Get the first line:
	lineLength = s.LineLength(0)
	text = s.GetText(0, lineLength)
	
	# If we have .LOG then add a blank line and then the date and time
	if text == ".LOG":
		timestr = "\r\n\r\n" + time.asctime(time.localtime()) + "\r\n"
		s.AppendText(len(timestr), timestr)
		
		s.DocumentEnd()
	
	oldDocLoad(doc)

pypn.glue.onDocLoad = docLoad