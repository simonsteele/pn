import pn
from pypn.decorators import script

@script("Hello!")
def testScript():
	pn.AddOutput("Hello From Python!")
