#include "stdafx.h"
#include "styles.h"

/////////////////////////////////////////////////////////////////////////////////////
// StyleDetails

void StyleDetails::layer(StyleDetails& other)
{
	if((other.values & edvFontName) == 0)
		FontName = other.FontName;

	if((other.values & edvFontSize) == 0)
		FontSize = other.FontSize;

	if((other.values & edvForeColor) == 0)
		ForeColor = other.ForeColor;

	if((other.values & edvBackColor) == 0)
		BackColor = other.BackColor;

	if((other.values & edvBold) == 0)
		Bold = other.Bold;

	if((other.values & edvItalic) == 0)
		Italic = other.Italic;

	if((other.values & edvUnderline) == 0)
		Underline = other.Underline;

	if((other.values & edvEOLFilled) == 0)
		EOLFilled = other.EOLFilled;

	if((other.values & edvClass) == 0)
		classname = other.classname;
}

/////////////////////////////////////////////////////////////////////////////////////
// FullStyleDetails

FullStyleDetails::FullStyleDetails()
{
	Class = NULL;
	CustomClass = NULL;
	GroupClass = NULL;
	Style = NULL;
	CustomStyle = NULL;
}

void FullStyleDetails::Combine(const StyleDetails* defStyle, StyleDetails& into)
{
	// First we build from any style class or custom style class
	// If there is none, we build from the default style.
	if(CustomClass && Class)
	{
		into = *Class;
		into.layer(*CustomClass);
	}
	else if(Class)
		into = *Class;
	else if(GroupClass)
		into = *GroupClass;
	else
		into = *defStyle;
	
	// Now we layer on the actual style settings
	into.layer(*Style);

	// Now we customise the style with user choices
	if(CustomStyle)
		into.layer(*CustomStyle);

	// Reset values...
	into.values = 0;
}