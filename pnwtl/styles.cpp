/**
 * @file styles.cpp
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "styles.h"

/////////////////////////////////////////////////////////////////////////////////////
// Utilities

/**
 * Take a string containing keywords and possible extraneous whitespace and formatting
 * and turn into a single line of space separated keywords.
 */
std::string NormaliseKeywords(const std::string& in)
{
	std::string response;
	response.reserve(in.length());

	bool space(true);
	const char* p(in.c_str());
	while(*p)
	{
		switch(*p)
		{
			// ignore:
			case '\r':
				break;
			// collapse:
			case '\n':
			case '\t':
			case ' ':
			{
				if(!space)
				{
					response += ' ';
					space = true;
				}
			}
			break;
			// add:
			default:
			{
				response += *p;
				space = false;
			}
			break;
		}

		p++;
	}

	if(response.length() && response[response.length()-1] == ' ')
		response.erase(response.length()-1);

	return response;
}

int chartoval(TCHAR inp)
{
	int Result = 0;
	
	if (inp >= '0' && inp <= '9') 
	{
		Result = inp - 48;
	}
	else if (inp >= 'a' && inp <= 'f')
	{
		Result = inp - 87;
	}
	else if (inp >= 'A' && inp <= 'F') 
	{
		Result = inp - 55;
	}
  
	return Result;
}

COLORREF PNStringToColor(LPCTSTR input)
{
  	LPCTSTR	Part;
	int		res;

	//b,g,r - output format in hex $bbggrr
	//r,g,b - input format in string, rr,gg,bb
	// Default colour
	res = ::GetSysColor(COLOR_WINDOWTEXT);
	// only works for xxxxxx colours...
	if (_tcslen(input) != 6)
	{
		return res;
	}
	
	Part = input;
	res = (0x10 * chartoval(Part[0]));
	if (Part[1] != _T('0'))
	{
		res = res + (chartoval(Part[1]));
	}
	Part += 2;
	res += (0x1000 * chartoval(Part[0]));
	res += (0x100 * chartoval(Part[1]));
	Part += 2;
	res += (0x100000 * chartoval(Part[0]));
	res += (0x10000 * chartoval(Part[1]));

	return res;
}

bool PNStringToBool(LPCTSTR input)
{
	return (input[0] == 'T' || input[0] == 't');
}

/////////////////////////////////////////////////////////////////////////////////////
// EditorColours

EditorColours::EditorColours() : values(0)
{
}

const EditorColours& EditorColours::operator = (const EditorColours& copy)
{
	values = copy.values;
	crSelFore = copy.crSelFore;
	crSelBack = copy.crSelBack;
	crCaret = copy.crCaret;
	crIG = copy.crIG;
	crMarkAll = copy.crMarkAll;
	crSmartHL = copy.crSmartHL;
	crTemplateField = copy.crTemplateField;

	return *this;
}

void EditorColours::SetColour(Colours colour, COLORREF setColour)
{
	switch(colour)
	{
	case ecSelFore:
		crSelFore = setColour;
		break;
	case ecSelBack:
		crSelBack = setColour;
		break;
	case ecCaret:
		crCaret = setColour;
		break;
	case ecIndentG:
		crIG = setColour;
		break;
	case ecMarkAll:
		crMarkAll = setColour;
		break;
	case ecSmartHL:
		crSmartHL = setColour;
		break;
	case ecTemplateField:
		crTemplateField = setColour;
		break;
	}

	values |= colour;
}

bool EditorColours::HasColour(Colours colour) const
{
	return (values & colour) != 0;
}

bool EditorColours::HasColours() const
{
	return (values != 0);
}

/**
 * @return True if colour configured, false otherwise
 */
bool EditorColours::GetColour(Colours colour, COLORREF& theColour) const
{
	if( (values & colour) == 0)
		return false;
	
	switch(colour)
	{
		case ecSelFore:
			theColour = crSelFore;
			return true;
		case ecSelBack:
			theColour = crSelBack;
			return true;
		case ecCaret:
			theColour = crCaret;
			return true;
		case ecIndentG:
			theColour = crIG;
			return true;
		case ecMarkAll:
			theColour = crMarkAll;
			return true;
		case ecSmartHL:
			theColour = crSmartHL;
			return true;
		case ecTemplateField:
			theColour = crTemplateField;
			return true;
	}
		
	return false;
}

void EditorColours::SetFromXml(const XMLAttributes& atts)
{
	LPCTSTR szKey, szValue;
	int val;
	for(int i = 0; i < atts.getCount(); i++)
	{
		szKey = atts.getName(i);
		szValue = atts.getValue(i);
		val = (szValue[0] == _T('-') ? CLR_NONE : PNStringToColor(szValue));

		if(_tcscmp(szKey, _T("selFore")) == 0)
		{
			SetColour(ecSelFore, (DWORD)val);
		}
		else if(_tcscmp(szKey, _T("selBack")) == 0)
		{
			SetColour(ecSelBack, (DWORD)val);
		}
		else if(_tcscmp(szKey, _T("caret")) == 0)
		{
			SetColour(ecCaret, (DWORD)val);
		}
		else if(_tcscmp(szKey, _T("indentGuides")) == 0)
		{
			SetColour(ecIndentG, (DWORD)val);
		}
		else if (_tcscmp(szKey, _T("markAll")) == 0)
		{
			SetColour(ecMarkAll, (DWORD)val);
		}
		else if (_tcscmp(szKey, _T("smartHighlight")) == 0)
		{
			SetColour(ecSmartHL, (DWORD)val);
		}
		else if (_tcscmp(szKey, _T("templateField")) == 0)
		{
			SetColour(ecTemplateField, (DWORD)val);
		}
	}
}

void EditorColours::SendColours(CScintilla* pSc) const
{
	if((values & ecSelFore) != 0)
	{
		if((int)crSelFore == CLR_NONE)
			pSc->SPerform(SCI_SETSELFORE, false, 0);
		else
			pSc->SPerform(SCI_SETSELFORE, true, crSelFore);
	}
	
	if ((values & ecSelBack) != 0)
	{
		pSc->SPerform(SCI_SETSELBACK, true, crSelBack);
	}
	
	if ((values & ecCaret) != 0)
	{
		pSc->SPerform(SCI_SETCARETFORE, crCaret);
	}
	
	if ((values & ecIndentG) != 0)
	{
		pSc->SPerform(SCI_STYLESETFORE, STYLE_INDENTGUIDE, crIG);
	}

	if (values & ecMarkAll)
	{
		pSc->SPerform(SCI_INDICSETFORE, INDIC_MARKALL, crMarkAll);
	}

	if (values & ecSmartHL)
	{
		pSc->SPerform(SCI_INDICSETFORE, INDIC_SMARTHIGHLIGHT, crSmartHL);
	}

	if (values & ecTemplateField)
	{
		pSc->SPerform(SCI_INDICSETFORE, INDIC_TEXTCLIPFIELD, crTemplateField);
	}
}

/**
 * This applies any colours in other to this object.
 */
void EditorColours::Combine(const EditorColours* other)
{
	if (other->values & ecSelFore)
	{
		SetColour(ecSelFore, other->crSelFore);
	}

	if (other->values & ecSelBack)
	{
		SetColour(ecSelBack, other->crSelBack);
	}

	if (other->values & ecCaret)
	{
		SetColour(ecCaret, other->crCaret);
	}

	if (other->values & ecIndentG)
	{
		SetColour(ecIndentG, other->crIG);
	}
	
	if (other->values & ecMarkAll)
	{
		SetColour(ecMarkAll, other->crMarkAll);
	}

	if (other->values & ecSmartHL)
	{
		SetColour(ecSmartHL, other->crSmartHL);
	}

	if (other->values & ecTemplateField)
	{
		SetColour(ecTemplateField, other->crTemplateField);
	}
}

void EditorColours::Clear()
{
	values = 0;
}

/////////////////////////////////////////////////////////////////////////////////////
// StyleDetails

StyleDetails::StyleDetails()
{
	Key = 0;
	//FontName = "Courier New";
	FontSize = 10;
	ForeColor = RGB(0,0,0);
	BackColor = RGB(255,255,255);
	Bold = false;
	Italic = false;
	Underline = false;
	EOLFilled = false;
	Hotspot = false;
	values = 0;
}

StyleDetails::StyleDetails(const StyleDetails& copy)
	:Key(copy.Key)
	,FontName(copy.FontName)
	,FontSize(copy.FontSize)
	,ForeColor(copy.ForeColor)
	,BackColor(copy.BackColor)
	,Bold(copy.Bold)
	,Italic(copy.Italic)
	,Underline(copy.Underline)
	,EOLFilled(copy.EOLFilled)
	,Hotspot(copy.Hotspot)

	,values(copy.values)
    ,name(copy.name)
	,classname(copy.classname)
{
}

StyleDetails& StyleDetails::operator = (const StyleDetails& copy)
{
	Key = copy.Key;
	FontName = copy.FontName;
	FontSize = copy.FontSize;
	ForeColor = copy.ForeColor;
	BackColor = copy.BackColor;
	Bold = copy.Bold;
	Italic = copy.Italic;
	Underline = copy.Underline;
	EOLFilled = copy.EOLFilled;
	Hotspot = copy.Hotspot;

	values = copy.values;
    name = copy.name;
	classname = copy.classname;
	
	return *this;
}

/**
 * This method should only compare parts of the style that
 * users can change - others need not be compared.
 */
bool StyleDetails::operator == (const StyleDetails& compare) const
{
	return (
		Key == compare.Key &&
		FontName == compare.FontName &&
		FontSize == compare.FontSize &&
		ForeColor == compare.ForeColor &&
		BackColor == compare.BackColor &&
		Bold == compare.Bold &&
		Italic == compare.Italic &&
		Underline == compare.Underline &&
		EOLFilled == compare.EOLFilled &&
		//values == copy.values &&
		classname == compare.classname //&&
		//name == compare.name
		);
}

bool StyleDetails::operator != (const StyleDetails& compare) const
{
	return !(*this == compare);
}

/**
 * This function sets the "values" bit mask with
 * all the values that are different from those in compare.
 * This only takes into account values that the user may
 * change - not hotspots for example.
 */
void StyleDetails::compareTo(const StyleDetails& compare)
{
	values = 0 |
		((FontName == compare.FontName) ? 0 : edvFontName) |
		((FontSize == compare.FontSize) ? 0 : edvFontSize) |
		((ForeColor == compare.ForeColor) ? 0 : edvForeColor) |
		((BackColor == compare.BackColor) ? 0 : edvBackColor) |
		((Bold == compare.Bold) ? 0 : edvBold) |
		((Italic == compare.Italic) ? 0 : edvItalic) |
		((Underline == compare.Underline) ? 0 : edvUnderline) |
		((EOLFilled == compare.EOLFilled) ? 0 : edvEOLFilled) |
		((classname == compare.classname) ? 0 : edvClass)//&&
		//values == copy.values &&
		//name == compare.name;
		;
}

/**
 * This sets any values that are not included in the values bitmask
 * to those in the update parameter.
 *
 * @param update source of the values to be copied.
 */
void StyleDetails::updateUnmasked(const StyleDetails& update)
{
	if((values & edvFontName) == 0)
		FontName = update.FontName;

	if((values & edvFontSize) == 0)
		FontSize = update.FontSize;

	if((values & edvForeColor) == 0)
		ForeColor = update.ForeColor;

	if((values & edvBackColor) == 0)
		BackColor = update.BackColor;

	if((values & edvBold) == 0)
		Bold = update.Bold;

	if((values & edvItalic) == 0)
		Italic = update.Italic;

	if((values & edvUnderline) == 0)
		Underline = update.Underline;

	if((values & edvEOLFilled) == 0)
		EOLFilled = update.EOLFilled;

	if((values & edvClass) == 0)
		classname = update.classname;
}

void StyleDetails::layer(const StyleDetails& other)
{
	if((other.values & edvFontName))
		FontName = other.FontName;

	if((other.values & edvFontSize))
		FontSize = other.FontSize;

	if((other.values & edvForeColor))
		ForeColor = other.ForeColor;

	if((other.values & edvBackColor))
		BackColor = other.BackColor;

	if((other.values & edvBold))
		Bold = other.Bold;

	if((other.values & edvItalic))
		Italic = other.Italic;

	if((other.values & edvUnderline))
		Underline = other.Underline;

	if((other.values & edvEOLFilled))
		EOLFilled = other.EOLFilled;

	if((other.values & edvClass))
		classname = other.classname;

	Hotspot |= other.Hotspot;
}

/////////////////////////////////////////////////////////////////////////////////////
// FullStyleDetails

FullStyleDetails::FullStyleDetails(int key)
{
	m_key = key;

	Style = NULL;
	CustomStyle = NULL;
}

FullStyleDetails::FullStyleDetails(const FullStyleDetails& copy)
{
	m_key = copy.GetKey();
	
	// These all point to instances managed elsewhere:
	Class = copy.Class;
	GroupClass = copy.GroupClass;

	// These are owned by FullStyleDetails:
	if(copy.Style)
		Style = new StyleDetails(*copy.Style);
	else
		Style = NULL;
	
	if(copy.CustomStyle)
		CustomStyle = new StyleDetails(*copy.CustomStyle);
	else
		CustomStyle = NULL;
}

FullStyleDetails::~FullStyleDetails()
{
	if(Style)
	{
		delete Style;
		Style = NULL;
	}
	if(CustomStyle)
	{
		delete CustomStyle;
		CustomStyle = NULL;
	}
}

void FullStyleDetails::Combine(const StyleDetails* defStyle, StyleDetails& into) const
{
	CombineNoCustom(defStyle, into);

	// Now we customise the style with user choices
	if(CustomStyle)
		into.layer(*CustomStyle);

	// Reset values...
	into.values = 0;
}

void FullStyleDetails::CombineNoCustom(const StyleDetails* defStyle, StyleDetails& into) const
{
	// First we build from any style class or custom style class
	// If there is none, we build from the default style.
	if(Class.get())
	{
		// Catch the case where for some reason the usersettings.xml file
		// has a style class that doesn't exist in master.scheme - shouldn't be
		// possible!
		if(Class->Style == NULL)
		{
			UNEXPECTED(_T("Found a style class reference with a custom class but no original class"));
			return;
		}

		Class->Combine(defStyle, into);
	}
	else if(GroupClass.get())
	{
		GroupClass->Combine(defStyle, into);
	}
	else if(defStyle)
		into = *defStyle;
	
	// Now we layer on the actual style settings
	into.layer(*Style);

	// Reset values...
	into.values = 0;
	into.name = Style->name;
}

int FullStyleDetails::GetKey() const
{
	return m_key;
}

void FullStyleDetails::Reset()
{
	if(CustomStyle)
	{
		delete CustomStyle;
		CustomStyle = NULL;
	}
}

void FullStyleDetails::CheckCustomisation(const StyleDetails* defStyle, const StyleDetails& user)
{
	StyleDetails baseStyle;
	CombineNoCustom(defStyle, baseStyle);

	if(user != baseStyle)
	{
		/* The style the user has configured and the original definition version
			do not match. We need to store the new style in the custom style
			store. */

		if( CustomStyle )
		{
			*CustomStyle = user;
		}
		else
		{
			CustomStyle = new StyleDetails( user );
		}
		
		CustomStyle->Key = GetKey();
		CustomStyle->compareTo( baseStyle );
	}
	else
	{
		/* If we have set the style to be like the original, then
			we can safely remove any custom styles. */
		Reset();
	}
}

/////////////////////////////////////////////////////////////////////////////////////
// CustomKeywordSet

CustomKeywordSet::CustomKeywordSet()
{
	key = 0;
	pWords = 0;
	pName = 0;
	pNext = 0;
}

CustomKeywordSet::CustomKeywordSet(const CustomKeywordSet& copy)
{
	pNext = NULL;
	pWords = NULL;

	key = copy.key;
	
	pName = new TCHAR[_tcslen(copy.pName)+1];
	_tcscpy(pName, copy.pName);
}

CustomKeywordSet::~CustomKeywordSet()
{
	if(pWords)
		delete [] pWords;
	if(pName)
		delete [] pName;
}

/////////////////////////////////////////////////////////////////////////////////////
// CustomKeywordHolder

CustomKeywordHolder::CustomKeywordHolder()
{
	pKeywordSets = NULL;
	pLast = NULL;
}

CustomKeywordHolder::~CustomKeywordHolder()
{
	CustomKeywordSet* pSet = pKeywordSets;
	CustomKeywordSet* pDel;
	while(pSet)
	{
		pDel = pSet;
		pSet = pSet->pNext;
		delete pDel;
	}

	pKeywordSets = NULL;
}

void CustomKeywordHolder::AddKeywordSet(CustomKeywordSet* pSet)
{
	if(pLast)
	{
		pLast->pNext = pSet;
		pLast = pSet;
	}
	else
	{
		pKeywordSets = pLast = pSet;
	}
	pLast->pNext = NULL;
}

void CustomKeywordHolder::DeleteKeywordSet(CustomKeywordSet* pSet)
{
	if(!pSet)
		return;

	CustomKeywordSet* pPrev = NULL;

	if(pSet == pKeywordSets)
	{
		pKeywordSets = pSet->pNext;
	}
	else
	{
		// Not the first item, something must point to it.
		CustomKeywordSet* pS = pKeywordSets;
		while(pS)
		{
			if(pS->pNext == pSet)
			{
				pPrev = pS;
				pS->pNext = pSet->pNext;
				break;
			}

			pS = pS->pNext;
		}
	}

	if(pSet == pLast)
		pLast = pPrev;

	delete pSet;
}

CustomKeywordSet* CustomKeywordHolder::FindKeywordSet(int key)
{
	CustomKeywordSet* pSet = pKeywordSets;
	while(pSet)
	{
		if(pSet->key == key)
			break;
		pSet = pSet->pNext;
	}
	return pSet;
}

CustomKeywordSet* CustomKeywordHolder::GetFirstKeywordSet() const
{
	return pKeywordSets;
}

/////////////////////////////////////////////////////////////////////////////////////
// SchemeDetails

SchemeDetails::SchemeDetails(LPCSTR name)
{
	Name = name;
	Flags = 0;
	CustomFlags = 0;
	CustomFlagFlags = 0;
	CustomTabWidth = 0;
}

SchemeDetails::~SchemeDetails()
{
}

StylePtr SchemeDetails::GetStyle(int key)
{
	StylePtr p = ::GetStyle(Styles, key);
	if(!p.get())
	{
		p = ::GetStyle(m_customStyles, key);
		if(p.get())
		{
			m_customStyles.remove(p);
		}
		else
		{
			p.reset( new FullStyleDetails(key) );
		}
		Styles.push_back(p);
	}
	return p;
}

StylePtr SchemeDetails::GetCustomStyle(int key)
{
	return ::GetStyle(m_customStyles, key);
}

void SchemeDetails::PreLoadCustomisedStyle(StylePtr& ptr)
{
	m_customStyles.push_back(ptr);
}

bool SchemeDetails::IsCustomised() const
{
	CustomKeywordSet* pKeywordSet = CustomKeywords.GetFirstKeywordSet();

	bool customStyle(false);

	for(StylePtrList::const_iterator i = Styles.begin(); i != Styles.end(); ++i)
	{
		if( (*i)->CustomStyle )
		{
			customStyle = true;
			break;
		}
	}

	return	customStyle || (pKeywordSet != NULL) || CustomColours.HasColours() || (CustomFlags != 0);
}

bool SchemeDetails::IsInternal() const
{
	return (Flags & schInternal) != 0;
}

void SchemeDetails::ResetAll()
{
	for(StylePtrList::iterator i = Styles.begin(); i != Styles.end(); ++i)
	{
		(*i)->Reset();
	}
}

void SchemeDetails::BeginStyleGroup(LPCTSTR name, LPCTSTR description, LPCTSTR classname)
{
	// Add a dummy style to mark the start of the group.
	StylePtr pDummy( new FullStyleDetails(-1) );
	pDummy->Style = new StyleDetails;
	pDummy->Style->values = edvGroupStart;
	Styles.push_back(pDummy);
	
	// Add information about the group for sending later.
	GroupDetails_t gd;
	gd.name = name;
	if(description)
		gd.description = description;
	if(classname)
		gd.classname = classname;
	GroupDetails.push_back(gd);
}

void SchemeDetails::EndStyleGroup()
{
	StylePtr pStyle(new FullStyleDetails(-1));
	
	pStyle->Style = new StyleDetails;
	pStyle->Style->values = edvGroupEnd;
	Styles.push_back(pStyle);
}

/////////////////////////////////////////////////////////////////////////////////////
// StylesList

StylesList::~StylesList()
{
	ClearStyles();
}

/**
 * Orphan a StyleDetails object into the list.
 */
void StylesList::AddStyle(StyleDetails* pStyle)
{
	m_Styles.push_back(pStyle);
}

/**
 * Removes pStyle from the list, also @see DetachStyle
 */
void StylesList::RemoveStyle(StyleDetails* pStyle)
{
	m_Styles.remove(pStyle);
}

/**
 * Removes any style from the list with the style number "key".
 */
StyleDetails* StylesList::RemoveStyle(int key)
{
	StyleDetails* pS = GetStyle(key);
	if(pS)
		RemoveStyle(pS);
	return pS;
}

/**
 * Deletes pStyle from the list.
 */
void StylesList::DeleteStyle(StyleDetails* pStyle)
{
	RemoveStyle(pStyle);
	delete pStyle;
}

/**
 * Deletes a style with the style number "key" from the list.
 */
void StylesList::DeleteStyle(int key)
{
	StyleDetails* pS = RemoveStyle(key);
	if(pS)
		delete pS;
}

/**
 * Deletes all stored styles.
 */
void StylesList::DeleteAllStyles()
{
	ClearStyles();
}

/**
 * Find a StyleDetails given its style number (key)
 */
StyleDetails* StylesList::GetStyle(int key)
{
	for(StylesList::SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		if((*i)->Key == key)
			return *i;
	}
	return NULL;
}

StylesList::SL_CIT StylesList::StylesBegin()
{
	return m_Styles.begin();
}

StylesList::SL_CIT StylesList::StylesEnd()
{
	return m_Styles.end();
}

size_t StylesList::StylesCount() const
{
	return m_Styles.size();
}

void StylesList::ClearStyles()
{
	for(StylesList::SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		delete (*i);
	}
	m_Styles.clear();
}