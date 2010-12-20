/**
 * @file xmlfileautocomplete.cpp
 * @brief XML-file based autocomplete
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "xmlfileautocomplete.h"

namespace Impl
{
	/**
	 * Single method/item definition, one or more for each tag
	 */
	class Definition
	{
	public:
		Definition(const char* returnval) : Return(returnval)
		{
		}

		Definition(const char* returnval, const char* params) : Return(returnval), Params(params)
		{
		}

		std::string Return;
		std::string Params;
	};

	/**
	 * Tag is one named autocomplete item, it can have many definitions
	 */
	class Tag
	{
	public:
		std::string Name;
		std::vector<Definition> Defs;
	};
}

using namespace Impl;

#define MATCH(ename) \
	(_tcscmp(name, _T(ename)) == 0)

#define IN_STATE(state) \
	(m_parseState == state)

#define STATE(state) \
	m_parseState = state

/**
 * Parse state handler for reading NPP xml files.
 */
class ParseHandler : public XMLParseState
{
public:
	ParseHandler(std::vector<Tag>& tags) : m_tags(tags), m_parseState(PHS_Start), m_current(NULL), m_currentDef(NULL) {}

	void startElement(XML_CSTR name, const XMLAttributes& atts)
	{
		if IN_STATE(PHS_Start)
		{
			if MATCH("AutoComplete")
			{
				STATE(PHS_AutoComplete);
			}
		}
		else if IN_STATE(PHS_AutoComplete)
		{
			if MATCH("KeyWord")
			{
				STATE(PHS_KeyWord);
				handleKeyword(atts);
			}
		}
		else if IN_STATE(PHS_KeyWord)
		{
			if MATCH("Overload")
			{
				STATE(PHS_Overload);
				handleOverload(atts);
			}
		}
		else if IN_STATE(PHS_Overload)
		{
			if MATCH("Param")
			{
				handleParam(atts);
			}
		}
		
	}

	void endElement(LPCTSTR name)
	{
		if IN_STATE(PHS_Overload)
		{
			if MATCH("Overload")
			{
				STATE(PHS_KeyWord);
			}
		}
		else if IN_STATE(PHS_KeyWord)
		{
			if MATCH("KeyWord")
			{
				STATE(PHS_AutoComplete);
			}
		}
		else if IN_STATE(PHS_AutoComplete)
		{
			if MATCH("AutoComplete")
			{
				STATE(PHS_Start);
			}
		}
	}

	void characterData(LPCTSTR data, int len)
	{
	}

private:
	void handleKeyword(const XMLAttributes& atts)
	{
		LPCTSTR name = atts.getValue(_T("name"));
		if (name != NULL && name[0] != NULL)
		{
			CT2CA tagName(name);
			Tag tag;
			tag.Name = tagName;
			m_tags.push_back(tag);
			m_current = &m_tags.back();
		}
	}

	void handleOverload(const XMLAttributes& atts)
	{
		LPCTSTR ret = atts.getValue(_T("retVal"));
		if (ret != NULL && ret[0] != NULL)
		{
			CT2CA retConv(ret);
			Definition def(retConv);
			m_current->Defs.push_back(def);
			m_currentDef = &m_current->Defs.back();
		}
		else
		{
			m_currentDef = NULL;
		}
	}

	void handleParam(const XMLAttributes& atts)
	{
		if (m_currentDef == NULL)
		{
			return;
		}

		LPCTSTR name = atts.getValue(_T("name"));
		if (name != NULL && name[0] != NULL)
		{
			if (m_currentDef->Params.size() > 0)
			{
				m_currentDef->Params += ", ";
			}

			CT2CA nameconv(name);
			m_currentDef->Params += nameconv;
		}
	}

	typedef enum {
		PHS_Start,
		PHS_AutoComplete,
		PHS_KeyWord,
		PHS_Overload
	} EState;

	EState m_parseState;
	std::vector<Tag>& m_tags;
	Tag* m_current;
	Definition* m_currentDef;
};

/**
 * Constructor, initializes word lists.
 */
XmlFileAutocompleteProvider::XmlFileAutocompleteProvider(LPCTSTR apiFile)
{
	try
	{
#ifdef _DEBUG
		OpTimer timer;
#endif

		XMLParser parser;
		ParseHandler handler(m_tags);
		parser.SetParseState(&handler);
		parser.LoadFile(apiFile);
	}
	catch (XMLParserException& ex)
	{
		CString err;
		err.Format(_T("Error Parsing API XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(ex.GetErrorCode()), ex.GetFileName(), ex.GetLine(), ex.GetColumn());

		g_Context.m_frame->SetStatusText(err);
	}
}

/**
 * Destructor.
 */
XmlFileAutocompleteProvider::~XmlFileAutocompleteProvider()
{
}

// Ignoring Tag and Keyword-based Autocomplete:
void XmlFileAutocompleteProvider::RegisterKeyWords(int set, const char* words) {}
void XmlFileAutocompleteProvider::RegisterTag(const char* tag, const char* name) {}
void XmlFileAutocompleteProvider::ResetTags() {}
void XmlFileAutocompleteProvider::Reset() {}

/**
 * Get the list of words to use
 * Initially a complete naive implementation, this should move to something more like a binary
 * search of the sorted list.
 */
void XmlFileAutocompleteProvider::GetWords(PN::BaseString& words, const char* root, int rootLength, bool includeParameters, char tokenSeparator)
{
	std::vector<Tag>::const_iterator it = m_tags.begin();
	bool startedMatching(false);
	for (; it != m_tags.end(); it++)
	{
		if (strncmp((*it).Name.c_str(), root, rootLength) == 0)
		{
			startedMatching = true;
			if (!words.Empty())
			{
				words += tokenSeparator;
			}

			words += (*it).Name.c_str();
		}
		else if (startedMatching)
		{
			// out of the group of matching strings, return...
			break;
		}
	}
}

// New function to return the correct prototypes for a method
void XmlFileAutocompleteProvider::GetPrototypes(PN::BaseString& prototypes, char tokenSeparator, const char* method, int methodLength)
{
	std::vector<Tag>::const_iterator it = m_tags.begin();
	for (; it != m_tags.end(); it++)
	{
		if ((*it).Name.size() == static_cast<size_t>(methodLength) && strncmp((*it).Name.c_str(), method, methodLength) == 0)
		{
			BOOST_FOREACH(const Definition& def, (*it).Defs)
			{
				if (!prototypes.Empty())
				{
					prototypes += tokenSeparator;
				}

				prototypes += def.Return.c_str();
				prototypes += " ";
				prototypes += (*it).Name.c_str();
				prototypes += "(";
				prototypes += def.Params.c_str();
				prototypes += ")";
			}

			break;
		}
	}
}