/**
 * @file Styles.h
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef styles_h__included
#define styles_h__included

#include <string>
#include <list>
#include <map>

using std::string;
using std::list;
using std::map;

/* The edvGroupStart value is a misappropriation of the values field. It allows
 * a StyleDetails object to be stored in a list as a marker for the start of
 * a group. The value should never be used by anything but the schemeparser.
 * The edvGroupEnd value is its counterpart. */
typedef enum {edvFontName = 0x0001,	edvFontSize = 0x0002, edvForeColor = 0x0004, edvBackColor = 0x0008,
				edvBold = 0x0010, edvItalic = 0x0020, edvUnderline = 0x0040, edvEOLFilled = 0x0080, 
				edvClass = 0x0100, edvGroupStart = 0x0200, edvGroupEnd = 0x0400} EValuesSet;

/** 
 * @brief represents a single style as sent to Scintilla.
 */
class StyleDetails
{
	public:
		StyleDetails()
		{
			Key = 0;
			FontName = "Courier New";
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

		StyleDetails(const StyleDetails& copy)
		{
			*this = copy;
		}

		StyleDetails& operator = (const StyleDetails& copy)
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
			classname = copy.classname;
			name = copy.name;
			
			return *this;
		}

		/**
		 * This method should only compare parts of the style that
		 * users can change - others need not be compared.
		 */
		bool operator == (const StyleDetails& compare)
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

		bool operator != (const StyleDetails& compare)
		{
			return !(*this == compare);
		}

		/**
		 * This function sets the "values" bit mask with
		 * all the values that are different from those in compare.
		 * This only takes into account values that the user may
		 * change - not hotspots for example.
		 */
		void compareTo(StyleDetails& compare)
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
		void updateUnmasked(StyleDetails& update)
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

		int Key;
		
		string FontName;
		int FontSize;
		COLORREF ForeColor;
		COLORREF BackColor;
		bool Bold;
		bool Italic;
		bool Underline;
		bool EOLFilled;
		bool Hotspot;

		string name;
		string classname;
		int values;
};

typedef list<StyleDetails*>	STYLES_LIST;
typedef STYLES_LIST::iterator SL_IT;
typedef STYLES_LIST::const_iterator SL_CIT;

/**
 * Simple wrapper class for a list of StyleDetails objects. The class
 * is designed to be aggregated into others, so the method names
 * all include "Style" to make their purpose clear.
 */
class StylesList
{
public:
	~StylesList()
	{
		ClearStyles();
	}

	/**
	 * Orphan a StyleDetails object into the list.
	 */
	void AddStyle(StyleDetails* pStyle)
	{
		m_Styles.push_back(pStyle);
	}

	/**
	 * Removes pStyle from the list, also @see DetachStyle
	 */
	void RemoveStyle(StyleDetails* pStyle)
	{
		m_Styles.remove(pStyle);
	}

	/**
	 * Removes any style from the list with the style number "key".
	 */
	StyleDetails* RemoveStyle(int key)
	{
		StyleDetails* pS = GetStyle(key);
		if(pS)
			RemoveStyle(pS);
		return pS;
	}

	/**
	 * Deletes pStyle from the list.
	 */
	void DeleteStyle(StyleDetails* pStyle)
	{
		RemoveStyle(pStyle);
		delete pStyle;
	}

	/**
	 * Deletes a style with the style number "key" from the list.
	 */
	void DeleteStyle(int key)
	{
		StyleDetails* pS = RemoveStyle(key);
		if(pS)
			delete pS;
	}

	/**
	 * Deletes all stored styles.
	 */
	void DeleteAllStyles()
	{
		ClearStyles();
	}

	/**
	 * Find a StyleDetails given its style number (key)
	 */
	StyleDetails* GetStyle(int key)
	{
		for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
		{
			if((*i)->Key == key)
				return *i;
		}
		return NULL;
	}

	SL_CIT StylesBegin()
	{
		return m_Styles.begin();
	}

	inline SL_CIT StylesEnd()
	{
		return m_Styles.end();
	}

	size_t StylesCount()
	{
		return m_Styles.size();
	}

	protected:
		void ClearStyles()
		{
			for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
			{
				delete (*i);
			}
			m_Styles.clear();
		}

		STYLES_LIST	m_Styles;
};

/**
 * @brief Represents a single set of keywords.
 */
class CustomKeywordSet
{
	public:
		CustomKeywordSet()
		{
			key = 0;
			pWords = 0;
			pName = 0;
			pNext = 0;
		}
		
		CustomKeywordSet(const CustomKeywordSet& copy)
		{
			pNext = NULL;
			pWords = NULL;

			key = copy.key;
			
			pName = new TCHAR[_tcslen(copy.pName)+1];
			_tcscpy(pName, copy.pName);
			
			// We rarely construct to copy the words...
			//pWords = new TCHAR[_tcslen(copy->pWords)+1];
			//_tcscpy(pWords, copy->pWords);
		}

	public:
		int		key;
		TCHAR*	pWords;
		TCHAR*	pName;
		CustomKeywordSet* pNext;
};

/**
 * @brief Collection class for a CustomKeywordSet instances.
 */
class CustomKeywordHolder
{
	public:
		CustomKeywordHolder()
		{
			pKeywordSets = NULL;
			pLast = NULL;
		}

		~CustomKeywordHolder()
		{
			CustomKeywordSet* pSet = pKeywordSets;
			CustomKeywordSet* pDel;
			while(pSet)
			{
				pDel = pSet;
				pSet = pSet->pNext;
				InternalDeleteSet(pDel);
			}

			pKeywordSets = NULL;
		}

		void AddKeywordSet(CustomKeywordSet* pSet)
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

		void DeleteKeywordSet(CustomKeywordSet* pSet)
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

			InternalDeleteSet(pSet);
		}

		CustomKeywordSet* FindKeywordSet(int key)
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

		CustomKeywordSet* GetFirstKeywordSet()
		{
			return pKeywordSets;
		}

	protected:
		inline void InternalDeleteSet(CustomKeywordSet* pDel)
		{
			if(pDel->pWords)
				delete [] pDel->pWords;
			if(pDel->pName)
				delete [] pDel->pName;
			delete pDel;
		}

		CustomKeywordSet* pKeywordSets;
		CustomKeywordSet* pLast;
};

/**
 * Aggregating class for keywords and styles - represents all the settings
 * for a customised scheme when loaded from the user settings file.
 */
class CustomisedScheme : public CustomKeywordHolder, public StylesList
{

};


typedef enum {ebvLexer = 0x01} EBaseSet;

class BaseScheme : public CustomisedScheme
{
	public:
		BaseScheme()
		{
			lexer		= _T("");
			styleBits	= 0;
			valuesSet	= 0;
			flags		= 0;
		}

		tstring lexer;
		int		styleBits;
		int		flags;

		int		valuesSet;
};

typedef map<CString, CustomisedScheme*> CUSTOMISED_NAMEMAP;
typedef CUSTOMISED_NAMEMAP::iterator CNM_IT;
typedef map<CString, StyleDetails*> STYLEDETAILS_NAMEMAP;
typedef STYLEDETAILS_NAMEMAP::iterator SDNM_IT;

/**
 * @class StylesMap
 * @brief Simple map<> wrapper to reduce code when using the style maps.
 */
class StylesMap
{
	public:
		~StylesMap()
		{
			Clear();
		}

		void Clear()
		{
			for(SDNM_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
			{
				delete (*i).second;
			}
			m_Styles.clear();
		}

		void AddStyle(const CString& name, StyleDetails* pStyle)
		{
			m_Styles.insert(m_Styles.end(),
				STYLEDETAILS_NAMEMAP::value_type(name, pStyle));
		}

		// at the moment, this just uses a CString to call the above fn.
		void AddStyle(LPCTSTR name, StyleDetails* pStyle)
		{
			AddStyle(CString(name), pStyle);
		}

		void DeleteStyle(const CString& name)
		{
			SDNM_IT i = m_Styles.find(name);
			if(i != m_Styles.end())
			{
				delete (*i).second;
				m_Styles.erase(i);
			}
		}

		void DeleteStyle(LPCTSTR name)
		{
			DeleteStyle(CString(name));
		}

		StyleDetails* GetStyle(const CString& name)
		{
			SDNM_IT i = m_Styles.find(name);
			if(i != m_Styles.end())
				return (*i).second;
			else
				return NULL;
		}

		// at the moment, this just uses a CString to call the above fn.
		StyleDetails* GetStyle(LPCTSTR name)
		{
			return GetStyle(CString(name));
		}

		STYLEDETAILS_NAMEMAP& GetMap()
		{
			return m_Styles;
		}

		int GetCount()
		{
			return m_Styles.size();
		}

	protected:
		STYLEDETAILS_NAMEMAP	m_Styles;
};

#endif // styles_h__included