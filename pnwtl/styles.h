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

typedef enum {edvFontName = 0x0001,	edvFontSize = 0x0002, edvForeColor = 0x0004, edvBackColor = 0x0008,
				edvBold = 0x0010, edvItalic = 0x0020, edvUnderline = 0x0040, edvEOLFilled = 0x0080, 
				edvClass = 0x0100} EValuesSet;

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
			values = copy.values;
			classname = copy.classname;
			name = copy.name;
			return *this;
		}

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

		int Key;
		
		string FontName;
		int FontSize;
		COLORREF ForeColor;
		COLORREF BackColor;
		bool Bold;
		bool Italic;
		bool Underline;
		bool EOLFilled;

		string name;
		string classname;
		int values;
};

typedef list<StyleDetails*>	STYLES_LIST;
typedef STYLES_LIST::iterator SL_IT;

struct CustomKeywordSet
{
	int		key;
	bool	bChanged;
	TCHAR*	pWords;
	TCHAR*	pName;
	CustomKeywordSet* pNext;
};

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
				if(pDel->pWords)
					delete [] pDel->pWords;
				if(pDel->pName)
					delete [] pDel->pName;
				delete pDel;
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
		CustomKeywordSet* pKeywordSets;
		CustomKeywordSet* pLast;
};

class CustomisedScheme : public CustomKeywordHolder
{
public:
	~CustomisedScheme()
	{
		for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
		{
			delete (*i);
		}
		m_Styles.clear();
	}

	StyleDetails* FindStyle(int key)
	{
		for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
		{
			if((*i)->Key == key)
				return *i;
		}
		return NULL;
	}

	STYLES_LIST	m_Styles;
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