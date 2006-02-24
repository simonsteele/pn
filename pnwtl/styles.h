/**
 * @file Styles.h
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef styles_h__included
#define styles_h__included

////////////////////////////////////////////////////////////
// Useful Functions
////////////////////////////////////////////////////////////

int chartoval(TCHAR inp);
COLORREF PNStringToColor(LPCTSTR input);
bool PNStringToBool(LPCTSTR input);
tstring NormaliseKeywords(tstring& in);

////////////////////////////////////////////////////////////
// EditorColours
////////////////////////////////////////////////////////////

class EditorColours
{
public:
	typedef enum {
		ecSelFore	= 0x01,
		ecSelBack	= 0x02,
		ecCaret		= 0x04,
		ecIndentG	= 0x08,
	} Colours;

	EditorColours();

	const EditorColours& operator = (const EditorColours& copy);

	void SetColour(Colours colour, COLORREF setColour);

	bool HasColour(Colours colour) const;

	bool HasColours() const;

	/**
	 * @return True if colour configured, false otherwise
	 */
	bool GetColour(Colours colour, COLORREF& theColour) const;

	void SetFromXml(XMLAttributes& atts);

	void SendColours(CScintilla* pSc) const;

	/**
	 * This applies any colours in other to this object.
	 */
	void Combine(const EditorColours* other);

	void Clear();

protected:
	COLORREF	crSelFore;
	COLORREF	crSelBack;
	COLORREF	crCaret;
	COLORREF	crIG;
	SHORT		values;
};

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
		StyleDetails();
		StyleDetails(const StyleDetails& copy);
		StyleDetails& operator = (const StyleDetails& copy);

		/**
		 * This method should only compare parts of the style that
		 * users can change - others need not be compared.
		 */
		bool operator == (const StyleDetails& compare) const;

		bool operator != (const StyleDetails& compare) const;;

		/**
		 * This function sets the "values" bit mask with
		 * all the values that are different from those in compare.
		 * This only takes into account values that the user may
		 * change - not hotspots for example.
		 */
		void compareTo(const StyleDetails& compare);

		/**
		 * This sets any values that are not included in the values bitmask
		 * to those in the update parameter.
		 *
		 * @param update source of the values to be copied.
		 */
		void updateUnmasked(const StyleDetails& update);

		void layer(const StyleDetails& other);

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

class FullStyleDetails;
typedef ::boost::shared_ptr<FullStyleDetails> StylePtr;

class FullStyleDetails
{
public:
	FullStyleDetails(int key);
	FullStyleDetails(const FullStyleDetails& copy);
	~FullStyleDetails();

	int GetKey() const;

	/**
	 * Combine all the details we know about into the definitive final
	 * style.
	 */
	void Combine(const StyleDetails* defStyle, StyleDetails& into) const;

	void CombineNoCustom(const StyleDetails* defStyle, StyleDetails& into) const;

	// Pointed at StyleDetails Instances:
	//StyleDetails* Class;		// Style Class
	//StyleDetails* CustomClass;	// Customised Style Class
	StylePtr	  Class;
	StylePtr	  GroupClass;	// Group Style Class (grouped styles)
	
	// Owned StyleDetails Instances:
	StyleDetails* Style;		// *The* Style (as in the .scheme file)
	StyleDetails* CustomStyle;	// Customised Style

private:
	int m_key;
};

typedef list<StyleDetails*>	STYLES_LIST;
typedef STYLES_LIST::iterator SL_IT;
typedef STYLES_LIST::const_iterator SL_CIT;

typedef std::list<StylePtr> StylePtrList;
typedef std::map<tstring, StylePtr> StylePtrMap;

static StylePtr GetStyle(StylePtrList& lst, int key)
{
	for(StylePtrList::const_iterator i = lst.begin(); i != lst.end(); ++i)
	{
		if((*i)->GetKey() == key)
			return (*i);
	}

	return StylePtr();
}

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

	size_t StylesCount() const
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

		CustomKeywordSet* GetFirstKeywordSet() const
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

struct GroupDetails_t
{
	tstring name;
	tstring description;
	tstring classname;
};

typedef std::list<GroupDetails_t> GroupDetailsList;

/*typedef struct tagStyleGroup
{
	tstring Name;
	StylePtrList Styles;
} StyleGroup;

typedef std::list<StyleGroup> StyleGroupList;*/

/**
 * Flat storage of a tree of grouped styles.
 */
class SchemeDetails
{
public:
	SchemeDetails(LPCTSTR name);
	
	StylePtr GetStyle(int key);

	bool IsCustomised() const;
	bool IsInternal() const;

	void ResetAll();

	void BeginStyleGroup(LPCTSTR name, LPCTSTR description, LPCTSTR classname);
	void EndStyleGroup();

	tstring				Name;
	tstring				Title;
	StylePtrList		Styles;
	
	GroupDetailsList	GroupDetails;

	EditorColours		Colours;

	EditorColours		CustomColours;
	
	CustomKeywordHolder Keywords;

	CustomKeywordHolder CustomKeywords;

	int                 Flags;

	int					CustomFlags;
	int					CustomFlagFlags;
	int					CustomTabWidth;
};

typedef std::list<SchemeDetails*> SchemeDetailsList;
typedef std::map<tstring, SchemeDetails*> SchemeDetailsMap;

typedef enum {ebvLexer = 0x01} EBaseSet;

class BaseScheme : public SchemeDetails
{
	public:
		BaseScheme(LPCTSTR name) : SchemeDetails(name)
		{
			lexer		= _T("");
			styleBits	= 0;
			valuesSet	= 0;
			flags		= 0;
		}

		int					flags;
		int					valuesSet;
		tstring				lexer;
		int					styleBits;
};

typedef map<tstring, StyleDetails*> STYLEDETAILS_NAMEMAP;
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

		void AddStyle(const tstring& name, StyleDetails* pStyle)
		{
			m_Styles.insert(m_Styles.end(),
				STYLEDETAILS_NAMEMAP::value_type(name, pStyle));
		}

		void AddStyle(LPCTSTR name, StyleDetails* pStyle)
		{
			AddStyle(tstring(name), pStyle);
		}

		void DeleteStyle(const tstring& name)
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
			DeleteStyle(tstring(name));
		}

		StyleDetails* GetStyle(const tstring& name)
		{
			SDNM_IT i = m_Styles.find(name);
			if(i != m_Styles.end())
				return (*i).second;
			else
				return NULL;
		}

		StyleDetails* GetStyle(LPCTSTR name)
		{
			return GetStyle(tstring(name));
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