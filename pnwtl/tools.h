/**
 * @file tools.h
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef tools_h__included
#define tools_h__included

typedef std::list<SToolDefinition*> TOOLDEFS_LIST;

/**
 * @brief Collection class representing tools associated with one scheme
 */
class SchemeTools
{
	public:
		SchemeTools(LPCTSTR schemename);
		~SchemeTools();

		TOOLDEFS_LIST&	GetTools();
		int				GetMenu(CSMenuHandle& menu, int iInsertAfter, int iCommand = TOOLS_RUNTOOL);

		void			Add(SToolDefinition* pDef);
		void			Delete(SToolDefinition* pDef);

		void			WriteDefinition(ofstream& stream);

		void			ReleaseMenuResources();

	protected:
		void			BuildMenu(int iCommand);
		
		TOOLDEFS_LIST	m_Tools;
		tstring			m_Scheme;
		CSPopupMenu		m_Menu;
};

/**
 * @brief Class can be used both standalone and as a singleton.
 */
class SchemeToolsManager : public Singleton<SchemeToolsManager>, public XMLParseState
{
	public:
		SchemeToolsManager();
		~SchemeToolsManager();

		SchemeTools* GetToolsFor(LPCTSTR scheme);
		int GetMenuFor(LPCTSTR scheme, CSMenuHandle& menu, int iInsertBefore);

		void ReLoad(bool bWantMenuResources = false);
		void Save();

	protected:
		void Clear(bool bWantMenuResources = false);

		// Scheme & Tool Creation
		void processScheme(XMLAttributes& atts);
		void processTool(XMLAttributes& atts);

		// XML Parsing
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){}

	protected:
		typedef std::map<tstring, SchemeTools*> SCHEMETOOLS_MAP;

		SchemeTools*	m_pCur;
		SCHEMETOOLS_MAP m_toolSets;
};

class CToolCommandString : public CustomFormatStringBuilder<CToolCommandString>
{
	public:
		void OnFormatChar(TCHAR thechar)
		{
			switch(thechar)
			{
				case _T('f'):
					m_string += pChild->GetFileName(FN_FILE);
					break;

				case _T('d'):
					m_string += pChild->GetFileName(FN_PATH);
					break;

				case _T('n'):
					m_string += pChild->GetFileName(FN_FILEPART);
					break;

				case _T('l'):
					_itoa(pChild->GetPosition(EP_LINE), itosbuf, 10);
					m_string += itosbuf;
					break;

				case _T('c'):
					_itoa(pChild->GetPosition(EP_COL), itosbuf, 10);
					m_string += itosbuf;
					break;
			}		
		}

		CChildFrame* pChild;

	protected:
		TCHAR itosbuf[100];
};

void ExecuteTool(CChildFrame* pActiveChild, SToolDefinition* pDef);

#endif