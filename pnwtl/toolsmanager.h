/**
 * @file toolsmanager.h
 * @brief Manage External Tools
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef toolsmanager_h__included
#define toolsmanager_h__included

/**
 * @brief Class can be used both standalone and as a singleton.
 */
class ToolsManager : 
	public Singleton<ToolsManager, SINGLETON_AUTO_DELETE>, 
	public XMLParseState
{
	public:
		ToolsManager();
		virtual ~ToolsManager();

		SchemeTools* GetGlobalTools();
		SchemeTools* GetGlobalProjectTools();

		SchemeTools* GetToolsFor(LPCTSTR scheme);
		ProjectTools* GetToolsForProject(LPCTSTR id);
		//int GetMenuFor(LPCTSTR scheme, CSMenuHandle& menu, int iInsertBefore);

		void ReLoad(CommandDispatch* pDispatch = NULL);
		void Save();

		int UpdateToolsMenu(CSMenuHandle& tools, CommandDispatch* dispatcher, int iFirstToolCmd, int iDummyID, LPCSTR schemename, LPCTSTR projectId);

		const ToolSource* GetDefaultToolStore();

	protected:
		void Clear(CommandDispatch* pDispatch = NULL);

		int BuildMenu(TOOLDEFS_LIST& list, CommandDispatch* dispatcher, CSMenuHandle& menu, int iInsertBefore, int iCommand = TOOLS_RUNTOOL);

		// Scheme & Tool Creation
		void processScheme(XMLAttributes& atts);
		void processGlobal(XMLAttributes& atts);
		void processProject(XMLAttributes& atts);
		void processTool(XMLAttributes& atts);
		void processAllProjects(XMLAttributes& atts);

		// XML Parsing
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){}

	protected:
		typedef std::map<tstring, SchemeTools*> SCHEMETOOLS_MAP;
		typedef std::list<ToolSource*> SOURCES_LIST;

		SchemeTools* find(LPCTSTR id, SCHEMETOOLS_MAP& col);

		ToolSource			m_DefaultToolsSource;
		SchemeTools*		m_pCur;
		GlobalTools*		m_pGlobalTools;
		GlobalProjectTools*	m_pGlobalProjectTools;
		ToolSource*			m_pCurSource;
		SCHEMETOOLS_MAP		m_toolSets;
		SCHEMETOOLS_MAP		m_projectTools;
		SOURCES_LIST		m_toolSources;
};

#endif