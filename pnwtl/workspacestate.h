#ifndef workspacestate_h__included_B08ABC4F_1F76_44e1_9602_1F4E89FADCF6
#define workspacestate_h__included_B08ABC4F_1F76_44e1_9602_1F4E89FADCF6

class WorkspaceState : XMLParseState
{
	public:
		void Load(LPCTSTR path = NULL);
		void Save(LPCTSTR path = NULL);

// Internal
	protected:
		void load(LPCTSTR filename);
		void save(LPCTSTR filename);
		void getDefaultPath(tstring& str) const;

// XMLParseState
	protected:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){};

// XML Bit Handlers
	protected:
		void handleProjectGroup(XMLAttributes& atts);
		void handleProject(XMLAttributes& atts);
		void handleFile(XMLAttributes& atts);

	protected:
		int m_parseState;
};

#endif //#ifndef #ifndef workspacestate_h__included_B08ABC4F_1F76_44e1_9602_1F4E89FADCF6