// pn.h

#include "pntypes.h"
#include "schemes.h"
#include "optionsmanager.h"

class CPNAppState
{
	public:
		CPNAppState();
		~CPNAppState();

		CSchemeManager& GetSchemes(){return m_Schemes;}
		COptionsManager& GetOptionsManager(){return m_Options;}

		SFindOptions*		GetFindOptions(){return &m_FindOptions;}
		SReplaceOptions*	GetReplaceOptions(){return &m_ReplaceOptions;}
	protected:

		CSchemeManager	m_Schemes;
		COptionsManager	m_Options;

		SFindOptions	m_FindOptions;
		SReplaceOptions	m_ReplaceOptions;
};

HWND GetCurrentEditor(CWindow* pMDIFrameWnd);
