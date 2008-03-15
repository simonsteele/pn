#ifndef editorfactory_h__included
#define editorfactory_h__included

class CChildFrame;
class CommandDispatch;
namespace TextClips { class TextClipsManager; }

class EditorFactory
{
public:
	EditorFactory(CommandDispatch* pCommandDispatch, TextClips::TextClipsManager* pClipManager, HWND hWndMDIClient);
	void SetMdiClient(HWND mdiClient);

	CChildFrame* FromFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding, bool& bOpened);
	CChildFrame* WithScheme(Scheme* pScheme);
	CChildFrame* Default();

private:
	CChildFrame* createChild(DocumentPtr& pD);
	void notifyChild(DocumentPtr& pD);
	
	CommandDispatch*			m_pCommandDispatch;
	TextClips::TextClipsManager* m_pClipManager;
	HWND						m_hWndMDIClient;
};

#endif //#ifndef editorfactory_h__included