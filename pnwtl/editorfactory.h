/**
 * @file editorfactory.h
 * @brief Create Editors
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef editorfactory_h__included
#define editorfactory_h__included

class CChildFrame;
class CommandDispatch;
class AutoCompleteManager;
namespace TextClips { class TextClipsManager; }

/**
 * Factory for editor windows
 */
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
	boost::shared_ptr<AutoCompleteManager> m_AutoComplete;
	HWND						m_hWndMDIClient;
};

#endif //#ifndef editorfactory_h__included