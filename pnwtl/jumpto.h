/**
 * @file jumpto.h
 * @brief Jump to method stuff...
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef jumpto_h__included
#define jumpto_h__included

#include "jumptointerface.h"
#include "include/plugin.h"
#include <map>

class CChildFrame;
class COutputView;

class IJumpToFindSink
{
	public:
		virtual void OnFound(int count, LPMETHODINFO methodInfo) = 0;
};

class JumpToPlugin : Plugin
{
	public:
		typedef void (__stdcall *FP_CALLBACK)(int dataCount, LPMETHODINFO methodInfo);

		JumpToPlugin(LPCTSTR filename);
		
		virtual bool Valid();

		tstring GetSchemesSupported();
		bool GetMethods(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback, int mask = TAGM_ALL);

	protected:
		typedef bool (__stdcall *LPFnGetMethods)(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback, int mask);
		typedef void (__stdcall *LPFnGetSchemesSupported)(wchar_t* schemesBuffer, int cchBuffer);
		typedef int (__stdcall *LPFnGetCapabilities)();

		LPFnGetMethods pfnGetMethods;
		LPFnGetSchemesSupported pfnGetSchemes;
		LPFnGetCapabilities pfnGetCaps;
};

typedef std::map<tstring, JumpToPlugin*> HANDLERS_MAP;
typedef std::list<JumpToPlugin*> PLUGINS_LIST;

class JumpToHandler : public Singleton<JumpToHandler, true>
{
	friend class Singleton<JumpToHandler, true>;

	public:
		void DoJumpTo(CChildFrame* pChildFrame, IJumpToFindSink* pNotifySink);
		void LoadHandler(LPCTSTR path, LPCTSTR filename);

	protected:
		JumpToHandler();
		~JumpToHandler();

		static void __stdcall callback(int dataCount, LPMETHODINFO methodInfo);

		HANDLERS_MAP handlers;
		PLUGINS_LIST plugins;

		COutputView* outputWindow;
		IJumpToFindSink* sink;
};

#endif