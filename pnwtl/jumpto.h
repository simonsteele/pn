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

typedef struct tagMethodInfo
{
	int			type;			// i.e. PNMETHOD_FUNCTION, PNMETHOD_PROCEDURE, PNMETHOD_CLASS etc.
	const char* methodName;		// i.e. Tag
	const char* parentName;		// i.e. class name, package name etc.
	long		lineNumber;		// line number of method in file.
	short		image;			// i.e. PNMETHODIMAGE_FUNCTION, PNMETHODIMAGE_...
} METHODINFO, * LPMETHODINFO;

#include "include/plugin.h"
#include <map>

class JumpToPlugin : Plugin
{
	public:
		typedef void (__stdcall *FP_CALLBACK)(int dataCount, LPMETHODINFO methodInfo);

		JumpToPlugin(LPCTSTR filename);
		
		virtual bool Valid();

		tstring GetSchemesSupported();
		bool GetMethods(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback);

	protected:
		typedef bool (__stdcall *LPFnGetMethods)(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback);
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
		void DoJumpTo(CChildFrame* pChildFrame);
		void LoadHandler(LPCTSTR path, LPCTSTR filename);

	protected:
		JumpToHandler();
		~JumpToHandler();

		static void __stdcall callback(int dataCount, LPMETHODINFO methodInfo);

		HANDLERS_MAP handlers;
		PLUGINS_LIST plugins;

		COutputView* outputWindow;
};

#endif