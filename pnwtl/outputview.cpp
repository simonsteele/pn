/**
 * @file OutputView.cpp
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "outputview.h"
#include "textview.h"
#include "childfrm.h"
#include "third_party/scintilla/include/scilexer.h"
#include "scaccessor.h"
#include "project.h"

//////////////////////////////////////////////////////////////////////////////
// COutputView
//////////////////////////////////////////////////////////////////////////////

COutputView::COutputView() : 
	Views::View(Views::vtOutput)
{
	m_bCustom = false;
}

COutputView::~COutputView()
{
}

/**
 * Try and find a sensible match for a relative filename
 */
bool COutputView::ExpandMatchedPath(CFileName& fn)
{
	bool bRet = false;

	if(m_basepath.length() != 0)
	{
		fn.Root( m_basepath.c_str() );
		//filename = fn.c_str();
#ifdef _DEBUG
		tstring dbgout = _T("Rooted path to: ");
		dbgout += fn.c_str();
		dbgout += _T("\n");
		::OutputDebugString(dbgout.c_str());
#endif
		bRet = true;
	}
	else
	{
		// There is no base directory, and the filename is relative.
		// Only a couple of options left.
		
		// 1. See if we can open from the project.
		tstring fullname;
		if( LocateInProjects(fn.c_str(), fullname) )
		{
			fn = fullname;
			bRet = true;
		}
		else
		{
			// 2) See if we can open from the current directory.
			// Attach the filename to the current directory so that it's
			// at least a full path.
			TCHAR dirbuf[MAX_PATH+1];
			memset(dirbuf, 0, (MAX_PATH + 1)*sizeof(TCHAR));
			GetCurrentDirectory(MAX_PATH, dirbuf);
			
			if( _tcslen(dirbuf) > 0 )
			{
				CFileName fn2 = fn;
				fn2.Root( dirbuf );	
				
				if(FileExists(fn2.c_str()))
				{
					fn = fn2;
					bRet = true;
				}
			}
		}
	}

	return bRet;
}

/**
 * @brief Uses any regular expression string to try and match an error.
 * @param style - style that the error is displayed in.
 * @param position - position of the character clicked.
 * @param re - Regular Expression object.
 */
bool COutputView::HandleREError(boost::xpressive::sregex& re, int style, int position)
{
	bool bRet = true;

	Scintilla::TextRange tr;
				
	ExtendStyleRange(position, style, &tr);
	std::string buf;
	buf.resize(tr.chrg.cpMax - tr.chrg.cpMin + 1);
	tr.lpstrText = &buf[0];
	GetTextRange(&tr);
	buf.resize(tr.chrg.cpMax - tr.chrg.cpMin);

	boost::xpressive::smatch match;
	if( boost::xpressive::regex_search(buf, match, re) )
	{
		std::string filename;
		std::string linestr;
		std::string colstr;
        
		// Extract the named matches from the RE, noting if there was a line or column.
		bool bFile = safe_get_submatch(match, filename, "f");
		bool bLine = safe_get_submatch(match, linestr, "l");
		bool bCol = safe_get_submatch(match, colstr, "c");

//#ifdef _DEBUG
//			tstring dbgout = _T("Matched file (");
//			dbgout += filename;
//			dbgout += _T(") line (");
//			dbgout += linestr;
//			dbgout += _T(") col (");
//			dbgout += colstr;
//			dbgout += _T(")\n");
//			LOG(dbgout.c_str());
//#endif

		if(bFile)
		{
			//First check if the file exists as is, if it does then we go with that,
			//else we try to resolve it.
			CA2CT filenameConv(filename.c_str());
			CFileName fn(filenameConv);
			fn.Sanitise();

//#ifdef _DEBUG
//			dbgout = _T("After sanitise, filename = ");
//			dbgout += fn.c_str();
//			dbgout += _T("\n");
//			LOG(dbgout.c_str());
//#endif

			if( fn.IsRelativePath() )
			{
				ExpandMatchedPath(fn);
				fn.Sanitise();
			}

			if(FileExists(fn.c_str()))
			{
				// If the file's already open, just switch to it, otherwise open it.
				if( !g_Context.m_frame->CheckAlreadyOpen(fn.c_str(), eSwitch) )
					g_Context.m_frame->Open(fn.c_str());
			}
			else
			{
				tstring msg = _T("Could not locate ");
				msg += filenameConv;
				msg += _T(". If the file exists, see help under \"Output\" to fix this.");
				g_Context.m_frame->SetStatusText(msg.c_str());
				bRet = false;
			}
		}

		if(bRet)
		{
			CChildFrame* pWnd = CChildFrame::FromHandle(GetCurrentEditor());
			CTextView* pView = pWnd->GetTextView();

			if( bLine )
			{
				int line = atoi(linestr.c_str());

				if(pView)
				{
					pView->GotoLine(line-1);
					
					if( bCol )
					{
						long lPos = pView->GetCurrentPos();
						long column = atol(colstr.c_str());
						lPos += column;
						pView->SetCurrentPos(lPos);
					}
				}
			}

			::SetFocus(pView->m_hWnd);
		}
	}
	else
	{
		bRet = false;
	}

	return bRet;
}

/**
 * @brief Try to locate a partial filename in the projects.
 */
bool COutputView::LocateInProjects(LPCTSTR part, tstring& full)
{
	Projects::Workspace* pWs = g_Context.m_frame->GetActiveWorkspace();
	if(pWs)
	{
		// Try the active project first.
		Projects::Project* pActive = pWs->GetActiveProject();
		Projects::File* pFile = pActive->FindRelativeFile(part);

		// If we didn't find it in the active project, look in the rest.
		if(!pFile)
		{
			for(Projects::PL_CIT i = pWs->GetProjects().begin();
				i != pWs->GetProjects().end();
				++i)
			{
				// try anything but the active project again.
				if((*i) != pActive)
				{
					pFile = (*i)->FindRelativeFile(part);
					if(pFile)
						break;
				}
			}
		}

		// if we found one, return the full file name.
		if(pFile)
		{
			full = pFile->GetFileName();
			return true;
		}
	}
	
	return false;
}

/**
 * @brief Calls HandleREError with a pre-built regular expression handler.
 */
void COutputView::HandleCustomError(int style, int position)
{
	HandleREError(*m_pRE, style, position);
}

/**
 * @brief Builds a regular expression object from reDef and then calls HandleREError.
 */
bool COutputView::BuildAndHandleREError(int style, int position, const char* reDef)
{
	try
	{
		boost::xpressive::sregex re = boost::xpressive::sregex::compile(std::string(reDef));
		return HandleREError(re, style, position);
	}
	catch (boost::xpressive::regex_error& ex)
	{
		CA2CT errorMessage(ex.what());
		::MessageBox(m_hWnd, errorMessage, _T("PN2 - Regular Expression Error"), MB_OK);
	}

	return false;
}

/**
 * @brief Parse a simple GCC error string:
 * filename.ext:linenumber: error string/whatever
 */
void COutputView::HandleGCCError(int style, int position)
{
	BuildAndHandleREError(style, position, "(?P<f>.+?):(?P<l>[0-9]+):((?P<c>[0-9]+):)? .*");
}

/**
 * @brief Parse a microsoft message string:
 *      filename.ext(line,col): message
 * Derived from: \s%f\(%l,%c\): 
 *
 * Currently tested with NAnt output.
 */
void COutputView::HandleMSError(int style, int position)
{
	BuildAndHandleREError(style, position, "\\s*(?P<f>.+)\\((?P<l>[0-9]+)(,(?P<c>[0-9]+))?\\)\\s*: ");
}

/**
 * @brief Parse Borland C++ errors, warnings and resource compiler warnings...
 *
 * Also handle lcc-win32 errors.
 *
 * Error E2034 clippert.cpp 207: message...
 * Warning W8070 clippert.cpp 208: message...
 * Error resources.rc 14 18: message (column line:)
 */
void COutputView::HandleBorlandCPPError(int style, int position)
{
	// Explanation of this RE: http://www.pnotepad.org/devlog/archives/000086.html
	
	if( !BuildAndHandleREError(style, position, "(Error|Warning) ((E|W)[0-9]{4} )?(?P<f>.+?) ((?P<c>[0-9]+) )?(?P<l>[0-9]+): .+") )
	{
		// Didn't match borland C++, try lcc-win32:
		BuildAndHandleREError(style, position, "(Error|Warning) (?P<f>.+): (?P<l>[0-9]+) .+");
	}
}

/**
 * @brief Parse perl errors...
 *
 * Perl errors are not anchored to the start of the line, beginning with "at".
 * syntax error at P:\tex\packages\authorindex.pl line 103, near "){"
 * String found where operator expected at P:\tex\packages\authorindex.pl line 110
 * Bareword "AUXFILE" not allowed while "strict subs" in use at P:\tex\packages\authorindex.pl line 100
 *
 * It might be nice in the future to handle the (near "expression") part of 
 * the string - although perhaps un-necessary.
 */
void COutputView::HandlePerlError(int style, int position)
{
	BuildAndHandleREError(style, position, "at (?P<f>.+) line (?P<l>[0-9]+)");
}

/**
 * @brief Parse python errors
 * 
 * File "T:\source\ipconfig\ipconfig.py", line 45
 */
void COutputView::HandlePythonError(int style, int position)
{
	BuildAndHandleREError(style, position, "File \"(?P<f>.+)\", line (?P<l>[0-9]+)");
}

/**
 * Handle when a hotspot is clicked.
 */
LRESULT COutputView::OnHotSpotClicked(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(!m_bCustom)
	{
		switch(wParam)
		{
			case SCE_ERR_PYTHON:
			{
				HandlePythonError(wParam, lParam);
			}
			break;

			case SCE_ERR_GCC:
			{
				HandleGCCError(wParam, lParam);
			}
			break;

			case SCE_ERR_MS:
			{
				HandleMSError(wParam, lParam);
			}
			break;

			case SCE_ERR_BORLAND: // Borland C++
			{
				HandleBorlandCPPError(wParam, lParam);
			}
			break;

			case SCE_ERR_PERL:
			{
				HandlePerlError(wParam, lParam);
			}
			break;
		}
	}
	else
	{
		if( wParam == 20 )
		{
			HandleCustomError(wParam, lParam);
		}
	}

	return 0;
}

/**
 * In this function we look for a hotspot clicked notification, and if we get
 * one then we post a message to ourselves with the details required to handle
 * it. The reason we don't just do it here is that the notify will come in the
 * middle of Scintilla handling a click - therefore we get window focus problems.
 */
int COutputView::HandleNotify(LPARAM lParam)
{
	Scintilla::SCNotification *scn = (Scintilla::SCNotification*)lParam;
	if( scn->nmhdr.code == SCN_HOTSPOTCLICK )
	{
		// Ignore if Ctrl is pressed...
		if( (((Scintilla::SCNotification*)lParam)->modifiers & SCMOD_CTRL) != 0 )
			return baseClass::HandleNotify(lParam);

		int style = GetStyleAt(scn->position);
		PostMessage(PN_HANDLEHSCLICK, style, scn->position);
		return 0;
	}
	else
		return baseClass::HandleNotify(lParam);
}

void COutputView::DoContextMenu(CPoint* point)
{
	CSPopupMenu popup(IDR_POPUP_OUTPUT);
	popup.CheckMenuItem(ID_OUTPUT_WORDWRAP, GetWrapMode() == SC_WRAP_WORD);
	g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL, m_hWnd);
}

void COutputView::SafeAppendText(LPCSTR s, int len, bool bScrollToView)
{
	if(len == -1)
		len = strlen(s);
	SendMessage(SCI_APPENDTEXT, len, reinterpret_cast<LPARAM>(s));

	if(bScrollToView)
	{
		int line = SendMessage(SCI_GETLENGTH, 0, 0);
		line = SendMessage(SCI_LINEFROMPOSITION, line, 0);
		SendMessage(SCI_ENSUREVISIBLEENFORCEPOLICY, line);
		SendMessage(SCI_GOTOLINE, line);
	}
}

void COutputView::AddToolOutput(LPCTSTR output, int nLength)
{
	// TODO: Argh, the inefficiency...
	if (nLength == -1)
	{
		nLength = _tcslen(output);
	}

	tstring opconv(output, nLength);
	CT2CA outputconv(opconv.c_str());
	SafeAppendText(outputconv, -1);
}

void COutputView::SetToolBasePath(LPCTSTR path)
{
	m_basepath = path;
}

void COutputView::SetToolParser(bool bBuiltIn, const char* customExpression)
{
	if(!bBuiltIn && customExpression != NULL)
	{
		m_bCustom = true;
		SetRE(customExpression, false);
	}
	else
	{
		m_bCustom = false;
		SetOutputLexer();
	}

	SetWrapMode(OPTIONS->Get(PNSK_INTERFACE, _T("OutputWrap"), false) ? SC_WRAP_WORD : SC_WRAP_NONE);
}

void COutputView::ClearOutput()
{
	ClearAll();
}

void COutputView::ShowOutput()
{
	g_Context.m_frame->ToggleDockingWindow(PNDW_OUTPUT, true, true);
}

void COutputView::HideOutput()
{
	g_Context.m_frame->ToggleDockingWindow(PNDW_OUTPUT, true, false);
}

LRESULT COutputView::OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ClearAll();

	return 0;
}

LRESULT COutputView::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::SendMessage(GetTopLevelParent(), WM_COMMAND, ID_OUTPUT_HIDE, (LPARAM)m_hWnd);
	return 0;
}

LRESULT COutputView::OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SendMessage(WM_CUT, 0, 0);

	return 0;
}

LRESULT COutputView::OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SendMessage(WM_COPY, 0, 0);

	return 0;
}

/**
 * Toggle word-wrap for the output view
 */
LRESULT COutputView::OnWordWrap(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	bool toggle = !(GetWrapMode() == SC_WRAP_WORD);
	SetWrapMode(toggle ? SC_WRAP_WORD : SC_WRAP_NONE);
	OPTIONS->Set(PNSK_INTERFACE, _T("OutputWrap"), toggle);

	return 0;
}

void COutputView::OnFirstShow()
{	
	if(!m_bCustom)
		SetOutputLexer();
	else
		SetCustomLexer();

	SetWrapMode(OPTIONS->Get(PNSK_INTERFACE, _T("OutputWrap"), false) ? SC_WRAP_WORD : SC_WRAP_NONE);
}

void COutputView::SetOutputLexer()
{
	Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName("output");
	if(pScheme && ::IsWindow(m_hWnd))
	{
		pScheme->Load( *(static_cast<CScintilla*>(this)) );
		
		// Override some nastiness inherited from the default schemes...
		SPerform(SCI_SETCARETLINEVISIBLE, false);
		SPerform(SCI_SETEDGEMODE, EDGE_NONE);
	}
}

void COutputView::SetCustomLexer()
{
	if( ::IsWindow(m_hWnd) )
	{
		// Load the default output lexer styles etc.
		SetOutputLexer();

		// Switch to container-based lexing...
		SetLexer(SCLEX_CONTAINER);
	}
}

LRESULT COutputView::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_OUTPUT), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	bHandled = FALSE;

	return 0;
}