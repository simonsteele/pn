#ifndef __ATLMSGBOXCHECK_H__
#define __ATLMSGBOXCHECK_H__

/////////////////////////////////////////////////////////////////////////////
// atlmsgboxex.h
// Written by Bjoern Graf (bjoern.graf@gmx.net)
// Copyright (c) 2003 Bjoern Graf.
//
// This code may be used in compiled form in any way you desire. This file
// may be redistributed by any means PROVIDING it is not sold for profit
// without the authors written consent, and providing that this notice and
// the authors name is included. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever. It's free, so don't hassle me about it.
//
// Modifications to support a forced system menu and also custom
// buttons by Simon Steele (s.steele@pnotepad.org) November 2004

//
// WTL adaptation of
// http://www.codeproject.com/dialog/xmessagebox.asp
//

#pragma once

#ifndef __cplusplus
	#error ATL requires C++ compilation (use a .cpp suffix)
#endif

#ifndef __ATLWIN_H__
	#error atlmsgboxcheck.h requires atlwin.h to be included first
#endif

namespace WTL
{
namespace BXT
{

//
// Additional buttons
//

#define MB_CONTINUEABORT	0x00000008L     // adds two buttons, "Continue"  and "Abort"
#define MB_CUSTOMBUTTONS	0x00000009L		// Use a custom list of buttons.
#define MB_FORCESYSMENU		0x00400000L		// Force system menu...
#define MB_DONOTASKAGAIN	0x01000000L     // add checkbox "Do not ask me again"
#define MB_DONOTTELLAGAIN	0x02000000L     // add checkbox "Do not tell me again"
#define MB_DONOTSHOWAGAIN	0x04000000L		// add checkbox "Do not show this message again"
#define MB_YESTOALL			0x10000000L     // must be used with either MB_YESNO or MB_YESNOCANCEL
#define MB_NOTOALL			0x20000000L     // must be used with either MB_YESNO or MB_YESNOCANCEL
#define MB_NORESOURCE		0x40000000L		// do not try to load button strings from resources
#define MB_NOSOUND			0x80000000L     // do not play sound when mb is displayed

#define MB_TYPESEXMASK				(MB_CONTINUEABORT | MB_DONOTASKAGAIN | MB_DONOTTELLAGAIN | MB_DONOTSHOWAGAIN | MB_YESTOALL | MB_NOTOALL | /*MB_NORESOURCE | */MB_NOSOUND)
#define MB_CHECKSMASK				(MB_DONOTASKAGAIN | MB_DONOTTELLAGAIN | MB_DONTSHOWAGAIN)

#define MB_DEFBUTTON5		0x00000400L
#define MB_DEFBUTTON6		0x00000500L

//
// Additional button return values
//

#ifndef IDCONTINUE
#define IDCONTINUE			11
#endif
#define IDYESTOALL			20
#define IDNOTOALL			21

// Return value flags
#define IDCHECKMARKED		0x0100

// Return value masks
#define MB_RESULTBUTTONMASK	0x00FF
#define MB_RESULTFLAGMASK	0x0F00

//
// Internal control identifiers
//

#define IDDONOTASKAGAIN		22
#define IDDONOTTELLAGAIN	23
#define IDDONOTSHOWAGAIN	24

#define IDICONCONTROL		100
#define IDMESSAGETEXT       101
#define IDCHECKBOX			102


#ifndef BXT_MB_MAX_BUTTON_TEXT
#define BXT_MB_MAX_BUTTON_TEXT	48
#endif

#ifndef BXT_IDS_MB_TEXT_BASE
#define BXT_IDS_MB_TEXT_BASE	0xDEAD	// 57005
#endif

/////////////////////////////////////////////////////////////////////////////
// CDlgTemplateBase

class CDlgTemplateBase
{
public:
	union
	{
		DLGTEMPLATE* m_pDlgTemplate;
		void* m_pData;
	};
	WORD*  m_pwCurrentPos;
	size_t m_size;

	CDlgTemplateBase() : m_pDlgTemplate(NULL), m_pwCurrentPos(NULL), m_size(0)
	{}
	~CDlgTemplateBase()
	{
		if(m_pDlgTemplate)
		{
			::free(m_pData);
			//delete [] m_pDlgTemplate;
			m_pDlgTemplate = NULL;
		}
	}

	bool Create(LPCTSTR pszClassName, LPCTSTR pszDlgTitle, DWORD dwStyle, SHORT x, SHORT y, SHORT cx, SHORT cy, WORD wFontSize, LPCTSTR pszFontName)
	{
		ATLASSERT(m_pDlgTemplate == NULL);

		// Allocate ample memory for building the template
#if 1
		m_size = sizeof(DLGTEMPLATE) + 2 * sizeof(WORD) + sizeof(WCHAR) * (::lstrlen(pszClassName) + ::lstrlen(pszDlgTitle) + ::lstrlen(pszFontName) + 3);
		m_size = ((m_size + 3) >> 2) << 2;
		m_pData = ::malloc(m_size);
		if(m_pData == NULL)
			return false;
		ZeroMemory(m_pData, m_size);
#else
		m_pDlgTemplate = new DLGTEMPLATE[50];
		if(m_pDlgTemplate == NULL)
			return false;
		ZeroMemory(m_pDlgTemplate, 50 * sizeof(DLGTEMPLATE));
#endif
		// Fill in the DLGTEMPLATE info
		DLGTEMPLATE* pdt = m_pDlgTemplate;
		pdt->style = dwStyle;
		pdt->dwExtendedStyle = 0L;
		pdt->cdit  = 0;
		pdt->x     = x;
		pdt->y     = y;
		pdt->cx    = cx;
		pdt->cy    = cy;

		m_pwCurrentPos = (WORD*)(++pdt);

		// Set Menu array to nothing
		*m_pwCurrentPos++ = L'\0';

		// Set Class array
		if(pszClassName != NULL)
			CopyToWideChar((WCHAR**)&m_pwCurrentPos, pszClassName);
		else
			*m_pwCurrentPos++ = L'\0';

		// Set Dlg title
		CopyToWideChar((WCHAR**)&m_pwCurrentPos, pszDlgTitle);

		// Set Font Size and Name
		*m_pwCurrentPos++ = wFontSize;
		if(pszFontName != NULL)
			CopyToWideChar((WCHAR**)&m_pwCurrentPos, pszFontName);
		else
			*m_pwCurrentPos++ = L'\0';

		return true;
	}

	bool AddControl(LPCTSTR pszClassName, LPCTSTR pszTitle, DWORD dwStyle, SHORT x, SHORT y, SHORT cx, SHORT cy, WORD id)
	{
		size_t size = sizeof(DLGITEMTEMPLATE) + sizeof(WCHAR) * (::lstrlen(pszClassName) + ::lstrlen(pszTitle) + 2) + sizeof(WORD);
		size = ((size + 3) >> 2) << 2;
		m_size += size;
		size_t offset = (BYTE*)m_pwCurrentPos - (BYTE*)m_pData;
		m_pData = ::realloc(m_pData, m_size);
		if(m_pData == NULL)
			return false;
		m_pwCurrentPos = (WORD*)((BYTE*)m_pData + offset);

		// Increase DIALOGTEMPLATE item counter
		m_pDlgTemplate->cdit++;

		// DWORD align the current ptr
		DLGITEMTEMPLATE* p;
		p = (DLGITEMTEMPLATE*)((((ULONG)(m_pwCurrentPos) + 3) >> 2) << 2);

		p->style = dwStyle;
		p->dwExtendedStyle = 0L;
		p->x  = x;
		p->y  = y;
		p->cx = cx;
		p->cy = cy;
		p->id = id;

		m_pwCurrentPos = (WORD*)(++p);

		// Set Class name
		CopyToWideChar((WCHAR**)&m_pwCurrentPos, pszClassName);

		// Set Title
		if (pszTitle)
			CopyToWideChar((WCHAR**)&m_pwCurrentPos, pszTitle);
		else
			*m_pwCurrentPos++ = L'\0';

		// Skip Extra Stuff
		*m_pwCurrentPos++ = 0;

		return true;
	}

	void CopyToWideChar(WCHAR** ppszOut, LPCTSTR pszIn)
	{
		DWORD  dwLen;
		WCHAR* pszOut;

		dwLen = ::lstrlen(pszIn);
		pszOut = *ppszOut;
#ifdef UNICODE
		// Copy Unicode to Unicode
		_wcsncpy(pszOut, pszIn, dwLen);
		// Add the null terminator
		pszOut[dwLen++] = L'\0';
#else
		// Copy Ansi to Unicode
		dwLen = MultiByteToWideChar(CP_ACP, 0, pszIn, dwLen, pszOut, dwLen);
		// Add the null terminator
		pszOut[dwLen++] = L'\0';
#endif
		*ppszOut += dwLen;
	}
};

/////////////////////////////////////////////////////////////////////////////
// CMsgBoxTemplateT

typedef struct tagMBITEM
{
	UINT	ID;
	LPCTSTR Caption;
} MBItem, *LPMBItem;

typedef const MBItem* LPCMBItem;

template < WORD TWidth, WORD THeight, bool TFitToContents, int TCheckBreakCount, bool TCenterButtons >
class CMsgBoxTemplateT : public CDlgTemplateBase
{
public:
	enum EMsgBoxConstrains
	{
		MB_SPACING        = 7,
		MB_BUTTON_WIDTH   = 50,
		MB_BUTTON_HEIGHT  = 14,
		MB_BUTTON_SPACING = 5,
		MB_MAX_WIDTH      = 500,
		MB_MIN_HEIGHT     = 55,
		MB_FORCE_WORD     = 0x7FFF,
	};

	HICON				m_hIcon;
	CSimpleArray<WORD>	m_buttons;
	WORD				m_buttonsWidth;
	LPCMBItem			m_lpButtonDefs;
	WORD				m_nButtons;

	CMsgBoxTemplateT() : CDlgTemplateBase(), m_hIcon(NULL), m_nButtons(0), m_lpButtonDefs(0)
	{}

	DLGTEMPLATE* Create(LPCTSTR pszMessage, LPCTSTR pszTitle, UINT uType)
	{
		DWORD dwStyle = DS_SHELLFONT | DS_SETFONT | DS_MODALFRAME | DS_FIXEDSYS | DS_CENTER | WS_POPUP | WS_CAPTION;
		UINT uButtons = uType & MB_TYPEMASK;
		bool bForceSysMenu = (uType & MB_FORCESYSMENU) != 0;
		if((uButtons != MB_ABORTRETRYIGNORE && uButtons != MB_YESNO && uButtons != MB_CONTINUEABORT) || bForceSysMenu)
			dwStyle |= WS_SYSMENU;
		if ((uType & MB_SYSTEMMODAL) != 0)
			dwStyle |= DS_SYSMODAL;

		CDlgTemplateBase::Create(NULL, pszTitle, dwStyle, 0, 0, TWidth, THeight, 8, _T("MS Shell Dlg"));

		// Set the default button.
		UINT nDefButton = (uType & MB_DEFMASK) >> 8;

		// Set up the available buttons.
		AddButtons(uType);

		if(TFitToContents && m_pDlgTemplate->cx < m_buttonsWidth + (2 * MB_SPACING))
			ResizeDlgTemplate(m_buttonsWidth + (2 * MB_SPACING), 0);

		// Add the icon and the message text.
		WORD x;
		WORD y = m_pDlgTemplate->cy - (MB_SPACING + MB_BUTTON_HEIGHT);
		WORD cy = y - (MB_SPACING + MB_BUTTON_SPACING);
		WORD cx;
		if(LoadIcon(uType))
		{
			AddControl(_T("Static"), NULL, SS_ICON | SS_REALSIZEIMAGE | WS_VISIBLE,
				MB_SPACING, MB_SPACING, 21, 20, IDICONCONTROL);
			x = MB_SPACING + 21 + MB_SPACING;
			cx = m_pDlgTemplate->cx - 21 - 3 * MB_SPACING;
		}
		else
		{
			x = MB_SPACING;
			cx = m_pDlgTemplate->cx - 2 * MB_SPACING;
			ResizeDlgTemplate(0, m_pDlgTemplate->cy - 9);
			y = m_pDlgTemplate->cy - (MB_SPACING + MB_BUTTON_HEIGHT);
			cy = y - (MB_SPACING + MB_BUTTON_SPACING);
		}
		AddControl(_T("Static"), pszMessage, SS_LEFT | SS_NOPREFIX | WS_VISIBLE,
			x, MB_SPACING, cx, cy, IDMESSAGETEXT);

		// Add the buttons to the dialog.
		if(TCenterButtons)
			x = (m_pDlgTemplate->cx - m_buttonsWidth) / 2;
		else
			x = m_pDlgTemplate->cx - (m_buttonsWidth + MB_SPACING);
		for(UINT i = 0, l = m_buttons.GetSize(); i < l; i++)
		{
			UINT uID = m_buttons[i];
			AddControl(_T("BUTTON"), GetButtonText(uID, (uType & MB_NORESOURCE) == 0),
				WS_TABSTOP | WS_VISIBLE | ((i == nDefButton) ? BS_DEFPUSHBUTTON : 0),
				x, y, MB_BUTTON_WIDTH, MB_BUTTON_HEIGHT, (WORD)uID);
			x += (MB_BUTTON_WIDTH + MB_BUTTON_SPACING);
		}

		// Add the show again checkbox.
		UINT uCheckId = 0;
		if((uType & MB_DONOTASKAGAIN) != 0)
			uCheckId = IDDONOTASKAGAIN;
		else if((uType & MB_DONOTTELLAGAIN) != 0)
			uCheckId = IDDONOTTELLAGAIN;
		else if((uType & MB_DONOTSHOWAGAIN) != 0)
			uCheckId = IDDONOTSHOWAGAIN;
		if(uCheckId != 0)
		{
			y = m_pDlgTemplate->cy - (MB_BUTTON_HEIGHT + MB_BUTTON_SPACING);
			if(m_buttons.GetSize() > TCheckBreakCount)
			{
				ResizeDlgTemplate(0, m_pDlgTemplate->cy + MB_BUTTON_HEIGHT);
				y += MB_BUTTON_HEIGHT + MB_BUTTON_SPACING;
			}
			AddControl(_T("BUTTON"), GetButtonText(uCheckId, (uType & MB_NORESOURCE) == 0), 
				BS_AUTOCHECKBOX | WS_TABSTOP | WS_VISIBLE, MB_SPACING, y, 135, 8, IDCHECKBOX);
		}

		if((uType & MB_NOSOUND) == 0)
			::MessageBeep(uType & MB_ICONMASK);

		return m_pDlgTemplate;
	}

	void ResizeDlgTemplate(WORD cx, WORD cy)
	{
		if(cx > 0)
			m_pDlgTemplate->cx = min(cx, MB_MAX_WIDTH);
		if(cy > 0)
			m_pDlgTemplate->cy = max(cy, MB_MIN_HEIGHT);
	}

	void AddCustomButtons()
	{
		LPCMBItem pItem = m_lpButtonDefs;
		if(pItem == NULL)
			return;

		for(int i = 0; i < m_nButtons; i++)
		{
			if(pItem == NULL)
				break;

			if(pItem->ID != NULL)
			{
				m_buttons.Add(pItem->ID);
			}
			if(pItem->Caption != NULL)
			{
				ATLASSERT(_tcslen(pItem->Caption) < BXT_MB_MAX_BUTTON_TEXT);
			}
			pItem++;
		}
	}

	void AddButtons(UINT uType)
	{
		switch(uType & MB_TYPEMASK)
		{
		case MB_CUSTOMBUTTONS:
			AddCustomButtons();
			break;
		case MB_OK:
			m_buttons.Add(IDOK);
			break;
		case MB_OKCANCEL:
			m_buttons.Add(IDOK);
			m_buttons.Add(IDCANCEL);
			break;
		case MB_ABORTRETRYIGNORE:
			m_buttons.Add(IDABORT);
			m_buttons.Add(IDRETRY);
			m_buttons.Add(IDIGNORE);
			break;
		case MB_YESNOCANCEL:
			m_buttons.Add(IDYES);
			if((uType & MB_YESTOALL) != 0)
				m_buttons.Add(IDYESTOALL);
			m_buttons.Add(IDNO);
			if((uType & MB_NOTOALL) != 0)
				m_buttons.Add(IDNOTOALL);
			m_buttons.Add(IDCANCEL);
			break;
		case MB_YESNO:
			m_buttons.Add(IDYES);
			if((uType & MB_YESTOALL) != 0)
				m_buttons.Add(IDYESTOALL);
			m_buttons.Add(IDNO);
			if((uType & MB_NOTOALL) != 0)
				m_buttons.Add(IDNOTOALL);
			break;
		case MB_RETRYCANCEL:
			m_buttons.Add(IDRETRY);
			m_buttons.Add(IDCANCEL);
			break;
#if(WINVER >= 0x0500)
		case MB_CANCELTRYCONTINUE:
			m_buttons.Add(IDCANCEL);
			m_buttons.Add(IDTRYAGAIN);
			m_buttons.Add(IDCONTINUE);
			break;
#endif // WINVER >= 0x0500
		case MB_CONTINUEABORT:
			m_buttons.Add(IDCONTINUE);
			m_buttons.Add(IDABORT);
			break;
		}

		if((uType & MB_HELP) != 0)
			m_buttons.Add(IDHELP);

		m_buttonsWidth = (WORD)((m_buttons.GetSize() * (MB_BUTTON_WIDTH + MB_BUTTON_SPACING)) - MB_BUTTON_SPACING);
	}

	bool GetCustomText(UINT uID, LPTSTR szText) const
	{
		LPCMBItem pItem = m_lpButtonDefs;

		for(int i = 0; i < m_nButtons; i++)
		{
			if(pItem == NULL)
				break;

			if(pItem->ID == uID)
			{
				if(pItem->Caption != NULL && _tcslen(pItem->Caption) > 0)
				{
					_tcscpy(szText, pItem->Caption);
					return true;
				}
				else
					break;
			}

			pItem++;
		}

		return false;
	}

	LPCTSTR GetButtonText(UINT uID, bool bUseResource) const
	{
		static TCHAR szText[BXT_MB_MAX_BUTTON_TEXT];

		szText[0] = 0;
		if(bUseResource)
		{
			TCHAR szTextLoad[BXT_MB_MAX_BUTTON_TEXT];
			int nRes = ::LoadString(_pModule->GetResourceInstance(), BXT_IDS_MB_TEXT_BASE + uID, szTextLoad, BXT_MB_MAX_BUTTON_TEXT);
			if(nRes < BXT_MB_MAX_BUTTON_TEXT - 1)
				::lstrcpy(szText, szTextLoad);
		}

		if(::lstrlen(szText) == 0)
		{
			switch(uID)
			{
			case IDOK:
				::lstrcpy(szText, _T("OK"));
				break;
			case IDCANCEL:
				::lstrcpy(szText, _T("Cancel"));
				break;
			case IDABORT:
				::lstrcpy(szText, _T("&Abort"));
				break;
			case IDRETRY:
				::lstrcpy(szText, _T("&Retry"));
				break;
			case IDIGNORE:
				::lstrcpy(szText, _T("&Ignore"));
				break;
			case IDYES:
				::lstrcpy(szText, _T("&Yes"));
				break;
			case IDNO:
				::lstrcpy(szText, _T("&No"));
				break;
#if(WINVER >= 0x0400)
			case IDCLOSE:
				::lstrcpy(szText, _T("Close"));
				break;
			case IDHELP:
				::lstrcpy(szText, _T("&Help"));
				break;
#endif // WINVER >= 0x0400
#if(WINVER >= 0x0500)
			case IDTRYAGAIN:
				::lstrcpy(szText, _T("&Try again"));
				break;
#endif // WINVER >= 0x0500
			case IDCONTINUE:
				::lstrcpy(szText, _T("&Continue"));
				break;
			case IDYESTOALL:
				::lstrcpy(szText, _T("Yes to &All"));
				break;
			case IDNOTOALL:
				::lstrcpy(szText, _T("No to A&ll"));
				break;
			case IDDONOTASKAGAIN:
				::lstrcpy(szText, _T("Do not ask me &again"));
				break;
			case IDDONOTTELLAGAIN:
				::lstrcpy(szText, _T("Do not tell me &again"));
				break;
			case IDDONOTSHOWAGAIN:
				::lstrcpy(szText, _T("Do not show this message &again"));
				break;
			default:
				if(!GetCustomText(uID, szText))
					ATLASSERT(false);
				break;
			}
		}
		return szText;
	}

	bool LoadIcon(UINT uType)
	{
		if((uType & MB_ICONMASK) != 0)
		{
			LPCTSTR lpIcon = NULL;
			switch(uType & MB_ICONMASK)
			{
			case MB_ICONEXCLAMATION:
				lpIcon = (LPCTSTR)IDI_EXCLAMATION;
				break;
			case MB_ICONHAND:
				lpIcon = (LPCTSTR)IDI_HAND;
				break;
			case MB_ICONQUESTION:
				lpIcon = (LPCTSTR)IDI_QUESTION;
				break;
			case MB_ICONASTERISK:
				lpIcon = (LPCTSTR)IDI_ASTERISK;
				break;
			}

			if(lpIcon)
				m_hIcon = ::LoadIcon(NULL, lpIcon);
		}
		return m_hIcon != NULL;
	}

	void FitToContents(CDialogImplBase* dlg)
	{
		if(TFitToContents)
		{
			CRect rc;
			HWND hWndIcon = dlg->GetDlgItem(IDICONCONTROL);

			int maxTextWidth = MB_MAX_WIDTH - (2 * MB_SPACING);
			if(hWndIcon)
				maxTextWidth -= (21 + MB_SPACING);

			// Get the size of the static text display control.
			CRect rcStatic;
			HWND hWnd = dlg->GetDlgItem(IDMESSAGETEXT);
			::GetWindowRect(hWnd, &rcStatic);
			
			// Get the text to be displayed.
			int len = ::GetWindowTextLength(hWnd);
			TCHAR* pszText = new TCHAR[len+1];
			::GetWindowText(hWnd, pszText, len+1);

			// Now measure the text display area, with maximums as defined by MB_MAX_WIDTH x THeight
			RECT rcText = { 0, 0, maxTextWidth, THeight };
			CClientDC dc(dlg->m_hWnd);
			HFONT hFont = (HFONT)dlg->SendMessage(WM_GETFONT);
			HFONT hOldFont = dc.SelectFont(hFont);
			dlg->MapDialogRect(&rcText);
			dc.DrawText(pszText, len, &rcText, DT_CALCRECT | DT_CENTER | DT_LEFT | DT_NOPREFIX | DT_WORDBREAK);
			dc.SelectFont(hOldFont);
			delete [] pszText;

			// rcText now contains the size needed to render the text within that box.
			// rcStatic is the current size of the text control.

			int widthText = rcText.right;
			int heightText = rcText.bottom;

			int dx = rcStatic.Width() - widthText;
			int dy = rcStatic.Height() - heightText;

			if(dy < 0 && hWndIcon != NULL)
			{
				RECT rcIcon;
				::GetWindowRect(hWndIcon, &rcIcon);
				dlg->ScreenToClient(&rcIcon);

				dy = ((rcIcon.bottom - rcIcon.top) - rcText.bottom) / 2;
				dlg->ScreenToClient(&rcStatic);
				::SetWindowPos(hWnd, NULL, rcStatic.left, rcStatic.top + dy, rcText.right, rcText.bottom, SWP_NOZORDER | SWP_SHOWWINDOW);
				dy = 0;
			}
			else
				::SetWindowPos(hWnd, NULL, 0, 0, rcText.right, rcText.bottom, SWP_NOZORDER | SWP_NOMOVE | SWP_SHOWWINDOW);

			RECT rcMax = { MB_MAX_WIDTH, m_pDlgTemplate->cx, 0, m_pDlgTemplate->cy };
			dlg->MapDialogRect(&rcMax);
			dlg->GetWindowRect(&rc);
			
			dx = -dx;
			dy = -dy;

			if(rcMax.left < rc.Width() + dx)
				dx = 0;
			if(rcMax.bottom < rc.Height() + dy)
				dy = 0;

			dlg->SetWindowPos(NULL, 0, 0, rc.Width() + dx, rc.Height() + dy, SWP_NOZORDER | SWP_NOMOVE | SWP_SHOWWINDOW);

			hWnd = dlg->GetDlgItem(IDCHECKBOX);
			::GetWindowRect(hWnd, &rc);
			dlg->ScreenToClient(&rc);
			::SetWindowPos(hWnd, NULL, rc.left, rc.top + dy, 0, 0, SWP_NOZORDER | SWP_NOSIZE | SWP_SHOWWINDOW);

			for(int i = 0, l = m_buttons.GetSize(); i < l; i++)
			{
				hWnd = dlg->GetDlgItem(m_buttons[i]);
				::GetWindowRect(hWnd, &rc);
				dlg->ScreenToClient(&rc);
				if(TCenterButtons)
					::SetWindowPos(hWnd, NULL, rc.left + dx / 2, rc.top + dy, 0, 0, SWP_NOZORDER | SWP_NOSIZE | SWP_SHOWWINDOW);
				else
					::SetWindowPos(hWnd, NULL, rc.left + dx, rc.top + dy, 0, 0, SWP_NOZORDER | SWP_NOSIZE | SWP_SHOWWINDOW);
			}

			if((m_pDlgTemplate->style & DS_CENTER) != 0)
				PNCenterWindow(dlg->m_hWnd, dlg->GetParent());
				//dlg->CenterWindow(dlg->GetParent()); - no multi-monitor support.
		}
	}
};

/////////////////////////////////////////////////////////////////////////////
// CMessageBoxCheckT

template < class TDlgTemplate >
class CMessageBoxCheckT : public CDialogImplBase
{
public:
	TDlgTemplate m_dlgTmpl;

	void SetCustomButtons(LPCMBItem lpItems, WORD nItems)
	{
		m_dlgTmpl.m_lpButtonDefs = lpItems;
		m_dlgTmpl.m_nButtons = nItems;
	}

// Creation
	INT_PTR DoModal(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title, UINT uType)
	{
		ATLASSERT(hWndOwner == NULL || ::IsWindow(hWndOwner));
		ATLASSERT(m_hWnd == NULL);

		LPTSTR lpstrMessage = NULL;
		if(IS_INTRESOURCE(message.m_lpstr))
		{
			for(int nLen = 256; ; nLen *= 2)
			{
				ATLTRY(lpstrMessage = new TCHAR[nLen]);
				if(lpstrMessage == NULL)
				{
					ATLASSERT(FALSE);
					return 0;
				}
				int nRes = ::LoadString(_pModule->GetResourceInstance(), LOWORD(message.m_lpstr), lpstrMessage, nLen);
				if(nRes < nLen - 1)
					break;
				delete [] lpstrMessage;
				lpstrMessage = NULL;
			}

			message.m_lpstr = lpstrMessage;
		}

		LPTSTR lpstrTitle = NULL;
		if(IS_INTRESOURCE(title.m_lpstr) && LOWORD(title.m_lpstr) != 0)
		{
			for(int nLen = 256; ; nLen *= 2)
			{
				ATLTRY(lpstrTitle = new TCHAR[nLen]);
				if(lpstrTitle == NULL)
				{
					ATLASSERT(FALSE);
					return 0;
				}
				int nRes = ::LoadString(_pModule->GetResourceInstance(), LOWORD(title.m_lpstr), lpstrTitle, nLen);
				if(nRes < nLen - 1)
					break;
				delete [] lpstrTitle;
				lpstrTitle = NULL;
			}

			title.m_lpstr = lpstrTitle;
		}

		DLGTEMPLATE* pDlgTmpl = m_dlgTmpl.Create(message.m_lpstr, title.m_lpstr, uType);
		_AtlWinModule.AddCreateWndData(&m_thunk.cd, (CDialogImplBase*)this);
		int nRet = ::DialogBoxIndirect(_AtlBaseModule.GetResourceInstance(),
			pDlgTmpl, hWndOwner, StartDialogProc);

		m_hWnd = NULL;
		delete [] lpstrMessage;
		delete [] lpstrTitle;

		return nRet;
	}

// Message map and handlers
	BEGIN_MSG_MAP(CMessageBoxCheck)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDHELP, OnHelp)
#if(WINVER >= 0x0500)
		COMMAND_RANGE_HANDLER(IDOK, IDCONTINUE, OnCloseCmd)
#else
		COMMAND_RANGE_HANDLER(IDOK, IDCLOSE, OnCloseCmd)
#endif // WINVER >= 0x500
		COMMAND_RANGE_HANDLER(IDCONTINUE, IDNOTOALL, OnCloseCmd)
	END_MSG_MAP()

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		if(m_dlgTmpl.m_hIcon)
			::SendMessage(GetDlgItem(IDICONCONTROL), STM_SETICON, (WPARAM)m_dlgTmpl.m_hIcon, 0);

		m_dlgTmpl.FitToContents(this);
		return FALSE;
	}

	LRESULT OnHelp(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		HWND hWndOwner = GetWindow(GW_OWNER);
		if(hWndOwner != NULL)
		{
			HELPINFO hi;
			hi.cbSize = sizeof(hi);
			hi.iContextType = HELPINFO_WINDOW;
			hi.iCtrlId      = IDHELP;
			hi.hItemHandle  = m_hWnd;
			hi.dwContextId  = 0;
			::GetCursorPos(&hi.MousePos);
			::SendMessage(hWndOwner, WM_HELP, 0, (LPARAM)&hi);
		}
		return 0;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if((m_dlgTmpl.m_pDlgTemplate->style & WS_SYSMENU) == 0 && wID == IDCANCEL)
			return 0;

		if(wID == IDCANCEL && GetDlgItem(IDCANCEL) == NULL)
			wID = IDOK;

		if(IsDlgButtonChecked(IDCHECKBOX) == BST_CHECKED)
			wID |= IDCHECKMARKED;
		::EndDialog(m_hWnd, wID);
		return 0;
	}

// Implementation
};

typedef CMsgBoxTemplateT<257, 68, true, 2, false> CMsgBoxTemplateNet;
typedef CMsgBoxTemplateT<141, 58, true, 0, true> CMsgBoxTemplateWin;

typedef CMessageBoxCheckT<CMsgBoxTemplateNet> CMessageBoxCheckNet;
typedef CMessageBoxCheckT<CMsgBoxTemplateWin> CMessageBoxCheckWin;

#if !defined(MB_CHECK_STYLE) || MB_CHECK_STYLE == 0
typedef CMessageBoxCheckWin CMessageBoxCheck;
#else
typedef CMessageBoxCheckNet CMessageBoxCheck;
#endif

};


/////////////////////////////////////////////////////////////////////////////
// AtlMessageBoxCheck[Net|Win] - accepts both memory and resource based strings

inline int AtlMessageBoxCheck(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title = (LPCTSTR)NULL, UINT uType = MB_OK | MB_ICONINFORMATION)
{
	int nRet;
#if !defined(MB_CHECK_STYLE) || MB_CHECK_STYLE == 0
	if((uType & MB_TYPESEXMASK) == 0)
		nRet = AtlMessageBox(hWndOwner, message, title, uType);
	else
#endif
	{
		BXT::CMessageBoxCheck msgBox;
		nRet = msgBox.DoModal(hWndOwner, message, title, uType);
	}

	return nRet;
}

inline int AtlMessageBoxCheckNet(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title = (LPCTSTR)NULL, UINT uType = MB_OK | MB_ICONINFORMATION)
{
	BXT::CMessageBoxCheckNet msgBox;
	return msgBox.DoModal(hWndOwner, message, title, uType);
}
inline int AtlMessageBoxCheckWin(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title = (LPCTSTR)NULL, UINT uType = MB_OK | MB_ICONINFORMATION)
{
	int nRet;
	if((uType & MB_TYPESEXMASK) == 0)
		nRet = AtlMessageBox(hWndOwner, message, title, uType);
	else
	{
		BXT::CMessageBoxCheckWin msgBox;
		nRet = msgBox.DoModal(hWndOwner, message, title, uType);
	}
	return nRet;
}

inline int AtlCustomMessageBoxNet(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title = (LPCTSTR)NULL, BXT::LPCMBItem lpItems = NULL, WORD nItems = 0, UINT uType = MB_CUSTOMBUTTONS | MB_ICONINFORMATION)
{
	BXT::CMessageBoxCheckNet msgBox;
	msgBox.SetCustomButtons(lpItems, nItems);
	
	// Ensure custom buttons.
	int type = uType;
	
	if(lpItems != NULL && nItems > 0)
		type |= MB_CUSTOMBUTTONS;
	
	return msgBox.DoModal(hWndOwner, message, title, type);
}

inline int AtlCustomMessageBoxWin(HWND hWndOwner, _U_STRINGorID message, _U_STRINGorID title = (LPCTSTR)NULL, BXT::LPCMBItem lpItems = NULL, WORD nItems = 0, UINT uType = MB_CUSTOMBUTTONS | MB_ICONINFORMATION)
{
	BXT::CMessageBoxCheckWin msgBox;
	msgBox.SetCustomButtons(lpItems, nItems);
	
	// Ensure custom buttons.
	int type = uType;
	
	if(lpItems != NULL && nItems > 0)
		type |= MB_CUSTOMBUTTONS;
	
	return msgBox.DoModal(hWndOwner, message, title, type);
}

};

#endif // __ATLMSGBOXCHECK_H__
