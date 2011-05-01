/**
 * @file aboutdlg.h
 * @brief interface of the CAboutDlg class
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef aboutdlg_h__included
#define aboutdlg_h__included

#include "version.h"

class CAboutDlg : public CDialogImpl<CAboutDlg>
{
public:
	enum { IDD = IDD_ABOUTBOX };

	BEGIN_MSG_MAP(CAboutDlg)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnCloseCmd)
		COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)
	END_MSG_MAP()

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		CenterWindow(GetParent());

		CString strVersion;
		strVersion.Format(LS(IDS_ABOUTVERSIONSTRING), PN_VERSION_FULL_T);

		CStatic sName;
		sName.Attach(GetDlgItem(IDC_NAME));
		
		fnTitle.CreateFont(
			14,                        // nHeight
			0,                         // nWidth
			0,                         // nEscapement
			0,                         // nOrientation
			FW_SEMIBOLD,               // nWeight
			FALSE,                     // bItalic
			FALSE,                     // bUnderline
			0,                         // cStrikeOut
			ANSI_CHARSET,              // nCharSet
			OUT_DEFAULT_PRECIS,        // nOutPrecision
			CLIP_DEFAULT_PRECIS,       // nClipPrecision
			DEFAULT_QUALITY,           // nQuality
			DEFAULT_PITCH | FF_SWISS,  // nPitchAndFamily
			_T("MS Shell Dlg 2"));     // lpszFacename

		sName.SetFont(fnTitle);

		::SetWindowText( GetDlgItem(IDC_VERSIONSTATIC), (LPCTSTR)strVersion);

		return TRUE;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		EndDialog(wID);
		return 0;
	}

	CFont fnTitle;
};

#endif // #ifndef aboutdlg_h__included
