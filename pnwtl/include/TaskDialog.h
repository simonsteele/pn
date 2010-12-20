#if !defined(AFX_TASKDIALOG_H__20073232_5AA0_1E88_3A6D_0080AD509054__INCLUDED_)
#define AFX_TASKDIALOG_H__20073232_5AA0_1E88_3A6D_0080AD509054__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CTask98Dialog - Task Dialog for legacy Windows platforms
//
// Written by Bjarke Viksoe (bjarke@viksoe.dk)
// Copyright (c) 2007 Bjarke Viksoe.
//
// This code may be used in compiled form in any way you desire. This
// source file may be redistributed by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever. It's free, so don't hassle me about it.
//
// Beware of bugs.
//


// The dialog requires a few string and icon resources to be defined. Ironic isn't it: the dialog
// is dynamically constructed but we still need resources.
// Here is a list:
//   IDS_TASKDLG_OK            "&OK"
//   IDS_TASKDLG_YES           "&Yes"
//   IDS_TASKDLG_NO            "&No"
//   IDS_TASKDLG_CANCEL        "&Cancel"
//   IDS_TASKDLG_RETRY         "&Retry"
//   IDS_TASKDLG_CLOSE         "Close"
//
//   IDI_TASKDLG_CHEVRON_LESS  Expander icon (less)
//   IDI_TASKDLG_CHEVRON_MORE  Expander icon (more)
//   IDI_TASKDLG_ARROW_NORMAL  Green Link Arrow
//   IDI_TASKDLG_ARROW_HOT     Green Link Arrow (hover)
//
#include "resource.h"



/////////////////////////////////////////////////////////////////////////
// TaskDialog declares
//

#if _WIN32_WINNT < 0x0501

#ifdef _WIN32
#include <pshpack1.h>
#endif // _WIN32

#define MAX_LINKID_TEXT 48

#define L_MAX_URL_LENGTH (2048 + 32 + sizeof("://"))

typedef struct tagLITEM
{
  UINT  mask;
  int   iLink;
  UINT  state;
  UINT  stateMask;
  WCHAR szID[MAX_LINKID_TEXT];
  WCHAR szUrl[L_MAX_URL_LENGTH];
} LITEM, *PLITEM;

typedef struct tagNMLINK
{
  NMHDR hdr;
  LITEM item;
} NMLINK, *PNMLINK;

class CLinkCtrl
{
public:
   static LPCTSTR GetWndClassName()
   {
      return _T("SysLink");
   }
};

#define BCM_FIRST               0x1600      // Button control messages
#define BCM_SETIMAGELIST        (BCM_FIRST + 0x0002)

typedef struct
{
    HIMAGELIST  himl;
    RECT        margin;
    UINT        uAlign;
} BUTTON_IMAGELIST, *PBUTTON_IMAGELIST;


#ifdef _WIN32
#include <poppack.h>
#endif // _WIN32

#endif // _WIN32_WINNT

#if _WIN32_WINNT < 0x0600 && !defined(TD_WARNING_ICON)

#ifdef _WIN32
#include <pshpack1.h>
#endif // _WIN32

typedef HRESULT (CALLBACK *PFTASKDIALOGCALLBACK)(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LONG_PTR lpRefData);

enum _TASKDIALOG_FLAGS
{
    TDF_ENABLE_HYPERLINKS               = 0x0001,
    TDF_USE_HICON_MAIN                  = 0x0002,
    TDF_USE_HICON_FOOTER                = 0x0004,
    TDF_ALLOW_DIALOG_CANCELLATION       = 0x0008,
    TDF_USE_COMMAND_LINKS               = 0x0010,
    TDF_USE_COMMAND_LINKS_NO_ICON       = 0x0020,
    TDF_EXPAND_FOOTER_AREA              = 0x0040,
    TDF_EXPANDED_BY_DEFAULT             = 0x0080,
    TDF_VERIFICATION_FLAG_CHECKED       = 0x0100,
    TDF_SHOW_PROGRESS_BAR               = 0x0200,
    TDF_SHOW_MARQUEE_PROGRESS_BAR       = 0x0400,
    TDF_CALLBACK_TIMER                  = 0x0800,
    TDF_POSITION_RELATIVE_TO_WINDOW     = 0x1000,
    TDF_RTL_LAYOUT                      = 0x2000,
    TDF_NO_DEFAULT_RADIO_BUTTON         = 0x4000,
    TDF_CAN_BE_MINIMIZED                = 0x8000
};
typedef int TASKDIALOG_FLAGS;   

typedef enum _TASKDIALOG_MESSAGES
{
    TDM_NAVIGATE_PAGE                       = WM_USER + 101,
    TDM_CLICK_BUTTON                        = WM_USER + 102,
    TDM_SET_MARQUEE_PROGRESS_BAR            = WM_USER + 103,
    TDM_SET_PROGRESS_BAR_STATE              = WM_USER + 104,
    TDM_SET_PROGRESS_BAR_RANGE              = WM_USER + 105,
    TDM_SET_PROGRESS_BAR_POS                = WM_USER + 106,
    TDM_SET_PROGRESS_BAR_MARQUEE            = WM_USER + 107,
    TDM_SET_ELEMENT_TEXT                    = WM_USER + 108,
    TDM_CLICK_RADIO_BUTTON                  = WM_USER + 110,
    TDM_ENABLE_BUTTON                       = WM_USER + 111,
    TDM_ENABLE_RADIO_BUTTON                 = WM_USER + 112,
    TDM_CLICK_VERIFICATION                  = WM_USER + 113,
    TDM_UPDATE_ELEMENT_TEXT                 = WM_USER + 114,
    TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER + 115,
    TDM_UPDATE_ICON                         = WM_USER + 116 
} TASKDIALOG_MESSAGES;

typedef enum _TASKDIALOG_NOTIFICATIONS
{
    TDN_CREATED                         = 0,
    TDN_NAVIGATED                       = 1,
    TDN_BUTTON_CLICKED                  = 2,
    TDN_HYPERLINK_CLICKED               = 3,
    TDN_TIMER                           = 4,
    TDN_DESTROYED                       = 5,
    TDN_RADIO_BUTTON_CLICKED            = 6,
    TDN_DIALOG_CONSTRUCTED              = 7,
    TDN_VERIFICATION_CLICKED            = 8,
    TDN_HELP                            = 9,
    TDN_EXPANDO_BUTTON_CLICKED          = 10
} TASKDIALOG_NOTIFICATIONS;

typedef struct _TASKDIALOG_BUTTON
{
    int     nButtonID;
    PCWSTR  pszButtonText;
} TASKDIALOG_BUTTON;

typedef enum _TASKDIALOG_ELEMENTS
{
    TDE_CONTENT,
    TDE_EXPANDED_INFORMATION,
    TDE_FOOTER,
    TDE_MAIN_INSTRUCTION
} TASKDIALOG_ELEMENTS;

typedef enum _TASKDIALOG_ICON_ELEMENTS
{
    TDIE_ICON_MAIN,
    TDIE_ICON_FOOTER
} TASKDIALOG_ICON_ELEMENTS;

#define TD_WARNING_ICON         MAKEINTRESOURCEW(-1)
#define TD_ERROR_ICON           MAKEINTRESOURCEW(-2)
#define TD_INFORMATION_ICON     MAKEINTRESOURCEW(-3)
#define TD_SHIELD_ICON          MAKEINTRESOURCEW(-4)

#define TDT_WARNING_ICON         MAKEINTRESOURCE(-1)
#define TDT_ERROR_ICON           MAKEINTRESOURCE(-2)
#define TDT_INFORMATION_ICON     MAKEINTRESOURCE(-3)
#define TDT_SHIELD_ICON          MAKEINTRESOURCE(-4)

enum _TASKDIALOG_COMMON_BUTTON_FLAGS
{
    TDCBF_OK_BUTTON            = 0x0001, // selected control return value IDOK
    TDCBF_YES_BUTTON           = 0x0002, // selected control return value IDYES
    TDCBF_NO_BUTTON            = 0x0004, // selected control return value IDNO
    TDCBF_CANCEL_BUTTON        = 0x0008, // selected control return value IDCANCEL
    TDCBF_RETRY_BUTTON         = 0x0010, // selected control return value IDRETRY
    TDCBF_CLOSE_BUTTON         = 0x0020  // selected control return value IDCLOSE
};
typedef int TASKDIALOG_COMMON_BUTTON_FLAGS;

typedef struct _TASKDIALOGCONFIG
{
    UINT        cbSize;
    HWND        hwndParent;
    HINSTANCE   hInstance;                              // used for MAKEINTRESOURCE() strings
    TASKDIALOG_FLAGS                dwFlags;            // TASKDIALOG_FLAGS (TDF_XXX) flags
    TASKDIALOG_COMMON_BUTTON_FLAGS  dwCommonButtons;    // TASKDIALOG_COMMON_BUTTON (TDCBF_XXX) flags
    PCWSTR      pszWindowTitle;                         // string or MAKEINTRESOURCE()
    union
    {
        HICON   hMainIcon;
        PCWSTR  pszMainIcon;
    };
    PCWSTR      pszMainInstruction;
    PCWSTR      pszContent;
    UINT        cButtons;
    const TASKDIALOG_BUTTON  *pButtons;
    int         nDefaultButton;
    UINT        cRadioButtons;
    const TASKDIALOG_BUTTON  *pRadioButtons;
    int         nDefaultRadioButton;
    PCWSTR      pszVerificationText;
    PCWSTR      pszExpandedInformation;
    PCWSTR      pszExpandedControlText;
    PCWSTR      pszCollapsedControlText;
    union
    {
        HICON   hFooterIcon;
        PCWSTR  pszFooterIcon;
    };
    PCWSTR      pszFooter;
    PFTASKDIALOGCALLBACK pfCallback;
    LONG_PTR    lpCallbackData;
    UINT        cxWidth;
} TASKDIALOGCONFIG;

#ifdef _WIN32
#include <poppack.h>
#endif // _WIN32

#endif // _WIN32_WINNT

#if _WIN32_WINNT < 0x0600

inline int AtlTaskDialog(HWND hWndParent, 
                         ATL::_U_STRINGorID WindowTitle, ATL::_U_STRINGorID MainInstructionText, ATL::_U_STRINGorID ContentText, 
                         TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons = 0U, ATL::_U_STRINGorID Icon = (LPCTSTR)NULL)
{
   int nRet = -1;
   typedef HRESULT (STDAPICALLTYPE *PFN_TaskDialog)(HWND hwndParent, HINSTANCE hInstance, PCWSTR pszWindowTitle, PCWSTR pszMainInstruction, PCWSTR pszContent, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons, PCWSTR pszIcon, int* pnButton);
   HMODULE m_hCommCtrlDLL = ::LoadLibrary(_T("comctl32.dll"));
   if(m_hCommCtrlDLL != NULL)
   {
      PFN_TaskDialog pfnTaskDialog = (PFN_TaskDialog)::GetProcAddress(m_hCommCtrlDLL, "TaskDialog");
      if(pfnTaskDialog != NULL)
      {
         USES_CONVERSION;
         HRESULT hRet = pfnTaskDialog(hWndParent, ModuleHelper::GetResourceInstance(), 
            IS_INTRESOURCE(WindowTitle.m_lpstr) ? (LPCWSTR) WindowTitle.m_lpstr : T2CW(WindowTitle.m_lpstr), 
            IS_INTRESOURCE(MainInstructionText.m_lpstr) ? (LPCWSTR) MainInstructionText.m_lpstr : T2CW(MainInstructionText.m_lpstr), 
            IS_INTRESOURCE(ContentText.m_lpstr) ?  (LPCWSTR) ContentText.m_lpstr : T2CW(ContentText.m_lpstr), 
            dwCommonButtons, 
            IS_INTRESOURCE(Icon.m_lpstr) ? (LPCWSTR) Icon.m_lpstr : T2CW(Icon.m_lpstr),
            &nRet);
         ATLVERIFY(SUCCEEDED(hRet));
      }

      ::FreeLibrary(m_hCommCtrlDLL);
   }
   return nRet;
}

#endif // _WIN32_WINNT < 0x0600

#if !(defined(IDI_SHIELD))
#define TD_WARNING_ICON         MAKEINTRESOURCEW(-1)
#define TD_ERROR_ICON           MAKEINTRESOURCEW(-2)
#define TD_INFORMATION_ICON     MAKEINTRESOURCEW(-3)
#define TD_SHIELD_ICON          MAKEINTRESOURCEW(-4)

#define TDT_WARNING_ICON         MAKEINTRESOURCE(-1)
#define TDT_ERROR_ICON           MAKEINTRESOURCE(-2)
#define TDT_INFORMATION_ICON     MAKEINTRESOURCE(-3)
#define TDT_SHIELD_ICON          MAKEINTRESOURCE(-4)

#define IDI_SHIELD          MAKEINTRESOURCE(32518)
#endif

inline int AtlTaskDialogIndirect(TASKDIALOGCONFIG* pTask, int* pnButton = NULL, int* pnRadioButton = NULL, BOOL* pfVerificationFlagChecked = NULL)
{
   // This allows apps to run on older versions of Windows
   typedef HRESULT (STDAPICALLTYPE *PFN_TaskDialogIndirect)(const TASKDIALOGCONFIG* pTaskConfig, int* pnButton, int* pnRadioButton, BOOL* pfVerificationFlagChecked);

   HRESULT hRet = E_UNEXPECTED;
   HMODULE m_hCommCtrlDLL = ::LoadLibrary(_T("comctl32.dll"));
   if(m_hCommCtrlDLL != NULL)
   {
      PFN_TaskDialogIndirect pfnTaskDialogIndirect = (PFN_TaskDialogIndirect)::GetProcAddress(m_hCommCtrlDLL, "TaskDialogIndirect");
      if(pfnTaskDialogIndirect != NULL)
         hRet = pfnTaskDialogIndirect(pTask, pnButton, pnRadioButton, pfVerificationFlagChecked);

      ::FreeLibrary(m_hCommCtrlDLL);
   }
   return hRet;
}

/////////////////////////////////////////////////////////////////////////
// CDialogBaseUnits - Dialog helper
//

/*class CDialogBaseUnits
{
public:
   SIZE m_sizeUnits;

   // Constructors

   CDialogBaseUnits()
   {
      // The base units of the out-dated System Font
      LONG nDlgBaseUnits = ::GetDialogBaseUnits();
      m_sizeUnits.cx = LOWORD(nDlgBaseUnits);
      m_sizeUnits.cy = HIWORD(nDlgBaseUnits);
   }

   CDialogBaseUnits(HWND hWnd)
   {
      m_sizeUnits.cx = m_sizeUnits.cy = 0;
      InitDialogBaseUnits(hWnd);
   }

   CDialogBaseUnits(HFONT hFont, HWND hWnd = NULL)
   {
      m_sizeUnits.cx = m_sizeUnits.cy = 0;
      InitDialogBaseUnits(hFont, hWnd);
   }

   CDialogBaseUnits(LOGFONT lf, HWND hWnd = NULL)
   {
      m_sizeUnits.cx = m_sizeUnits.cy = 0;
      InitDialogBaseUnits(lf, hWnd);
   }

   // Operations

   BOOL InitDialogBaseUnits(HWND hWnd)
   {
      ATLASSERT(::IsWindow(hWnd));
      RECT rc = { 0, 0, 4, 8 };
      if( !::MapDialogRect(hWnd, &rc) ) return FALSE;
      m_sizeUnits.cx = rc.right;
      m_sizeUnits.cy = rc.bottom;
      return TRUE;
   }

   BOOL InitDialogBaseUnits(LOGFONT lf, HWND hWnd = NULL)
   {
      CFont font;
      font.CreateFontIndirect(&lf);
      if( font.IsNull() ) return FALSE;
      return InitDialogBaseUnits(font, hWnd);
   }

   BOOL InitDialogBaseUnits(HFONT hFont, HWND hWnd = NULL)
   {
      ATLASSERT(hFont != NULL);
      CWindowDC dc = hWnd;
      HFONT hFontOld = dc.SelectFont(hFont);
      TEXTMETRIC tmText = { 0 };
      dc.GetTextMetrics(&tmText);
      m_sizeUnits.cy = tmText.tmHeight + tmText.tmExternalLeading;
      SIZE sizeText = { 0 };
      dc.GetTextExtent(_T("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"), 52, &sizeText);
      m_sizeUnits.cx = (sizeText.cx + 26) / 52;
      dc.SelectFont(hFontOld);
      return TRUE;
   }

   SIZE GetDialogBaseUnits() const
   {
      return m_sizeUnits;
   }

   INT MapDialogPixelsX(INT x) const
   {
      return MulDiv(x, 4, m_sizeUnits.cx);  // Pixels X to DLU
   }

   INT MapDialogPixelsY(INT y) const
   {
      return MulDiv(y, 8, m_sizeUnits.cy);  // Pixels Y to DLU
   }

   POINT MapDialogPixels(POINT pt) const
   {
      POINT out = { MapDialogPixelsX(pt.x), MapDialogPixelsY(pt.y) };
      return out;
   }

   SIZE MapDialogPixels(SIZE input) const
   {
      SIZE out = { MapDialogPixelsX(input.cx), MapDialogPixelsY(input.cy) };
      return out;
   }

   RECT MapDialogPixels(RECT input) const
   {
      RECT out = { MapDialogPixelsX(input.left), MapDialogPixelsY(input.top), MapDialogPixelsX(input.right), MapDialogPixelsY(input.bottom) };
      return out;
   }

   INT MapDialogUnitsX(INT x) const
   {
      return MulDiv(x, m_sizeUnits.cx, 4);  // DLU to Pixels X
   }

   INT MapDialogUnitsY(INT y) const
   {
      return MulDiv(y, m_sizeUnits.cx, 8);  // DLU to Pixels Y
   }

   POINT MapDialogUnits(POINT pt) const
   {
      POINT out = { MapDialogUnitsX(pt.x), MapDialogUnitsY(pt.y) };
      return out;
   }

   SIZE MapDialogUnits(SIZE input) const
   {
      SIZE out = { MapDialogUnitsX(input.cx), MapDialogUnitsY(input.cy) };
      return out;
   }

   RECT MapDialogUnits(RECT input) const
   {
      RECT out = { MapDialogUnitsX(input.left), MapDialogUnitsY(input.top), MapDialogUnitsX(input.right), MapDialogUnitsY(input.bottom) };
      return out;
   }
};*/


/////////////////////////////////////////////////////////////////////////
// TaskDialog dialog implementation
//

template< class T >
class CTask98DialogImpl : public CIndirectDialogImpl<T>, public CDialogBaseUnits
{
public:
   enum
   {
      IDC_TASKDLG_OK = IDOK,
      IDC_TASKDLG_YES = IDYES,
      IDC_TASKDLG_NO = IDNO,
      IDC_TASKDLG_CANCEL = IDCANCEL,
      IDC_TASKDLG_RETRY = IDRETRY,
      IDC_TASKDLG_CLOSE = IDCLOSE,
      //
      IDC_TASKDLG_TITLETEXT = 0x7400,
      IDC_TASKDLG_INSTRUCTIONSTEXT,
      IDC_TASKDLG_INSTRUCTIONSICON,
      IDC_TASKDLG_CONTENTTEXT,
      IDC_TASKDLG_VERIFYBUTTON,
      IDC_TASKDLG_EXPANDERICON,
      IDC_TASKDLG_EXPANDERTEXT,
      IDC_TASKDLG_EXTRATEXT,
      IDC_TASKDLG_FOOTERTEXT,
      IDC_TASKDLG_FOOTERICON,
      IDC_TASKDLG_PROGRESSBAR,
      //
      IDC_TASKDLG_CUSTOMBUTTON_FIRST,
      IDC_TASKDLG_CUSTOMBUTTON_LAST = IDC_TASKDLG_CUSTOMBUTTON_FIRST + 20,
      IDC_TASKDLG_RADIOBUTTON_FIRST,
      IDC_TASKDLG_RADIOBUTTON_LAST = IDC_TASKDLG_RADIOBUTTON_FIRST + 20,
   };

   enum 
   { 
      TIMERID_TIGGER = 4050,
      TIMERID_HOVERLINK = 4051,
      WIDTH_PROBE = 99999,
   };

   enum { MAX_TEXT_LENGTH = 2048 };

   CTask98DialogImpl() : m_bNavigated(false)
   {
      ::ZeroMemory(&m_cfg, sizeof(m_cfg));
      m_cfg.cbSize = sizeof(TASKDIALOGCONFIG);
      m_cfg.hInstance = ModuleHelper::GetResourceInstance();
      m_cfg.pfCallback = T::TaskDialogCallback;
      m_cfg.lpCallbackData = (LONG_PTR) static_cast<T*>(this);
      _Reset();
   }

   // Message map

   BEGIN_MSG_MAP(CTask98DialogImpl)
      MESSAGE_HANDLER(WM_INITDIALOG, OnWmInitDialog)
      MESSAGE_HANDLER(WM_DESTROY, OnWmDestroy)
      MESSAGE_HANDLER(WM_HELP, OnWmHelp)
      MESSAGE_HANDLER(WM_TIMER, OnWmTimer)
      MESSAGE_HANDLER(WM_ERASEBKGND, OnWmEraseBkgnd)
      MESSAGE_HANDLER(WM_CTLCOLORSTATIC, OnWmCtlColor)
      MESSAGE_HANDLER(WM_CTLCOLORBTN, OnWmCtlColor)
      MESSAGE_HANDLER(WM_SYSCOMMAND, OnSysCommand)
      MESSAGE_HANDLER(WM_DRAWITEM, OnWmDrawItem)
      MESSAGE_HANDLER(TDM_CLICK_BUTTON, OnMsgClickButton)
      MESSAGE_HANDLER(TDM_CLICK_VERIFICATION, OnMsgClickVerification)
      MESSAGE_HANDLER(TDM_CLICK_RADIO_BUTTON, OnMsgClickRadioButton)
      MESSAGE_HANDLER(TDM_ENABLE_BUTTON, OnMsgEnableButton)
      MESSAGE_HANDLER(TDM_ENABLE_RADIO_BUTTON, OnMsgEnableRadioButton)
      MESSAGE_HANDLER(TDM_SET_PROGRESS_BAR_POS, OnMsgSetProgressBarPos)
      MESSAGE_HANDLER(TDM_SET_PROGRESS_BAR_RANGE, OnMsgSetProgressBarRange)
      MESSAGE_HANDLER(TDM_SET_PROGRESS_BAR_STATE, OnMsgSetProgressBarState)
      MESSAGE_HANDLER(TDM_SET_PROGRESS_BAR_MARQUEE, OnMsgSetProgressBarMarquee)
      MESSAGE_HANDLER(TDM_SET_MARQUEE_PROGRESS_BAR, OnMsgSetMarqueeProgressBar)
      MESSAGE_HANDLER(TDM_SET_ELEMENT_TEXT, OnMsgSetElementText)
      MESSAGE_HANDLER(TDM_UPDATE_ELEMENT_TEXT, OnMsgUpdateElementText)
      COMMAND_HANDLER(IDC_TASKDLG_EXPANDERICON, STN_CLICKED, OnMsgExpandoClick);
      COMMAND_HANDLER(IDC_TASKDLG_EXPANDERTEXT, STN_CLICKED, OnMsgExpandoClick);
      COMMAND_HANDLER(IDC_TASKDLG_EXPANDERICON, STN_DBLCLK, OnMsgExpandoClick);
      COMMAND_HANDLER(IDC_TASKDLG_EXPANDERTEXT, STN_DBLCLK, OnMsgExpandoClick);
      COMMAND_ID_HANDLER(IDC_TASKDLG_VERIFYBUTTON, OnMsgVerificationClick);
      COMMAND_RANGE_HANDLER(1, 64, OnMsgCommonButtonClick)
      COMMAND_RANGE_HANDLER(IDC_TASKDLG_CUSTOMBUTTON_FIRST, IDC_TASKDLG_CUSTOMBUTTON_LAST, OnMsgCustomButtonClick)
      COMMAND_RANGE_HANDLER(IDC_TASKDLG_RADIOBUTTON_FIRST, IDC_TASKDLG_RADIOBUTTON_LAST, OnMsgRadioClick)
      NOTIFY_HANDLER(IDC_TASKDLG_FOOTERTEXT, NM_CLICK, OnMsgHyperlinkClicked)
      NOTIFY_HANDLER(IDC_TASKDLG_EXTRATEXT, NM_CLICK, OnMsgHyperlinkClicked)
   END_MSG_MAP()

   // Operations

   bool SetConfig(const TASKDIALOGCONFIG* pConfig)
   {
      ATLASSERT(pConfig);
      m_cfg = *pConfig;
      if( m_cfg.cButtons > 20 ) return false;
      if( m_cfg.cRadioButtons > 20 ) return false;
      if( (m_cfg.dwFlags & (TDF_USE_COMMAND_LINKS|TDF_USE_COMMAND_LINKS_NO_ICON)) != 0 && m_cfg.cButtons == 0 ) return FALSE;
      if( m_cfg.pszExpandedInformation != NULL && m_cfg.pszExpandedControlText == NULL ) m_cfg.pszExpandedControlText = L"Show details";
      if( m_cfg.pszExpandedInformation != NULL && m_cfg.pszCollapsedControlText == NULL ) m_cfg.pszCollapsedControlText= L"Hide details";
      return true;
   }

   bool IsNavigated() const
   {
      return m_bNavigated;
   }

   void GetDialogResult(int* pnButton, int* pnRadioButton, BOOL* pfVerificationFlagChecked) const
   {
      ATLASSERT(!::IsWindow(m_hWnd));
      if( pnButton != NULL ) *pnButton = m_iButtonResult;
      if( pnRadioButton != NULL ) *pnRadioButton = m_iRadioResult;
      if( pfVerificationFlagChecked != NULL ) *pfVerificationFlagChecked = m_bVerificationResult;
   }

   BOOL NavigatePage(TASKDIALOGCONFIG* pTaskConfig)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      if( !SetConfig(pTaskConfig) ) return FALSE;
      m_bNavigated = true;
      // BUG: Unlike the real Task Dialog we destroy the window and recreate
      //      a new dialog. There will be flicker and possible reposition of window.
      return EndDialog(IDCANCEL);
   }

   BOOL ClickButton(UINT uID)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      CButton ctrl = GetDlgItem(uID);
      for( UINT i = 0; i < m_cfg.cButtons; i++ ) {
         if( m_cfg.pButtons[i].nButtonID == (int) uID ) ctrl = GetDlgItem(IDC_TASKDLG_CUSTOMBUTTON_FIRST + i);
      }
      if( !ctrl.IsWindow() ) return FALSE;
      ctrl.Click();
      return TRUE;
   }

   BOOL EnableButton(UINT uID, BOOL bEnable)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      CButton ctrl = GetDlgItem(uID);
      for( UINT i = 0; i < m_cfg.cButtons; i++ ) {
         if( m_cfg.pButtons[i].nButtonID == (int) uID ) ctrl = GetDlgItem(IDC_TASKDLG_CUSTOMBUTTON_FIRST + i);
      }
      if( !ctrl.IsWindow() ) return FALSE;
      return ctrl.EnableWindow(bEnable);
   }

   BOOL ClickRadioButton(UINT uID)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      CButton ctrl;
      for( UINT i = 0; i < m_cfg.cRadioButtons; i++ ) {
         if( m_cfg.pRadioButtons[i].nButtonID == (int) uID ) ctrl = GetDlgItem(IDC_TASKDLG_RADIOBUTTON_FIRST + i);
      }
      if( !ctrl.IsWindow() ) return FALSE;
      ctrl.Click();
      return TRUE;
   }

   BOOL EnableRadioButton(UINT uID, BOOL bEnable)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      CButton ctrl;
      for( UINT i = 0; i < m_cfg.cRadioButtons; i++ ) {
         if( m_cfg.pRadioButtons[i].nButtonID == (int) uID ) ctrl = GetDlgItem(IDC_TASKDLG_RADIOBUTTON_FIRST + i);
      }
      if( !ctrl.IsWindow() ) return FALSE;
      return ctrl.EnableWindow(bEnable);
   }

   BOOL ClickVerification(BOOL bChecked, BOOL bTakeFocus)
   {
      BOOL bRes = CheckDlgButton(IDC_TASKDLG_VERIFYBUTTON, bChecked ? BST_CHECKED : BST_UNCHECKED);
      if( bRes && bTakeFocus ) CWindow(GetDlgItem(IDC_TASKDLG_VERIFYBUTTON)).SetFocus();
      return bRes;
   }

   BOOL SetElementText(TASKDIALOG_ELEMENTS Element, LPCWSTR pstrText)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      UINT uID = 0;
      switch( Element) {
      case TDE_CONTENT:              uID = IDC_TASKDLG_CONTENTTEXT; break;
      case TDE_MAIN_INSTRUCTION:     uID = IDC_TASKDLG_INSTRUCTIONSTEXT; break;
      case TDE_EXPANDED_INFORMATION: uID = IDC_TASKDLG_EXTRATEXT; break;
      case TDE_FOOTER:               uID = IDC_TASKDLG_FOOTERTEXT; break;
      }
      if( uID == 0 ) return FALSE;
      LPTSTR pstrBuffer = (LPTSTR) malloc(MAX_TEXT_LENGTH * sizeof(TCHAR));
      if( pstrBuffer == NULL ) return FALSE;
      _LoadString(pstrText, pstrBuffer, MAX_TEXT_LENGTH);
      SetDlgItemText(uID, pstrBuffer);
      free(pstrBuffer);
      // BUG: We need to reconstruct the dialog to make space for the new text
      return TRUE;
   }

   BOOL UpdateElementText(TASKDIALOG_ELEMENTS Element, LPCWSTR pstrText)
   {
      return SetElementText(Element, pstrText);
   }

   BOOL UpdateIcon(TASKDIALOG_ICON_ELEMENTS Element, LPCWSTR pstrIcon)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      UINT nCtlId = 0;
      switch( Element ) {
      case TDIE_ICON_FOOTER:  
         nCtlId = IDC_TASKDLG_FOOTERICON; 
         m_iconMain = _LoadIcon(pstrIcon, m_Metrics.cxyLargeIcon); 
         break;
      case TDIE_ICON_MAIN:    
         nCtlId = IDC_TASKDLG_INSTRUCTIONSICON; 
         m_iconFooter = _LoadIcon(pstrIcon, m_Metrics.cxySmallIcon); 
         break;
      default: return FALSE;
      }
      CWindow wnd = GetDlgItem(nCtlId);
      if( !wnd.IsWindow() ) return FALSE;
      return wnd.Invalidate();
   }

   BOOL SetProgressBarPos(UINT nPos)
   {
      ATLASSERT(::IsWindow(GetDlgItem(IDC_TASKDLG_PROGRESSBAR)));
      return SendDlgItemMessage(IDC_TASKDLG_PROGRESSBAR, PBM_SETPOS, nPos) != 0;
   }

   BOOL SetProgressBarRange(UINT nMin, UINT nMax)
   {
      ATLASSERT(::IsWindow(GetDlgItem(IDC_TASKDLG_PROGRESSBAR)));
      return SendDlgItemMessage(IDC_TASKDLG_PROGRESSBAR, PBM_SETRANGE, 0, MAKELPARAM(nMin, nMax)) != 0;
   }

   BOOL SetProgressBarState(int nNewState)
   {
      ATLASSERT(RunTimeHelper::IsVista());
      ATLASSERT(::IsWindow(GetDlgItem(IDC_TASKDLG_PROGRESSBAR)));
#ifndef PBM_SETSTATE
      const UINT PBM_SETSTATE = WM_USER + 16;
#endif // PBM_SETMARQUEE
      return SendDlgItemMessage(IDC_TASKDLG_PROGRESSBAR, PBM_SETSTATE, (WPARAM) nNewState, 0) != 0;
   }

   BOOL SetProgressBarMarquee(int iMarquee, UINT uTimer)
   {
      ATLASSERT(RunTimeHelper::IsVista());
      ATLASSERT(::IsWindow(GetDlgItem(IDC_TASKDLG_PROGRESSBAR)));
#ifndef PBM_SETMARQUEE
      const UINT PBM_SETMARQUEE = WM_USER + 10;
#endif // PBS_MARQUEE
      CWindow wnd = GetDlgItem(IDC_TASKDLG_PROGRESSBAR);
      if( !wnd.IsWindow() ) return FALSE;
      return wnd.SendMessage(PBM_SETMARQUEE, (WPARAM) iMarquee, uTimer) != 0;
   }

   void SetMarqueeProgressBar(BOOL bMarquee)
   {
      ATLASSERT(RunTimeHelper::IsVista());
      ATLASSERT(::IsWindow(GetDlgItem(IDC_TASKDLG_PROGRESSBAR)));
#ifndef PBS_MARQUEE
      const DWORD PBS_MARQUEE = 0x08;
#endif // PBS_MARQUEE
      CWindow wnd = GetDlgItem(IDC_TASKDLG_PROGRESSBAR);
      if( !wnd.IsWindow() ) return;
      wnd.ModifyStyle(bMarquee ? 0 : PBS_MARQUEE, bMarquee ? PBS_MARQUEE : 0);
      SetProgressBarMarquee(1, 30);
   }

   // Default TaskDialog callback implementation

   static HRESULT CALLBACK TaskDialogCallback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LONG_PTR lpRefData)
   {
      ATLASSERT(lpRefData!=0);
      T* pT = (T*) lpRefData;
      ATLASSERT(hwnd==pT->m_hWnd); hwnd;
      BOOL bRet = FALSE;
      switch( msg ) {
      case TDN_DIALOG_CONSTRUCTED:
         pT->OnDialogConstructed();
         break;
      case TDN_CREATED:
         pT->OnCreated();
         break;
      case TDN_BUTTON_CLICKED:
         bRet = pT->OnButtonClicked((int) wParam);
         break;
      case TDN_RADIO_BUTTON_CLICKED:
         pT->OnRadioButtonClicked((int) wParam);
         break;
      case TDN_HYPERLINK_CLICKED:
         pT->OnHyperlinkClicked((LPCWSTR) lParam);
         break;
      case TDN_EXPANDO_BUTTON_CLICKED:
         pT->OnExpandoButtonClicked(wParam != 0);
         break;
      case TDN_VERIFICATION_CLICKED:
         pT->OnVerificationClicked(wParam != 0);
         break;
      case TDN_HELP:
         pT->OnHelp();
         break;
      case TDN_TIMER:
         bRet = pT->OnTimer((DWORD) wParam);
         break;
      case TDN_NAVIGATED:
         pT->OnNavigated();
         break;
      case TDN_DESTROYED:
         pT->OnDestroyed();
         break;
      default:
         ATLTRACE2(atlTraceUI, 0, _T("Unknown notification received in CTask98DialogImpl::TaskDialogCallback\n"));
         break;
      }
      return (HRESULT) bRet;
   }

   // Overrideables - notification handlers

   void OnDialogConstructed()
   {
   }

   void OnCreated()
   {
   }

   BOOL OnButtonClicked(int /*nButton*/)
   {
      return FALSE;   // don't prevent dialog to close
   }

   void OnRadioButtonClicked(int /*nRadioButton*/)
   {
   }

   void OnHyperlinkClicked(LPCWSTR /*pszHREF*/)
   {
   }

   void OnExpandoButtonClicked(bool /*bExpanded*/)
   {
   }

   void OnVerificationClicked(bool /*bChecked*/)
   {
   }

   void OnHelp()
   {
   }

   BOOL OnTimer(DWORD /*dwTickCount*/)
   {
      return FALSE;   // don't reset counter
   }

   void OnNavigated()
   {
   }

   void OnDestroyed()
   {
   }

   // Data Members

   TASKDIALOGCONFIG m_cfg;           // Copy of configuration
   bool m_bNavigated;                // Dialog was navigated?
   int m_iButtonResult;              // Button result
   int m_iRadioResult;               // Radio-button result
   BOOL m_bVerificationResult;       // Verification button result
   DWORD m_dwTick;                   // Start tick when dialog was shown
   UINT m_nDefCtlId;                 // Default dialog button
   UINT m_nHoverId;                  // Currently hover Command Link button ID
   SIZE m_sizeDialog;                // Recommended dialog size (in pixels)
   CFont m_fontTitle;                // Title font
   CFont m_fontText;                 // Text font
   CBrush m_brWhite;                 // Brush for white (top) background
   CBrush m_brGrey;                  // Brush for grey (bottom) background
   CIcon m_iconMain;                 // Icon in Main Instructions text
   CIcon m_iconFooter;               // Icon in Footer text
   CIcon m_iconArrowNormal;          // Command Link arrow
   CIcon m_iconArrowHot;             // Command Link arrow (hot)
   CIcon m_iconChevronLess;          // Chevron icon (less)
   CIcon m_iconChevronMore;          // Chevron icon (more)
   bool m_bCreated;                  // Dialog fully initialized yet?
   bool m_bExpanded;                 // Expando area is expanded?
   bool m_bHasCustomLinks;           // Has custom Command Link buttons?

   struct {
      SIZE sizeDialogPadding;
      SIZE sizeButtonPadding;
      SIZE sizeLinkPadding;
      SIZE sizeRadioButton;
      INT cxRadioIndent;
      INT cxButtonGap;
      INT cxButtonsDivider;
      INT cxySmallIcon;
      INT cxyLargeIcon;
      INT cxyArrowIcon;
      INT cxyExpanderIcon;
      INT cxLargeIconGap;
      INT cxSmallIconGap;
      INT cyInstructionsGap;
      INT cyContentGap;
      INT cyRadioGap;
      INT cyExpanderGap;
      INT cyButtonLineGap;
      INT cyProgressBar;
      COLORREF clrTitleText;
      COLORREF clrDividerDark;
      COLORREF clrDividerLight;
      COLORREF clrButtonDivider;
      COLORREF clrBkTop;
      COLORREF clrBkBottom;
      COLORREF clrCmdLinkSelect;
      LONG iButtonLinePos;
      LONG iFooterLinePos;
      LONG iExpandedLinePos;
      SIZE sizeButtons;
      INT cxMinButton;
      INT cxMinCommandLink;
      INT cxMaxVerification;
      INT cxBestMainInstruction;
      INT cxBestContent;
      INT cxBestRadioButton;
      INT cxBestCommandLink;
      INT cxBestProgressBar;
   } m_Metrics;

   // Construction

   void DoInitTemplate() 
   {
      m_Template.Reset();

      ::InitCommonControls();

      bool bIsCommCtrl6 = RunTimeHelper::IsCommCtrl6();

      CWindowDC dc = HWND_DESKTOP;
      if( !m_fontTitle.IsNull() ) m_fontTitle.DeleteObject();
      if( !m_fontText.IsNull() ) m_fontText.DeleteObject();
      CLogFont lfMsgBox;
      lfMsgBox.SetMessageBoxFont();
      m_fontText.CreateFontIndirect(&lfMsgBox);
      CLogFont lfTitle = lfMsgBox;
      lfTitle.MakeLarger(bIsCommCtrl6 ? 4 : 2);
      lfTitle.MakeBolder(bIsCommCtrl6 ? 0 : 1);
      m_fontTitle.CreateFontIndirect(&lfTitle);

      InitDialogBaseUnits(m_fontText, m_cfg.hwndParent);

      SIZE baseUnit = GetDialogBaseUnits();
      m_Metrics.sizeDialogPadding.cx = baseUnit.cx * 3 / 2;
      m_Metrics.sizeDialogPadding.cy = baseUnit.cy * 2 / 3;
      m_Metrics.sizeButtonPadding.cx = baseUnit.cx * 3;
      m_Metrics.sizeButtonPadding.cy = baseUnit.cy * 3 / 10;
      m_Metrics.sizeLinkPadding.cx = baseUnit.cx * 2;
      m_Metrics.sizeLinkPadding.cy = baseUnit.cy / 2;
      m_Metrics.sizeRadioButton.cx = ::GetSystemMetrics(SM_CXMENUCHECK) + 6;
      m_Metrics.sizeRadioButton.cy = ::GetSystemMetrics(SM_CYMENUCHECK);
      m_Metrics.cxRadioIndent = baseUnit.cx * 3 / 2;
      m_Metrics.cxButtonGap = baseUnit.cx * 1;
      m_Metrics.cxButtonsDivider = 40;
      m_Metrics.cxySmallIcon = ::GetSystemMetrics(SM_CYSMICON);
      m_Metrics.cxyLargeIcon = ::GetSystemMetrics(SM_CYICON);
      m_Metrics.cxyArrowIcon = 20;
      m_Metrics.cxyExpanderIcon = 20;
      m_Metrics.cxLargeIconGap = baseUnit.cx * 3 / 2;
      m_Metrics.cxSmallIconGap = baseUnit.cx * 3 / 2;
      m_Metrics.cyInstructionsGap = baseUnit.cy * 3 / 4;
      m_Metrics.cyContentGap = baseUnit.cy * 3 / 2;
      m_Metrics.cyRadioGap = baseUnit.cy / 2;
      m_Metrics.cyExpanderGap = baseUnit.cy / 3;
      m_Metrics.cyButtonLineGap = baseUnit.cy * 2 / 3;
      m_Metrics.cyProgressBar = baseUnit.cy * 1;
      m_Metrics.iButtonLinePos = 0;
      m_Metrics.iFooterLinePos = 0;
      m_Metrics.iExpandedLinePos = 0;
      m_Metrics.sizeButtons.cx = 0;
      m_Metrics.sizeButtons.cy = 0;
      m_Metrics.cxMinButton = baseUnit.cx * 4;  // +8 DLU
      m_Metrics.cxMinCommandLink = baseUnit.cx * 60;
      m_Metrics.cxMaxVerification = baseUnit.cx * 70;
      m_Metrics.cxBestMainInstruction = baseUnit.cx * 60;
      m_Metrics.cxBestContent = baseUnit.cx * 50;
      m_Metrics.cxBestCommandLink = baseUnit.cx * 70;
      m_Metrics.cxBestProgressBar = baseUnit.cx * 45;
      m_Metrics.cxBestRadioButton = baseUnit.cx * 60;
      m_Metrics.clrTitleText = bIsCommCtrl6 ? RGB(0,51,153) : ::GetSysColor(COLOR_BTNTEXT);
      m_Metrics.clrCmdLinkSelect = bIsCommCtrl6 ? RGB(140,232,255) : RGB(140,140,140);
      m_Metrics.clrBkTop = ::GetSysColor(bIsCommCtrl6 ? COLOR_WINDOW : COLOR_BTNFACE);
      m_Metrics.clrBkBottom = ::GetSysColor(COLOR_BTNFACE);
      m_Metrics.clrDividerDark = ::GetSysColor(COLOR_BTNSHADOW);
      m_Metrics.clrDividerLight = ::GetSysColor(COLOR_BTNHIGHLIGHT);
      m_Metrics.clrButtonDivider = ::GetSysColor(bIsCommCtrl6 ? COLOR_BTNSHADOW : COLOR_BTNFACE);

      if( !m_brWhite.IsNull() ) m_brWhite.DeleteObject();
      if( !m_brGrey.IsNull() ) m_brGrey.DeleteObject();
      m_brWhite.CreateSolidBrush(m_Metrics.clrBkTop);
      m_brGrey.CreateSolidBrush(m_Metrics.clrBkBottom);

      // Determine size of dialog. First try to determine the optimal width of
      // the dialog. Then calculate the height of the dialog based on that width.
      m_sizeDialog = _LayoutControls(WIDTH_PROBE, false);
      const LONG CX_MIN_DIALOG = baseUnit.cx * 60;
      if( m_sizeDialog.cx < CX_MIN_DIALOG ) m_sizeDialog.cx = CX_MIN_DIALOG;
      if( m_cfg.cxWidth > 0 ) m_sizeDialog.cx = MapDialogUnitsX(m_cfg.cxWidth);
      LONG cxWidth = m_sizeDialog.cx;
      m_sizeDialog = _LayoutControls(cxWidth, false);
      m_sizeDialog.cx = cxWidth;

      if( m_cfg.dwCommonButtons == 0 && m_cfg.cButtons == 0 ) m_cfg.dwCommonButtons = TDCBF_OK_BUTTON;
      if( (m_cfg.dwCommonButtons & TDCBF_CANCEL_BUTTON) != 0 ) m_cfg.dwFlags |= TDF_ALLOW_DIALOG_CANCELLATION;

      DWORD dwHelpID = 0;
      DWORD dwExStyle = 0;
      DWORD dwStyle = WS_POPUP | WS_CAPTION | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | DS_MODALFRAME | DS_ABSALIGN;
      if( (m_cfg.dwFlags & TDF_CAN_BE_MINIMIZED) != 0 ) dwStyle |= WS_MINIMIZEBOX;
      if( (m_cfg.dwFlags & TDF_ALLOW_DIALOG_CANCELLATION) != 0 ) dwStyle |= WS_SYSMENU;      
      if( (m_cfg.dwFlags & TDF_POSITION_RELATIVE_TO_WINDOW) == 0 ) dwStyle |= DS_CENTER;
#ifndef WS_EX_LAYOUTRTL
      const UINT WS_EX_LAYOUTRTL = 0x00400000L;
#endif // WS_EX_LAYOUTRTL
      if( (m_cfg.dwFlags & TDF_RTL_LAYOUT) != 0 ) dwExStyle |= WS_EX_LAYOUTRTL | WS_EX_RIGHT;

      TCHAR szCaption[120] = { 0 };
      _LoadString(m_cfg.pszWindowTitle, szCaption, sizeof(szCaption) / sizeof(TCHAR));

      WORD lfHeight = (WORD) lfMsgBox.lfHeight;
      if( lfMsgBox.lfHeight < 0 ) lfHeight = (WORD) (-MulDiv(72, lfMsgBox.lfHeight, GetDeviceCaps(dc, LOGPIXELSY)));

      SIZE dluDialog = MapDialogPixels(m_sizeDialog);
      RECT rc = _CenterDialog(dluDialog);
      m_Template.Create(true, szCaption, 
         (short) rc.left, (short) rc.top, (short) dluDialog.cx, (short) dluDialog.cy, 
         dwStyle, dwExStyle, 
         lfMsgBox.lfFaceName, lfHeight, (WORD) lfMsgBox.lfWeight, lfMsgBox.lfItalic, lfMsgBox.lfCharSet, 
         dwHelpID);
   }

   void DoInitControls() 
   {
      _LayoutControls(m_sizeDialog.cx, true);
   }

   // Message handler

   LRESULT OnWmInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      // Not accepting callbacks at this moment...
      m_bCreated = false;
      _Reset();
      // Font assignment
      SendMessageToDescendants(WM_SETFONT, (WPARAM) (HFONT) m_fontText);
      SendDlgItemMessage(IDC_TASKDLG_INSTRUCTIONSTEXT, WM_SETFONT, (WPARAM) (HFONT) m_fontTitle);
      // Create a timer?
      if( m_bHasCustomLinks ) SetTimer(TIMERID_HOVERLINK, 100);
      if( (m_cfg.dwFlags & TDF_CALLBACK_TIMER) != 0 ) SetTimer(TIMERID_TIGGER, 200);
      // Set verification checkmark
      if( (m_cfg.dwFlags & TDF_VERIFICATION_FLAG_CHECKED) != 0 ) CheckDlgButton(IDC_TASKDLG_VERIFYBUTTON, BST_CHECKED);
      // Set Marquee Progress Bar
      if( (m_cfg.dwFlags & TDF_SHOW_MARQUEE_PROGRESS_BAR) != 0 ) SetMarqueeProgressBar(TRUE);
      // Set default radio button (may be first button)
      if( (m_cfg.dwFlags & TDF_NO_DEFAULT_RADIO_BUTTON) == 0 && m_cfg.cRadioButtons > 0 ) {
         CButton ctrl;
         for( UINT n = 0; !ctrl.IsWindow() && n < m_cfg.cRadioButtons; n++ ) {
            if( m_cfg.nDefaultRadioButton == m_cfg.pRadioButtons[n].nButtonID ) ctrl = GetDlgItem(IDC_TASKDLG_RADIOBUTTON_FIRST + n);
         }
         if( !ctrl.IsWindow() ) ctrl = GetDlgItem(IDC_TASKDLG_RADIOBUTTON_FIRST);
         if( ctrl.IsWindow() ) ctrl.Click();
      }
      // Clear arrow image on lines?
      if( !m_bHasCustomLinks && (m_cfg.dwFlags & TDF_USE_COMMAND_LINKS_NO_ICON) != 0 ) {
         for( UINT n = 0; n < m_cfg.cButtons; n++ ) {
            CButton ctrl = GetDlgItem(IDC_TASKDLG_CUSTOMBUTTON_FIRST + n);
            BUTTON_IMAGELIST bil = { (HIMAGELIST) -1, 0 };
            ctrl.SendMessage(BCM_SETIMAGELIST, 0, (LPARAM) &bil);
         }
      }
      // Set expansion level
      if( m_cfg.pszExpandedInformation != NULL && (m_cfg.dwFlags & TDF_EXPANDED_BY_DEFAULT) == 0 ) {
         SendMessage(WM_COMMAND, MAKEWPARAM(IDC_TASKDLG_EXPANDERICON, STN_CLICKED));
      }
      // Set default dialog button
      if( m_nDefCtlId > 0 && m_nDefCtlId != (UINT) -1 ) {
         SendMessage(DM_SETDEFID, m_nDefCtlId);
         CWindow(GetDlgItem(m_nDefCtlId)).SetFocus();
      }
      // Get icons...
      m_iconMain = (HICON) ((m_cfg.dwFlags & TDF_USE_HICON_MAIN) != 0 ? m_cfg.hMainIcon : _LoadIcon(m_cfg.pszMainIcon, m_Metrics.cxyLargeIcon));
      m_iconFooter = (HICON) ((m_cfg.dwFlags & TDF_USE_HICON_FOOTER) != 0 ? m_cfg.hFooterIcon : _LoadIcon(m_cfg.pszFooterIcon, m_Metrics.cxySmallIcon));
      m_iconArrowHot = _LoadIcon(MAKEINTRESOURCEW(IDI_TASKDLG_ARROW_HOT), 20);
      m_iconArrowNormal = _LoadIcon(MAKEINTRESOURCEW(IDI_TASKDLG_ARROW_NORMAL), 20);
      m_iconChevronLess = _LoadIcon(MAKEINTRESOURCEW(IDI_TASKDLG_CHEVRON_LESS), 20);
      m_iconChevronMore = _LoadIcon(MAKEINTRESOURCEW(IDI_TASKDLG_CHEVRON_MORE), 20);
      if( !m_iconMain.IsNull() && GetParent() == NULL ) SetIcon(m_iconMain, FALSE);
      // Play that funky music...
      _PlaySound();
      // Ready to accept user input...
      m_bCreated = true;
      _DoCallback(TDN_DIALOG_CONSTRUCTED);
      _DoCallback(m_bNavigated ? TDN_NAVIGATED : TDN_CREATED);
      m_bNavigated = false;
      return 0;
   }

   LRESULT OnWmDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
   {
      KillTimer(TIMERID_TIGGER);
      KillTimer(TIMERID_HOVERLINK);
      _DoCallback(TDN_DESTROYED);
      m_bVerificationResult = IsDlgButtonChecked(IDC_TASKDLG_VERIFYBUTTON);
      bHandled = FALSE;
      return 0;
   }

   LRESULT OnWmHelp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      _DoCallback(TDN_HELP);
      return 0;
   }

   LRESULT OnWmTimer(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
   {
      if( wParam == TIMERID_TIGGER ) {
         BOOL bReset = (BOOL) _DoCallback(TDN_TIMER, ::GetTickCount() - m_dwTick);
         if( bReset ) m_dwTick = ::GetTickCount();
      }
      if( wParam == TIMERID_HOVERLINK ) {
         POINT pt = { 0 };
         ::GetCursorPos(&pt);
         ScreenToClient(&pt);
         UINT nHoverId = 0;
         HWND hWnd = ::ChildWindowFromPoint(m_hWnd, pt);
         if( hWnd != NULL ) {
            UINT nCtlId = ::GetDlgCtrlID(hWnd);
            if( nCtlId >= IDC_TASKDLG_CUSTOMBUTTON_FIRST && nCtlId <= IDC_TASKDLG_CUSTOMBUTTON_LAST ) nHoverId = nCtlId;
         }
         if( m_nHoverId != nHoverId ) {
            if( m_nHoverId != 0 ) CWindow(GetDlgItem(m_nHoverId)).Invalidate();
            m_nHoverId = nHoverId;
            if( m_nHoverId != 0 ) CWindow(GetDlgItem(m_nHoverId)).Invalidate();
         }
      }
      bHandled = FALSE;
      return 0;
   }

   LRESULT OnWmEraseBkgnd(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      CClientDC dc = m_hWnd;
      RECT rcClient = { 0 };
      GetClientRect(&rcClient);
      RECT rcTop = { 0, 0, rcClient.right, m_Metrics.iButtonLinePos };
      RECT rcBottom = { 0, m_Metrics.iButtonLinePos, rcClient.right, rcClient.bottom };
      dc.FillSolidRect(&rcTop, m_Metrics.clrBkTop);
      dc.FillSolidRect(&rcBottom, m_Metrics.clrBkBottom);
      RECT rcButtonLine = { 0, rcBottom.top, rcClient.right, rcBottom.top + 1 };
      dc.FillSolidRect(&rcButtonLine, m_Metrics.clrButtonDivider);
      if( m_Metrics.iFooterLinePos > 0 ) {
         RECT rcFooterLine1 = { 0, m_Metrics.iFooterLinePos, rcClient.right, m_Metrics.iFooterLinePos + 1 };
         dc.FillSolidRect(&rcFooterLine1, m_Metrics.clrDividerDark);
         RECT rcFooterLine2 = { 0, m_Metrics.iFooterLinePos + 1, rcClient.right, m_Metrics.iFooterLinePos + 2 };
         dc.FillSolidRect(&rcFooterLine2, m_Metrics.clrDividerLight);
      }
      if( m_Metrics.iExpandedLinePos > 0 ) {
         RECT rcExpandoLine1 = { 0, m_Metrics.iExpandedLinePos, rcClient.right, m_Metrics.iExpandedLinePos + 1 };
         dc.FillSolidRect(&rcExpandoLine1, m_Metrics.clrDividerDark);
         RECT rcExpandoLine2 = { 0, m_Metrics.iExpandedLinePos + 1, rcClient.right, m_Metrics.iExpandedLinePos + 2 };
         dc.FillSolidRect(&rcExpandoLine2, m_Metrics.clrDividerLight);
      }
      return 1;
   }

   LRESULT OnWmCtlColor(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      LRESULT lRes = DefWindowProc();
      UINT nCtlID = ::GetDlgCtrlID((HWND) lParam);
      CDCHandle dc = (HDC) wParam;
      switch( nCtlID ) {
      case IDC_TASKDLG_INSTRUCTIONSICON:
         dc.SetBkMode(TRANSPARENT);
         dc.SetBkColor(m_Metrics.clrBkTop);
         lRes = (LRESULT) (HBRUSH) m_brWhite;
         break;
      case IDC_TASKDLG_INSTRUCTIONSTEXT:
         dc.SetTextColor(m_Metrics.clrTitleText);
         dc.SetBkMode(TRANSPARENT);
         dc.SetBkColor(m_Metrics.clrBkTop);
         lRes = (LRESULT) (HBRUSH) m_brWhite;
         break;
      case IDC_TASKDLG_CONTENTTEXT:
         dc.SetBkMode(TRANSPARENT);
         dc.SetBkColor(m_Metrics.clrBkTop);
         lRes = (LRESULT) (HBRUSH) m_brWhite;
         break;
      case IDC_TASKDLG_FOOTERICON:
      case IDC_TASKDLG_FOOTERTEXT:
      case IDC_TASKDLG_EXPANDERICON:
      case IDC_TASKDLG_EXPANDERTEXT:
      case IDC_TASKDLG_VERIFYBUTTON:
         dc.SetBkMode(TRANSPARENT);
         dc.SetBkColor(m_Metrics.clrBkBottom);
         lRes = (LRESULT) (HBRUSH) m_brGrey;
         break;
      case IDC_TASKDLG_OK:
      case IDC_TASKDLG_NO:
      case IDC_TASKDLG_YES:
      case IDC_TASKDLG_RETRY:
      case IDC_TASKDLG_CLOSE:
      case IDC_TASKDLG_CANCEL:
         dc.SetBkMode(TRANSPARENT);
         dc.SetBkColor(m_Metrics.clrBkBottom);
         lRes = (LRESULT) (HBRUSH) m_brGrey;
         break;
      default:
         {
            COLORREF clrBack;
            if( nCtlID >= IDC_TASKDLG_RADIOBUTTON_FIRST && nCtlID <= IDC_TASKDLG_RADIOBUTTON_LAST ) {
               clrBack = m_Metrics.clrBkTop;
               lRes = (LRESULT) (HBRUSH) m_brWhite;
            }
            else if( nCtlID >= IDC_TASKDLG_CUSTOMBUTTON_FIRST && nCtlID <= IDC_TASKDLG_CUSTOMBUTTON_LAST && (m_cfg.dwFlags & (TDF_USE_COMMAND_LINKS|TDF_USE_COMMAND_LINKS_NO_ICON)) != 0 ) {
               clrBack = m_Metrics.clrBkTop;
               lRes = (LRESULT) (HBRUSH) m_brWhite;
            }
            else if( nCtlID == IDC_TASKDLG_EXTRATEXT && (m_cfg.dwFlags & TDF_EXPAND_FOOTER_AREA) == 0 ) {
               clrBack = m_Metrics.clrBkTop;
               lRes = (LRESULT) (HBRUSH) m_brWhite;
            }
            else {
               clrBack = m_Metrics.clrBkBottom;
               lRes = (LRESULT) (HBRUSH) m_brGrey;
            }
            dc.SetBkMode(TRANSPARENT);
            dc.SetBkColor(clrBack);
         }
      }
      return lRes;
   }

   LRESULT OnSysCommand(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
   {
      if( wParam == SC_CLOSE ) {
         if( (m_cfg.dwFlags & TDF_ALLOW_DIALOG_CANCELLATION) == 0 ) return 0;         
      }
      bHandled = FALSE;
      return 0;
   }

   LRESULT OnWmDrawItem(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
   {
      LPDRAWITEMSTRUCT lpDIS = (LPDRAWITEMSTRUCT) lParam;
      if( m_bHasCustomLinks && lpDIS->CtlID >= IDC_TASKDLG_CUSTOMBUTTON_FIRST && lpDIS->CtlID <= IDC_TASKDLG_CUSTOMBUTTON_LAST ) {
         _CustomDrawCommandLink(lpDIS);
         return 0;
      }
      if( lpDIS->CtlID == IDC_TASKDLG_INSTRUCTIONSICON ) {
         _CustomDrawIcon(lpDIS, m_Metrics.clrBkTop, m_iconMain, m_Metrics.cxyLargeIcon);
         return 0;
      }
      if( lpDIS->CtlID == IDC_TASKDLG_FOOTERICON ) {
         _CustomDrawIcon(lpDIS, m_Metrics.clrBkBottom, m_iconFooter, m_Metrics.cxySmallIcon);
         return 0;
      }
      if( lpDIS->CtlID == IDC_TASKDLG_EXPANDERICON ) {
         _CustomDrawIcon(lpDIS, m_Metrics.clrBkBottom, m_bExpanded ? m_iconChevronLess : m_iconChevronMore, m_Metrics.cxyExpanderIcon);
         return 0;
      }
      bHandled = FALSE;
      return 0;
   }

   // TaskDialog API messages

   LRESULT OnMsgClickButton(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      ClickButton(wParam);
      return 0;
   }

   LRESULT OnMsgClickRadioButton(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      ClickRadioButton(wParam);
      return 0;
   }

   LRESULT OnMsgClickVerification(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      ClickVerification((BOOL) wParam, (BOOL) lParam);
      return 0;
   }

   LRESULT OnMsgEnableButton(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      return (LRESULT) EnableButton(wParam, (BOOL) lParam);
   }

   LRESULT OnMsgEnableRadioButton(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      return (LRESULT) EnableRadioButton(wParam, (BOOL) lParam);
   }

   LRESULT OnMsgSetElementText(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      return (LRESULT) SetElementText((TASKDIALOG_ELEMENTS) wParam, (LPCWSTR) lParam);
   }

   LRESULT OnMsgUpdateElementText(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      return (LRESULT) UpdateElementText((TASKDIALOG_ELEMENTS) wParam, (LPCWSTR) lParam);
   }

   LRESULT OnMsgSetProgressBarPos(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      SetProgressBarPos(wParam);
      return 0;
   }

   LRESULT OnMsgSetProgressBarRange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
   {
      SetProgressBarRange(LOWORD(lParam), HIWORD(lParam));
      return 0;
   }

   LRESULT OnMsgSetMarqueeProgressBar(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      SetMarqueeProgressBar((BOOL) wParam);
      return 0;
   }

   LRESULT OnMsgSetProgressBarState(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      return (LRESULT) SetProgressBarState((int) wParam);
   }

   LRESULT OnMsgSetProgressBarMarquee(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      return (LRESULT) SetProgressBarMarquee(wParam, lParam);
   }

   // Command message handlers

   LRESULT OnMsgCommonButtonClick(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
   {
      UINT nBtnID = wID;
      if( _DoCallback(TDN_BUTTON_CLICKED, nBtnID) == S_FALSE ) return 0;
      m_iButtonResult = (int) nBtnID;
      EndDialog(wID);
      return 0;
   }

   LRESULT OnMsgCustomButtonClick(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
   {
      UINT nBtnID = m_cfg.pButtons[wID - IDC_TASKDLG_CUSTOMBUTTON_FIRST].nButtonID;
      if( _DoCallback(TDN_BUTTON_CLICKED, nBtnID) == S_FALSE ) return 0;
      m_iButtonResult = (int) nBtnID;
      EndDialog(wID);
      return 0;
   }

   LRESULT OnMsgExpandoClick(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
   {
      m_bExpanded = !m_bExpanded;
      _DoCallback(TDN_EXPANDO_BUTTON_CLICKED, m_bExpanded);
      _DoExpandCollapse();
      return 0;
   }

   LRESULT OnMsgVerificationClick(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
   {
      _DoCallback(TDN_VERIFICATION_CLICKED, IsDlgButtonChecked(IDC_TASKDLG_VERIFYBUTTON) == BST_CHECKED);
      return 0;
   }

   LRESULT OnMsgRadioClick(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
   {
      m_iRadioResult = (int) m_cfg.pRadioButtons[wID - IDC_TASKDLG_RADIOBUTTON_FIRST].nButtonID;
      _DoCallback(TDN_RADIO_BUTTON_CLICKED, (WPARAM) m_iRadioResult);
      return 0;
   }

   LRESULT OnMsgHyperlinkClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      NMLINK* pLink = (NMLINK*) pnmh;
      _DoCallback(TDN_HYPERLINK_CLICKED, 0, (LPARAM) (LPCWSTR) (pLink->item.szID[0] != '\0' ? pLink->item.szID : pLink->item.szUrl));
      return 0;
   }

   // Implementation

   void _Reset()
   {
      m_iRadioResult = 0;
      m_iButtonResult = IDCANCEL;
      m_bExpanded = true;
      m_nHoverId = 0;
      m_dwTick = ::GetTickCount();
   }

   SIZE _LayoutControls(int cxMaxDialog, bool bCreateControls)
   {
      SIZE sizeEmpty = { 0 };
      LPTSTR pstrBuffer = (LPTSTR) malloc(MAX_TEXT_LENGTH * sizeof(TCHAR));
      if( pstrBuffer == NULL ) return sizeEmpty;

      LONG cxDialog, cxExpander, cxMaxText;
      SIZE sizeTitle, sizeText, sizeTemp;
      LONG cyExpander;
      DWORD dwStyle, dwExStyle;
      UINT n;

      bool bIsCommCtrl6 = RunTimeHelper::IsCommCtrl6();
      bool bIsVista = RunTimeHelper::IsVista();

      LONG ypos = m_Metrics.sizeDialogPadding.cy;

      cxDialog = 0;
      m_nDefCtlId = 0;

      // Main icon

      LONG xpos = m_Metrics.sizeDialogPadding.cx;

      LONG cyIcon = 0;
      if( m_cfg.pszMainIcon != NULL || (m_cfg.dwFlags & TDF_USE_HICON_MAIN) != 0 ) {
         RECT rcIcon = { xpos, ypos, xpos + m_Metrics.cxyLargeIcon, ypos + m_Metrics.cxyLargeIcon };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_OWNERDRAW;
            dwExStyle = 0;
            _AddControl(CStatic::GetWndClassName(), IDC_TASKDLG_INSTRUCTIONSICON, rcIcon, dwStyle, dwExStyle, _T(""));
         }
         xpos += (rcIcon.right - rcIcon.left) + m_Metrics.cxLargeIconGap;
         cyIcon = m_Metrics.cxyLargeIcon;
      }

      // Main Instructions text

      if( m_cfg.pszMainInstruction != NULL ) {
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         if( cxMaxDialog == WIDTH_PROBE ) cxMaxText = m_Metrics.cxBestMainInstruction;
         sizeText = _GetTextSize(m_cfg.pszMainInstruction, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK | DT_NOPREFIX, m_fontTitle, cxMaxText);
         if( sizeText.cy < m_Metrics.cxyLargeIcon ) sizeText.cy = m_Metrics.cxyLargeIcon;
         RECT rcItem = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
            dwExStyle = 0;
            _AddControl(CStatic::GetWndClassName(), IDC_TASKDLG_INSTRUCTIONSTEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.bottom - rcItem.top < cyIcon ) rcItem.bottom = rcItem.top + cyIcon;
         ypos += (rcItem.bottom - rcItem.top) + m_Metrics.cyInstructionsGap;
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
      }

      // Content text

      if( m_cfg.pszContent != NULL ) {
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         if( cxMaxDialog == WIDTH_PROBE ) cxMaxText = m_Metrics.cxBestContent;
         sizeText = _GetTextSizeHREF(m_cfg.pszContent, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK | DT_NOPREFIX, m_fontText, cxMaxText);
         RECT rcItem = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
            dwExStyle = 0;
            LPCTSTR pstrClassName = bIsCommCtrl6 ? CLinkCtrl::GetWndClassName() : CStatic::GetWndClassName();
            if( !bIsCommCtrl6 ) _RemoveHREF(pstrBuffer);
            _AddControl(pstrClassName, IDC_TASKDLG_CONTENTTEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( cyIcon > 0 && m_cfg.pszMainInstruction == NULL && rcItem.bottom - rcItem.top < cyIcon ) rcItem.bottom = rcItem.top + cyIcon;
         ypos += (rcItem.bottom - rcItem.top);
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
      }

      ypos += (m_Metrics.cyContentGap / 2);

      // Expando in Content area

      if( m_cfg.pszExpandedInformation != NULL && ((m_cfg.dwFlags & TDF_EXPAND_FOOTER_AREA) == 0) ) {
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         sizeText = _GetTextSizeHREF(m_cfg.pszExpandedInformation, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK | DT_NOPREFIX, m_fontText, cxMaxText);
         RECT rcItem = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
            dwExStyle = 0;
            LPCTSTR pstrClassName = bIsCommCtrl6 ? CLinkCtrl::GetWndClassName() : CStatic::GetWndClassName();
            if( !bIsCommCtrl6 ) _RemoveHREF(pstrBuffer);
            _AddControl(pstrClassName, IDC_TASKDLG_EXTRATEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
         ypos += (rcItem.bottom - rcItem.top) + m_Metrics.cyButtonLineGap;
         _AlignDLU(ypos, HTBOTTOM);
      }

      // Progress Bar

      if( (m_cfg.dwFlags & (TDF_SHOW_PROGRESS_BAR|TDF_SHOW_MARQUEE_PROGRESS_BAR)) != 0 ) {
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         if( cxMaxDialog == WIDTH_PROBE ) cxMaxText = m_Metrics.cxBestProgressBar;
         RECT rcItem = { xpos, ypos, xpos + cxMaxText, ypos + m_Metrics.cyProgressBar };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | PBS_SMOOTH;
            dwExStyle = 0;
            _AddControl(CProgressBarCtrl::GetWndClassName(), IDC_TASKDLG_PROGRESSBAR, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
         ypos += (rcItem.bottom - rcItem.top) + m_Metrics.cyInstructionsGap;
      }

      // Radio buttons

      xpos += m_Metrics.cxRadioIndent;

      if( m_cfg.cRadioButtons > 0 ) {
         for( n = 0; n < m_cfg.cRadioButtons; n++ ) {
            cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx - m_Metrics.sizeRadioButton.cx;
            if( cxMaxDialog == WIDTH_PROBE ) cxMaxText = m_Metrics.cxBestRadioButton;
            sizeText = _GetTextSize(m_cfg.pRadioButtons[n].pszButtonText, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, cxMaxText);
            sizeText.cx += m_Metrics.sizeRadioButton.cx;
            if( sizeText.cy < m_Metrics.sizeRadioButton.cy ) sizeText.cy = m_Metrics.sizeRadioButton.cy;
            RECT rcButton = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
            if( bCreateControls ) {
               dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_AUTORADIOBUTTON | BS_TOP | BS_MULTILINE;
               dwExStyle = 0;
               if( n == 0 ) dwStyle |= WS_GROUP;
               _AddControl(CButton::GetWndClassName(), (WORD) (IDC_TASKDLG_RADIOBUTTON_FIRST + n), rcButton, dwStyle, dwExStyle, pstrBuffer);
            }
            if( rcButton.right > cxDialog ) cxDialog = rcButton.right;
            ypos += sizeText.cy + m_Metrics.cyRadioGap;
         }
         ypos += (m_Metrics.cyContentGap / 2) - m_Metrics.cyRadioGap;
      }

      // Command Links

      m_bHasCustomLinks = false;
      if( m_cfg.cButtons > 0 && (m_cfg.dwFlags & (TDF_USE_COMMAND_LINKS|TDF_USE_COMMAND_LINKS_NO_ICON)) != 0 ) {
         _AlignDLU(ypos, HTBOTTOM);
         if( m_cfg.cRadioButtons > 0 ) ypos += (m_Metrics.cyContentGap / 2);
         for( n = 0; n < m_cfg.cButtons; n++ ) {
            _LoadString(m_cfg.pButtons[n].pszButtonText, pstrBuffer, MAX_TEXT_LENGTH);
            LPCTSTR pstrTitle = NULL, pstrText = NULL;
            SIZE_T cchTitle = 0, cchText = 0;
            _SplitCommandText(pstrBuffer, pstrTitle, cchTitle, pstrText, cchText);
            LONG cxPadding = (m_Metrics.sizeLinkPadding.cx * 2);
            if( (m_cfg.dwFlags & TDF_USE_COMMAND_LINKS_NO_ICON) == 0) cxPadding += m_Metrics.cxyArrowIcon + m_Metrics.cxSmallIconGap;
            cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx - cxPadding;
            if( cxMaxDialog == WIDTH_PROBE ) cxMaxText = m_Metrics.cxBestCommandLink - cxPadding;
            sizeTitle = _GetTextSize(pstrTitle, cchTitle, DT_WORDBREAK, m_fontTitle, cxMaxText);
            sizeText = _GetTextSize(pstrText, cchText, DT_NOPREFIX | DT_WORDBREAK, m_fontText, cxMaxText);
            if( sizeTitle.cx > sizeText.cx ) sizeText.cx = sizeTitle.cx;
            sizeText.cx += cxPadding;
            if( sizeText.cx < m_Metrics.cxMinCommandLink ) sizeText.cx = m_Metrics.cxMinCommandLink;
            RECT rcButton = { xpos, ypos, cxMaxDialog - m_Metrics.sizeDialogPadding.cx, ypos + sizeTitle.cy + sizeText.cy + (m_Metrics.sizeLinkPadding.cy * 2) };
            _AlignDLU(rcButton.bottom, HTBOTTOM);
            if( bCreateControls ) {
               dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_TOP | BS_MULTILINE;
               dwExStyle = 0;
               if( n == 0 ) dwStyle |= WS_GROUP;
               if( bIsVista ) {
#ifndef BS_COMMANDLINK
                  const DWORD BS_COMMANDLINK = 0x0000000E;
                  const DWORD BS_DEFCOMMANDLINK = 0x0000000F;
#endif // BS_COMMANDLINK
                  if( m_cfg.pButtons[n].nButtonID == m_cfg.nDefaultButton ) {
                     dwStyle |= BS_DEFCOMMANDLINK;
                     m_nDefCtlId = (UINT) -1;
                  }
                  else {
                     dwStyle |= BS_COMMANDLINK;
                  }
               }
               else {
                  m_bHasCustomLinks = true;
                  if( m_cfg.pButtons[n].nButtonID == m_cfg.nDefaultButton ) m_nDefCtlId = IDC_TASKDLG_CUSTOMBUTTON_FIRST + n;
                  dwStyle |= BS_OWNERDRAW;
               }
               _AddControl(CButton::GetWndClassName(), (WORD) (IDC_TASKDLG_CUSTOMBUTTON_FIRST + n), rcButton, dwStyle, dwExStyle, pstrBuffer);
            }
            if( xpos + sizeText.cx > cxDialog ) cxDialog = xpos + sizeText.cx;
            ypos += (rcButton.bottom - rcButton.top);
         }
         ypos += (m_Metrics.cyContentGap / 2);
      }

     
      // Expander area

      m_Metrics.iButtonLinePos = ypos;
      ypos += m_Metrics.cyButtonLineGap;

      xpos = m_Metrics.sizeDialogPadding.cx;
      cyExpander = 0;
      cxExpander = 0;
      if( m_cfg.pszExpandedInformation != NULL ) {
         RECT rcIcon = { xpos, ypos, xpos + m_Metrics.cxyExpanderIcon, ypos + m_Metrics.cxyExpanderIcon };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_OWNERDRAW | SS_NOTIFY;
            dwExStyle = 0;         
            _AddControl(CStatic::GetWndClassName(), IDC_TASKDLG_EXPANDERICON, rcIcon, dwStyle, dwExStyle, _T(""));
         }
         LONG xoffset = m_Metrics.cxyExpanderIcon + m_Metrics.cxSmallIconGap;
         sizeTemp = _GetTextSize(m_cfg.pszCollapsedControlText, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, m_Metrics.cxMaxVerification);
         sizeText = _GetTextSize(m_cfg.pszExpandedControlText, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, m_Metrics.cxMaxVerification);
         if( sizeTemp.cx > sizeText.cx ) sizeText.cx = sizeTemp.cx;
         if( sizeTemp.cy > sizeText.cy ) sizeText.cy = sizeTemp.cy;
         sizeText.cx += xoffset;
         LONG yoffset = 0;
         if( sizeText.cy < m_Metrics.cxyExpanderIcon ) yoffset = (m_Metrics.cxyExpanderIcon / 2) - (sizeText.cy / 2);
         RECT rcItem = { xpos + xoffset, ypos + yoffset, xpos + xoffset + sizeText.cx, ypos + yoffset + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX | SS_NOTIFY;
            dwExStyle = 0;
            _AddControl(CStatic::GetWndClassName(), IDC_TASKDLG_EXPANDERTEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.right > cxExpander ) cxExpander = rcItem.right;
         cyExpander += (rcItem.bottom - rcItem.top) + (yoffset * 2);
         if( m_cfg.pszVerificationText != NULL ) cyExpander += m_Metrics.cyExpanderGap;
      }

      // Verification text

      xpos = m_Metrics.sizeDialogPadding.cx;
      if( m_cfg.pszVerificationText != NULL ) {
         sizeText = _GetTextSize(m_cfg.pszVerificationText, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, m_Metrics.cxMaxVerification);
         sizeText.cx += m_Metrics.sizeRadioButton.cx;
         if( sizeText.cy < m_Metrics.sizeRadioButton.cy ) sizeText.cy = m_Metrics.sizeRadioButton.cy;
         if( sizeText.cy < m_Metrics.sizeButtons.cy && cyExpander == 0 ) cyExpander = (m_Metrics.sizeButtons.cy / 2) - (sizeText.cy / 2);
         const LONG cxIndent = 2;
         RECT rcButton = { xpos + cxIndent, ypos + cyExpander, xpos + cxIndent + sizeText.cx, ypos + cyExpander + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_AUTOCHECKBOX | BS_TOP | BS_LEFT | BS_MULTILINE;
            dwExStyle = 0;
            _AddControl(CButton::GetWndClassName(), IDC_TASKDLG_VERIFYBUTTON, rcButton, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcButton.right > cxExpander ) cxExpander = rcButton.right;
         cyExpander += (rcButton.bottom - rcButton.top);
      }

      // Buttons

      xpos = cxExpander + m_Metrics.cxButtonsDivider;     
      if( m_Metrics.sizeButtons.cx > 0 ) xpos += cxMaxDialog - m_Metrics.sizeDialogPadding.cx - xpos - m_Metrics.sizeButtons.cx;

      m_Metrics.sizeButtons.cx = 0;
      m_Metrics.sizeButtons.cy = 0;

      if( m_cfg.cButtons > 0 && (m_cfg.dwFlags & (TDF_USE_COMMAND_LINKS|TDF_USE_COMMAND_LINKS_NO_ICON)) == 0 ) {
         for( n = 0; n < m_cfg.cButtons; n++ ) {
            sizeText = _GetTextSize(m_cfg.pButtons[n].pszButtonText, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, 9999);
            if( sizeText.cx < m_Metrics.cxMinButton ) sizeText.cx = m_Metrics.cxMinButton;
            RECT rcButton = { xpos, ypos, xpos + sizeText.cx + (m_Metrics.sizeButtonPadding.cx * 2), ypos + sizeText.cy + (m_Metrics.sizeButtonPadding.cy * 2) };
            _AlignDLU(rcButton.right, HTRIGHT);
            if( bCreateControls ) {
               dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON | BS_MULTILINE;
               dwExStyle = 0;
               _AddControl(CButton::GetWndClassName(), (WORD) (IDC_TASKDLG_CUSTOMBUTTON_FIRST + n), rcButton, dwStyle, dwExStyle, pstrBuffer);
            }
            xpos = rcButton.right + m_Metrics.cxButtonGap;
            m_Metrics.sizeButtons.cx += (rcButton.right - rcButton.left) + m_Metrics.cxButtonGap;
            if( m_nDefCtlId == 0 ) m_nDefCtlId = IDC_TASKDLG_CUSTOMBUTTON_FIRST + n;
            if( m_cfg.nDefaultButton == m_cfg.pButtons[n].nButtonID ) m_nDefCtlId = IDC_TASKDLG_CUSTOMBUTTON_FIRST + n;
            if( rcButton.bottom - rcButton.top > m_Metrics.sizeButtons.cy ) m_Metrics.sizeButtons.cy = (rcButton.bottom - rcButton.top);
         }
      }

      if( m_cfg.dwCommonButtons != 0 ) {
         int buttons[] = { 
            TDCBF_OK_BUTTON,     IDOK,     IDS_TASKDLG_OK,
            TDCBF_YES_BUTTON,    IDYES,    IDS_TASKDLG_YES,
            TDCBF_NO_BUTTON,     IDNO,     IDS_TASKDLG_NO,
            TDCBF_RETRY_BUTTON,  IDRETRY,  IDS_TASKDLG_RETRY,
            TDCBF_CANCEL_BUTTON, IDCANCEL, IDS_TASKDLG_CANCEL,
            TDCBF_CLOSE_BUTTON,  IDCLOSE,  IDS_TASKDLG_CLOSE,
         };
         for( n = 0; n < sizeof(buttons) / sizeof(buttons[0]); n += 3 ) {
            if( (m_cfg.dwCommonButtons & buttons[n]) == 0 ) continue;
            sizeText = _GetTextSize(MAKEINTRESOURCEW(buttons[n + 2]), pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK, m_fontText, 9999);
            if( sizeText.cx < m_Metrics.cxMinButton ) sizeText.cx = m_Metrics.cxMinButton;
            RECT rcButton = { xpos, ypos, xpos + sizeText.cx + (m_Metrics.sizeButtonPadding.cx * 2), ypos + sizeText.cy + (m_Metrics.sizeButtonPadding.cy * 2) };
            _AlignDLU(rcButton.right, HTRIGHT);
            if( bCreateControls ) {
               dwStyle = WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON | BS_MULTILINE;
               dwExStyle = 0;
               _AddControl(CButton::GetWndClassName(), (WORD) buttons[n + 1], rcButton, dwStyle, dwExStyle, pstrBuffer);
            }
            xpos = rcButton.right + m_Metrics.cxButtonGap;
            m_Metrics.sizeButtons.cx += (rcButton.right - rcButton.left) + m_Metrics.cxButtonGap;
            if( m_nDefCtlId == 0 ) m_nDefCtlId = (UINT) buttons[n + 1];
            if( m_cfg.nDefaultButton == buttons[n + 1] ) m_nDefCtlId = (UINT) buttons[n + 1];
            if( rcButton.bottom - rcButton.top > m_Metrics.sizeButtons.cy ) m_Metrics.sizeButtons.cy = (rcButton.bottom - rcButton.top);
         }
      }

      if( m_Metrics.sizeButtons.cx > 0 ) {
         m_Metrics.sizeButtons.cx -= m_Metrics.cxButtonGap;
         xpos -= m_Metrics.cxButtonGap;
      }
      if( xpos > cxDialog ) cxDialog = xpos;

      if( cyExpander < m_Metrics.sizeButtons.cy ) cyExpander = m_Metrics.sizeButtons.cy;
      ypos += cyExpander;

      // Footer text

      xpos = m_Metrics.sizeDialogPadding.cx;
      if( m_cfg.pszFooter != NULL ) {
         ypos += m_Metrics.cyButtonLineGap;
         m_Metrics.iFooterLinePos = ypos;
         ypos += m_Metrics.cyButtonLineGap;
         if( m_cfg.pszFooterIcon != NULL || (m_cfg.dwFlags & TDF_USE_HICON_FOOTER) != 0 ) {
            RECT rcItem = { xpos, ypos, xpos + m_Metrics.cxySmallIcon, ypos + m_Metrics.cxySmallIcon };
            if( bCreateControls ) {
               dwStyle = WS_CHILD | WS_VISIBLE | SS_OWNERDRAW;
               dwExStyle = 0;         
               _AddControl(CStatic::GetWndClassName(), IDC_TASKDLG_FOOTERICON, rcItem, dwStyle, dwExStyle, _T(""));
            }
            xpos += (rcItem.right - rcItem.left) + m_Metrics.cxSmallIconGap;
         }
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         sizeText = _GetTextSizeHREF(m_cfg.pszFooter, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK | DT_NOPREFIX, m_fontText, cxMaxText);
         if( sizeText.cy < m_Metrics.cxySmallIcon ) sizeText.cy = m_Metrics.cxySmallIcon;
         RECT rcItem = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
            dwExStyle = 0;
            LPCTSTR pstrClassName = bIsCommCtrl6 ? CLinkCtrl::GetWndClassName() : CStatic::GetWndClassName();
            if( !bIsCommCtrl6 ) _RemoveHREF(pstrBuffer);
            _AddControl(pstrClassName, IDC_TASKDLG_FOOTERTEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
         ypos += (rcItem.bottom - rcItem.top);
      }

      // Expando in Footer area

      xpos = m_Metrics.sizeDialogPadding.cx;
      if( m_cfg.pszExpandedInformation != NULL && ((m_cfg.dwFlags & TDF_EXPAND_FOOTER_AREA) != 0) ) {
         ypos += m_Metrics.cyButtonLineGap;
         m_Metrics.iExpandedLinePos = ypos;
         ypos += m_Metrics.cyButtonLineGap;
         cxMaxText = cxMaxDialog - xpos - m_Metrics.sizeDialogPadding.cx;
         sizeText = _GetTextSizeHREF(m_cfg.pszExpandedInformation, pstrBuffer, MAX_TEXT_LENGTH, DT_WORDBREAK | DT_NOPREFIX, m_fontText, cxMaxText);
         RECT rcItem = { xpos, ypos, xpos + sizeText.cx, ypos + sizeText.cy };
         if( bCreateControls ) {
            dwStyle = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
            dwExStyle = 0;
            LPCTSTR pstrClassName = bIsCommCtrl6 ? CLinkCtrl::GetWndClassName() : CStatic::GetWndClassName();
            if( !bIsCommCtrl6 ) _RemoveHREF(pstrBuffer);
            _AddControl(pstrClassName, IDC_TASKDLG_EXTRATEXT, rcItem, dwStyle, dwExStyle, pstrBuffer);
         }
         if( rcItem.right > cxDialog ) cxDialog = rcItem.right;
         ypos += (rcItem.bottom - rcItem.top);
      }

      cxDialog += m_Metrics.sizeDialogPadding.cx;
      ypos += m_Metrics.sizeDialogPadding.cy;

      free(pstrBuffer);

      SIZE sizeDialog = { cxDialog, ypos };
      return sizeDialog;
   }

   void _CustomDrawIcon(LPDRAWITEMSTRUCT lpDIS, COLORREF clrBack, HICON hIcon, int cxyIcon)
   {
      CDCHandle dc = lpDIS->hDC;
      RECT rc = lpDIS->rcItem;
      dc.FillSolidRect(&rc, clrBack);
      dc.DrawIconEx(rc.left, rc.top, hIcon, cxyIcon, cxyIcon);
   }

   void _CustomDrawCommandLink(LPDRAWITEMSTRUCT lpDIS)
   {
      CButton ctrl = lpDIS->hwndItem;
      CDCHandle dc = lpDIS->hDC;
      RECT rc = lpDIS->rcItem;
      HBRUSH hOldBrush = dc.GetCurrentBrush();
      HFONT hOldFont = dc.GetCurrentFont();
      HPEN hOldPen = dc.GetCurrentPen();

      if( lpDIS->CtlID == m_nHoverId ) {
         dc.DrawFrameControl(&rc, DFC_BUTTON, DFCS_BUTTONPUSH);
         RECT rcInteriour = rc;
         ::InflateRect(&rcInteriour, -2, -2);
         dc.FillSolidRect(&rcInteriour, m_Metrics.clrBkTop);
      }
      else if( (lpDIS->itemAction & ODA_DRAWENTIRE) != 0 ) {
         dc.FillSolidRect(&rc, m_Metrics.clrBkTop);
      }

      COLORREF clrBorder = CLR_INVALID;
      if( m_nDefCtlId == lpDIS->CtlID ) clrBorder = m_Metrics.clrCmdLinkSelect;
      if( (lpDIS->itemState & ODS_SELECTED) != 0 ) clrBorder = ::GetSysColor(COLOR_3DSHADOW);
      if( clrBorder != CLR_INVALID ) {
         POINT ptArc = { 6, 6 };
         CPen penBorder;
         penBorder.CreatePen(PS_SOLID, 1, clrBorder);
         dc.SelectPen(penBorder);
         dc.SelectBrush((HBRUSH) ::GetStockObject(HOLLOW_BRUSH));
         dc.RoundRect(&rc, ptArc);
      }

      ::InflateRect(&rc, -m_Metrics.sizeLinkPadding.cx, -m_Metrics.sizeLinkPadding.cy);

      if( (m_cfg.dwFlags & TDF_USE_COMMAND_LINKS_NO_ICON) == 0 ) {
         dc.DrawIconEx(rc.left, rc.top + 2, lpDIS->CtlID == m_nHoverId ? m_iconArrowHot : m_iconArrowNormal, 20, 20);
         rc.left += m_Metrics.cxyArrowIcon + m_Metrics.cxSmallIconGap;
      }

      LONG cx = (rc.right - rc.left);
      TCHAR szWindowText[300] = { 0 };
      ctrl.GetWindowText(szWindowText, sizeof(szWindowText) / sizeof(TCHAR));
      LPCTSTR pstrTitle = NULL, pstrText = NULL;
      SIZE_T cchTitle = 0, cchText = 0;
      _SplitCommandText(szWindowText, pstrTitle, cchTitle, pstrText, cchText);
      SIZE sizeTitle = _GetTextSize(pstrTitle, cchTitle, DT_WORDBREAK, m_fontTitle, cx);

      RECT rcTitle = { rc.left, rc.top, rc.right, rc.bottom };
      dc.SetTextColor(m_Metrics.clrTitleText);
      dc.SetBkMode(TRANSPARENT);
      dc.SelectFont(m_fontTitle);
      dc.DrawText(pstrTitle, cchTitle, &rcTitle, DT_WORDBREAK);
      RECT rcText = { rc.left, rc.top + sizeTitle.cy + 1, rc.right, rc.bottom };
      dc.SelectFont(m_fontText);
      dc.DrawText(pstrText, cchText, &rcText, DT_NOPREFIX | DT_WORDBREAK);

      dc.SelectPen(hOldPen);
      dc.SelectFont(hOldFont);
      dc.SelectBrush(hOldBrush);
   }

   void _PlaySound()
   {
      if( m_cfg.pszMainIcon == TD_ERROR_ICON ) ::MessageBeep(MB_ICONHAND);
      if( m_cfg.pszMainIcon == TD_WARNING_ICON ) ::MessageBeep(MB_ICONEXCLAMATION);
      if( m_cfg.pszMainIcon == TD_INFORMATION_ICON ) ::MessageBeep(MB_ICONASTERISK);
   }

   void _RemoveHREF(LPTSTR pstr) const
   {
      if( (m_cfg.dwFlags & TDF_ENABLE_HYPERLINKS) == 0 ) return;
      LPTSTR p = pstr;
      while( *p != '\0' ) {
         if( *p == '<' ) {
            ATLTRACE(_T("Warning: Will remove hyperlinks from TaskDialog; no support on this platform"));
            while( *p != '\0' && *p != '>' ) p = ::CharNext(p);
            if( *p == '>' ) p = ::CharNext(p);
         }
         else {
            *pstr = *p;
#ifdef _MBCS
            if( ::IsDBCSLeadByte(*p) ) pstr[1] = p[1];
#endif // _MBCS
            p = ::CharNext(p);
            pstr = ::CharNext(pstr);
         }
      }
      *pstr = '\0';
   }

   RECT _CenterDialog(SIZE dluDialog) const
   {
      RECT rcArea = { 0 };
      if( ::IsWindow(m_cfg.hwndParent) && (m_cfg.dwFlags & TDF_POSITION_RELATIVE_TO_WINDOW) != 0 ) {
         ::GetWindowRect(m_cfg.hwndParent, &rcArea);
      }
      else {
         ::SystemParametersInfo(SPI_GETWORKAREA, NULL, &rcArea, NULL);
      }
      rcArea = MapDialogPixels(rcArea);
      RECT rc = { 0 };
      rc.left = rcArea.left + ((rcArea.right - rcArea.left) / 2 - dluDialog.cx / 2);
      rc.top = rcArea.top + ((rcArea.bottom - rcArea.top) / 2 - dluDialog.cy / 2);
      rc.right = rc.left + dluDialog.cx;
      rc.bottom = rc.top + dluDialog.cy;
      return rc;
   }

   HRESULT _DoCallback(UINT uMsg, WPARAM wParam = 0, LPARAM lParam = 0)
   {
      if( !m_bCreated ) return S_OK;
      if( m_cfg.pfCallback == NULL ) return S_OK;
      return m_cfg.pfCallback(m_hWnd, uMsg, wParam, lParam, m_cfg.lpCallbackData);
   }

   void _DoExpandCollapse()
   {
      if( (m_cfg.dwFlags & TDF_EXPAND_FOOTER_AREA) == 0 )
      {
         CWindow wndText = GetDlgItem(IDC_TASKDLG_EXTRATEXT);
         RECT rcText = { 0 };
         wndText.GetWindowRect(&rcText);
         int cy = (rcText.bottom - rcText.top) + m_Metrics.cyButtonLineGap;
         if( !m_bExpanded ) cy = -cy;
         HWND hWndFirst;
         CWindow wndChild = hWndFirst = GetWindow(GW_CHILD);
         while( wndChild != NULL ) {
            RECT rcWin = { 0 };
            wndChild.GetWindowRect(&rcWin);
            if( wndChild != wndText && rcWin.top >= rcText.top ) {
               ::OffsetRect(&rcWin, 0, cy);
               ::MapWindowPoints(HWND_DESKTOP, m_hWnd, (LPPOINT) &rcWin, 2);
               wndChild.MoveWindow(&rcWin);
            }
            wndChild = wndChild.GetWindow(GW_HWNDNEXT);
            if( wndChild == hWndFirst ) break;
         }
         wndText.ShowWindow(m_bExpanded ? SW_SHOWNOACTIVATE: SW_HIDE);
         if( m_Metrics.iButtonLinePos != 0 ) m_Metrics.iButtonLinePos += cy;
         if( m_Metrics.iFooterLinePos != 0 ) m_Metrics.iFooterLinePos += cy;
         ResizeClient(-1, m_bExpanded ? m_sizeDialog.cy : m_sizeDialog.cy + cy);
      }
      else
      {
         ResizeClient(-1, m_bExpanded ? m_sizeDialog.cy : m_Metrics.iExpandedLinePos - 1);
      }

      // Change text in Expander label
      LPTSTR pstrBuffer = (LPTSTR) malloc(MAX_TEXT_LENGTH * sizeof(TCHAR));
      if( pstrBuffer == NULL ) return;
      _LoadString(m_bExpanded ? m_cfg.pszExpandedControlText : m_cfg.pszCollapsedControlText, pstrBuffer, MAX_TEXT_LENGTH);
      SetDlgItemText(IDC_TASKDLG_EXPANDERTEXT, pstrBuffer);
      free(pstrBuffer);

      // Make sure we redraw stuff
      CWindow(GetDlgItem(IDC_TASKDLG_EXPANDERICON)).Invalidate();
      CWindow(GetDlgItem(IDC_TASKDLG_EXPANDERTEXT)).Invalidate();
      Invalidate();
   }

   void _AddControl(ATL::_U_STRINGorID ClassName, WORD wId, RECT rc, DWORD dwStyle, DWORD dwExStyle, ATL::_U_STRINGorID Text)
   {
      rc = MapDialogPixels(rc);
      m_Template.AddControl(ClassName, wId, (short) rc.left, (short) rc.top, (short) (rc.right - rc.left), (short) (rc.bottom - rc.top), dwStyle, dwExStyle, Text);
   }

   HICON _LoadIcon(LPCWSTR pstr, int cxy) const
   {
      USES_CONVERSION;
      if( pstr == NULL ) return NULL;
      HINSTANCE hInst = m_cfg.hInstance;
      if( pstr == TD_ERROR_ICON ) pstr = MAKEINTRESOURCEW(IDI_ERROR), hInst = NULL;
      if( pstr == TD_WARNING_ICON ) pstr = MAKEINTRESOURCEW(IDI_EXCLAMATION), hInst = NULL;
      if( pstr == TD_INFORMATION_ICON ) pstr = MAKEINTRESOURCEW(IDI_ASTERISK), hInst = NULL;
#if _WIN32_WINNT >= 0x0600
      if( pstr == TD_SHIELD_ICON ) pstr = MAKEINTRESOURCEW(IDI_SHIELD), hInst = NULL;
#endif // _WIN_WINNT
      UINT fuLoad = LR_DEFAULTCOLOR | LR_LOADTRANSPARENT;
      if( hInst == NULL ) fuLoad |= LR_SHARED;
      LPCTSTR pstrIcon = IS_INTRESOURCE(pstr) ? (LPCTSTR) pstr : W2CT(pstr);
      HICON hIcon = (HICON) ::LoadImage(hInst, pstrIcon, IMAGE_ICON, cxy, cxy, fuLoad);
      if( hIcon == NULL ) hIcon = (HICON) ::LoadImage(NULL, MAKEINTRESOURCE(IDI_APPLICATION), IMAGE_ICON, cxy, cxy, LR_DEFAULTCOLOR | LR_SHARED | LR_LOADTRANSPARENT);
      return hIcon;
   }

   void _LoadString(LPCWSTR pstr, LPTSTR pszBuffer, SIZE_T cchMax) const
   {
      USES_CONVERSION;
      ::ZeroMemory(pszBuffer, cchMax * sizeof(TCHAR));
      if( pstr == NULL ) return;
      if( IS_INTRESOURCE(pstr) ) ::LoadString(ModuleHelper::GetResourceInstance(), LOWORD(pstr), pszBuffer, cchMax);
      else ::lstrcpyn(pszBuffer, W2CT(pstr), cchMax);
   }

   void _SplitCommandText(LPTSTR pstrBuffer, LPCTSTR& pstrTitle, SIZE_T& cchTitle, LPCTSTR& pstrText, SIZE_T& cchText) const
   {
      LPTSTR pstrSel = pstrBuffer;
      while( *pstrSel != '\0' && *pstrSel != '\n' ) pstrSel = ::CharNext(pstrSel);
      if( *pstrSel == '\0' ) {
         pstrTitle = pstrBuffer;
         pstrText = NULL;
         cchTitle = lstrlen(pstrTitle);
         cchText = 0;
      }
      else {
         pstrTitle = pstrBuffer;
         pstrText = pstrSel + 1;
         cchText = lstrlen(pstrText);
         cchTitle = lstrlen(pstrBuffer) - cchText - 1;
      }
   }

   void _AlignDLU(LONG& pos, int iType) const
   {
      switch( iType ) {
      case HTLEFT:   pos -= pos % GetDialogBaseUnits().cx; break;
      case HTRIGHT:  pos += GetDialogBaseUnits().cx - (pos % GetDialogBaseUnits().cx); break;
      case HTTOP:    pos -= pos % GetDialogBaseUnits().cy; break;
      case HTBOTTOM: pos += GetDialogBaseUnits().cy - (pos % GetDialogBaseUnits().cy); break;
      }
   }

   SIZE _GetTextSize(LPCWSTR pstr, LPTSTR pszBuffer, SIZE_T cchMax, UINT uStyle, HFONT hFont, int cxMax) const
   {
      _LoadString(pstr, pszBuffer, cchMax);
      return _GetTextSize(pszBuffer, (UINT) -1, uStyle, hFont, cxMax);
   }

   SIZE _GetTextSizeHREF(LPCWSTR pstr, LPTSTR pszBuffer, SIZE_T cchMax, UINT uStyle, HFONT hFont, int cxMax) const
   {
      _LoadString(pstr, pszBuffer, cchMax);
      _RemoveHREF(pszBuffer);
      SIZE sizeText = _GetTextSize(pszBuffer, (UINT) -1, uStyle, hFont, cxMax);
      _LoadString(pstr, pszBuffer, cchMax);
      return sizeText;
   }

   SIZE _GetTextSize(LPCTSTR pstr, SIZE_T cchMax, UINT uStyle, HFONT hFont, int cxMax) const
   {
      CClientDC dc = m_cfg.hwndParent;
      SIZE sizeText = { 0 };
      if( pstr == NULL ) return sizeText;
      if( pstr[0] == '\0' ) return sizeText;
      HFONT hOldFont = dc.SelectFont(hFont);
      RECT rc = { 0, 0, cxMax, 9999 };
      if( cchMax == MAX_TEXT_LENGTH ) cchMax = (SIZE_T) -1;
      dc.DrawText(pstr, cchMax, &rc, DT_CALCRECT | uStyle);
      dc.SelectFont(hOldFont);
      SIZE size = { rc.right - rc.left + 1, rc.bottom - rc.top + 1 };
      return size;
   }
};

class CTask98Dialog : public CTask98DialogImpl<CTask98Dialog>
{
};


/////////////////////////////////////////////////////////////////////////
// TaskDialog 98 helpers
//

inline HRESULT Task98DialogIndirect(const TASKDIALOGCONFIG* pTaskConfig, int* pnButton, int* pnRadioButton, BOOL* pfVerificationFlagChecked)
{
   ATLASSERT(pTaskConfig);
   ATLASSERT(pTaskConfig->cbSize==sizeof(TASKDIALOGCONFIG));
   if( pnButton != NULL ) *pnButton = 0;
   CTask98Dialog dlg;
   if( !dlg.SetConfig(pTaskConfig) ) return E_FAIL;
   do { dlg.DoModal(pTaskConfig->hwndParent); } while( dlg.IsNavigated() );
   dlg.GetDialogResult(pnButton, pnRadioButton, pfVerificationFlagChecked);
   return S_OK;
}

inline HRESULT Task98Dialog(HWND hwndParent, HINSTANCE hInstance, LPCTSTR pszWindowTitle, LPCTSTR pszMainInstruction, LPCTSTR pszContent, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons, LPCTSTR pszIcon, int* pnButton)
{
   USES_CONVERSION;
   TASKDIALOGCONFIG cfg = { 0 };
   cfg.cbSize = sizeof(cfg);
   cfg.hInstance = hInstance;
   cfg.hwndParent = hwndParent;
   cfg.dwCommonButtons = dwCommonButtons;
   cfg.pszWindowTitle = IS_INTRESOURCE(pszWindowTitle) ? (LPCWSTR) pszWindowTitle : T2CW(pszWindowTitle);
   cfg.pszMainInstruction = IS_INTRESOURCE(pszMainInstruction) ? (LPCWSTR) pszMainInstruction : T2CW(pszMainInstruction);
   cfg.pszContent = IS_INTRESOURCE(pszContent) ? (LPCWSTR) pszContent : T2CW(pszContent);
   cfg.pszMainIcon = IS_INTRESOURCE(pszIcon) ? (LPCWSTR) pszIcon : T2CW(pszIcon);
   return Task98DialogIndirect(&cfg, pnButton, NULL, NULL);
}


#endif // !defined(AFX_TASKDIALOG_H__20073232_5AA0_1E88_3A6D_0080AD509054__INCLUDED_)

