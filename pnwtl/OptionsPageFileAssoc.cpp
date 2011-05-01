/**
 * @file OptionsPageFileAssoc.cpp
 * @brief File Associations Window
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"
#include "ssreg.h"
#include "l10n.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageFileAssoc
//////////////////////////////////////////////////////////////////////////////

COptionsPageFileAssoc::COptionsPageFileAssoc()
	: m_bDirty(false)
	, m_mode(ModeNone)
	, m_fam()
	, m_bKeyChange(false)
{
	m_bExplorerMenuRegistered = checkExplorerContextMenu();
}

tstring COptionsPageFileAssoc::GetTreePosition()
{
	return LS(IDS_OPTPAGE_FILEASSOC);
}

void COptionsPageFileAssoc::OnOK()
{
	if(!m_bCreated)
		return;

	CString ext;
	CString method;
	FileAssoc fa;
	for(int i = 0; i < m_list.GetItemCount(); i++)
	{
		ListItemToFileAssoc(i, fa);
		m_fam.SetAssociation(fa);
	}
	m_fam.UpdateAssociations();

	OPTIONS->Set(PNSK_INTERFACE, _T("CheckAssocsOnStartup"),
		IsDlgButtonChecked(IDC_CHECKONSTARTUP) == BST_CHECKED);
}

void COptionsPageFileAssoc::OnInitialise()
{
	CheckDlgButton(IDC_CHECKONSTARTUP,
		OPTIONS->Get(PNSK_INTERFACE, _T("CheckAssocsOnStartup"), false) ? BST_CHECKED : BST_UNCHECKED);

	const FileAssocManager::FileAssocs& fas = m_fam.GetAssociations();
	for(int i = 0; i < fas.GetSize(); i++)
	{
		const FileAssoc& fa = fas[i];
		CString verbName(fa.GetVerbName(true));
		m_list.AddItem(i, ColConflict, _T(""));
		m_list.SetItemText(i, ColExtension, fa.GetExtension());
		m_list.SetItemText(i, ColMethod, (LPCTSTR)verbName);
		m_list.SetItemText(i, ColTypeName, fa.GetCurrentTypeName());
	}
}

void COptionsPageFileAssoc::OnCancel()
{
}

LRESULT COptionsPageFileAssoc::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_combo.Attach(GetDlgItem(IDC_FILEASSO_METHOD));
	m_combo.AddString(LS(IDS_FILEASSOC_OPEN));
	m_combo.AddString(LS(IDS_FILEASSOC_EDIT));
	m_combo.AddString(LS(IDS_FILEASSOC_EDITWITH));
	m_combo.SetCurSel(0);

	m_buttonAddEdit.Attach(GetDlgItem(IDC_ADD));
	m_buttonRemove.Attach(GetDlgItem(IDC_REMOVE));

	m_list.Attach(GetDlgItem(IDC_FILEASSO_LIST));
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_INFOTIP);
	m_list.AddColumn(_T("!"), ColConflict, ColConflict, LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM, LVCFMT_CENTER);
	m_list.AddColumn(LS(IDS_HDR_FILEASSOC_EXTENSION), ColExtension, ColExtension);
	m_list.AddColumn(LS(IDS_HDR_FILEASSOC_METHOD), ColMethod, ColMethod);

	RECT rc;
	::GetWindowRect(GetDlgItem(IDC_FILEASSO_LIST), &rc);

	tstring typeName(LS(IDS_HDR_FILEASSOC_TYPENAME));

	LVCOLUMN lvc = { 0 };
	lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;
	lvc.fmt = LVCFMT_LEFT;
	lvc.pszText = const_cast<TCHAR*>(typeName.c_str());
	lvc.cx = (rc.right - rc.left) / 2;
	lvc.iSubItem = ColTypeName;
	m_list.InsertColumn(ColTypeName, &lvc);

	// Prepare to draw conflicts highlighted
	m_boldFont;
	HFONT hFont = GetFont();
	LOGFONT lf = {0};
	GetObject(hFont, sizeof(lf), &lf);
	lf.lfWeight = FW_BOLD;
	m_boldFont.CreateFontIndirect(&lf);

	// Ensure there's (some) contrast between the highlight color and the background
	m_colors[0] = ::GetSysColor(COLOR_WINDOWTEXT);
	COLORREF bkgnd = ::GetSysColor(COLOR_WINDOW);
	COLORREF highlight = RGB(255, 0, 0);
	const int tolerance = 0x7F;
	int r = abs(GetRValue(bkgnd) - GetRValue(highlight));
	int g = abs(GetGValue(bkgnd) - GetGValue(highlight));
	int b = abs(GetBValue(bkgnd) - GetBValue(highlight));
	if(r < tolerance && g < tolerance && b < tolerance)
	{
		//TODO: Should a calculate to get an inverse take place here?
		// But this if body will only be reached when the user has a redish
		// window background and we won't be able to use this signal color
		// then...
		highlight = m_colors[0];
	}

	m_colors[1] = highlight;

	if (m_bExplorerMenuRegistered)
	{
		GetDlgItem(IDC_ENABLEEXPLORERCONTEXT).SetWindowText(LS(IDS_OPTIONS_REMOVEEDITWITH));		
	}

	return 0;
}

LRESULT COptionsPageFileAssoc::OnExtensionChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString curExt;
	GetDlgItemText(IDC_EXTENSION, curExt);
	if(curExt.GetLength() == 0)
	{
		SetMode(ModeNone, false);
	}
	else
	{
		// If the extension exists in the list, select the item and set the
		// EditMode. This case is true when the user just selected an item
		// in the list (may be a performace problem on a very low end computer
		// and a very large list of extensions. For this edge case a separate
		// m_list.GetSelectedIndex() != -1 test would perform better).

		// ListViewCtrl::FindItem does not work here because the extension is
		// in a subitem.
		int extIndex;
		for(extIndex = 0; extIndex < m_list.GetItemCount(); extIndex++)
		{
			CString ext;
			m_list.GetItemText(extIndex, 1, ext);
			if(ext == curExt)
			{
				break;
			}
		}

		if(extIndex < m_list.GetItemCount())
		{
			CString method;
			m_list.GetItemText(extIndex, ColMethod, method);
			m_combo.SelectString(0, method);
			m_bKeyChange = true;
			m_list.SelectItem(extIndex);
			m_bKeyChange = false;
			SetMode(ModeEdit, false);
		}
		else
		{
			SetMode(ModeAdd, false);
		}
	}

	return 0;
}

LRESULT COptionsPageFileAssoc::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString ext;
	GetDlgItemText(IDC_EXTENSION, ext);
	CString method;
	m_combo.GetLBText(m_combo.GetCurSel(), method);

	FileAssoc fa(ext, method);
	m_fam.SetAssociation(fa);

	int itemIndex;
	if(m_mode == ModeAdd)
	{
		itemIndex = m_list.GetItemCount();
		m_list.AddItem(itemIndex, ColConflict, _T(""));
		m_list.SetItemText(itemIndex, ColExtension, fa.GetExtension());
		m_list.SetItemText(itemIndex, ColTypeName, fa.GetCurrentTypeName());
	}
	else //if(m_mode == ModeEdit)
	{
		itemIndex = m_list.GetSelectedIndex();
	}

	m_list.SetItemText(itemIndex, ColMethod, method);

	if(fa.HasConflict())
	{
		if(m_conflicts.FindKey(ext) != -1)
		{
			m_conflicts.SetAt(ext, fa.GetCurrentAppName());
		}
		else
		{
			m_conflicts.Add(ext, fa.GetCurrentAppName());
		}
		m_list.SetItemText(itemIndex, ColConflict, _T("!"));
	}
	else if(!fa.IsValid())
	{
		// We can ignore the test for VerbNone in FileAssoc::IsValid() here
		// because the user cannot enter VerbNone.
		tstring conflictMsg(LS(IDS_FILEASSOC_BADCHARS));
		LPCTSTR invalidChars = FileAssoc::GetInvalidChars();
		for(int i = 0; invalidChars[i] != 0; i++)
		{
			conflictMsg += invalidChars[i];
			conflictMsg += _T(' ');
		}

		m_list.SetItemText(itemIndex, ColConflict, _T("!"));
		m_list.SetItemText(itemIndex, ColExtension, ext);
		m_list.SetItemText(itemIndex, ColTypeName, conflictMsg.c_str());
	}
	else
	{
		int i = m_conflicts.FindKey(ext);
		if(i != -1)
		{
			m_conflicts.RemoveAt(i);
		}

		m_list.SetItemText(itemIndex, ColConflict, _T(""));
	}

	m_bDirty = true;
	SetMode(ModeNone);
	return 0;
}

LRESULT COptionsPageFileAssoc::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int selIndex = m_list.GetSelectedIndex();
	if(selIndex != -1)
	{
		RemoveExtension(selIndex);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnCheckNowClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_fam.UpdateAssociations();
	return 0;
}

LRESULT COptionsPageFileAssoc::OnEnableExplorerContextMenu(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (m_bExplorerMenuRegistered)
	{
		removeContextMenu();
	}
	else
	{
		addContextMenu();
	}

	return 0;
}

LRESULT COptionsPageFileAssoc::OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	// The item selected state can change throught various ways:
	// 1. the user selected an item in the list
	// 2. the user modifies an extension he just selected which results in a
	//    deselect
	// 3. the user clicks in the list view but not on an item
	// In the first case, we want to switch to edit mode, in the 2nd to add
	// mode and in the last to none.

	NMLISTVIEW* plv = (LPNMLISTVIEW)pnmh;
	if(/*m_mode != ModeAdd && */plv->uChanged == LVIF_STATE)
	{
		if((plv->uNewState & LVIS_SELECTED) != 0)
		{
			CString ext;
			CString method;
			m_list.GetItemText(plv->iItem, ColExtension, ext);
			m_list.GetItemText(plv->iItem, ColMethod, method);
			//NOTE: setting the extension edit after clearing it in SetMode results in the item being deselected again
			SetMode(ModeEdit, !m_bKeyChange, ext);
			m_combo.SelectString(0, method);
		}
		else
		{
			SetMode(ModeNone);
		}
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListSetFocus(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
{
	if(m_list.GetSelectedIndex() == -1)
	{
		SetMode(ModeNone);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLVGETINFOTIP* plgit = (NMLVGETINFOTIP*)pnmh;
	CString ext;
	m_list.GetItemText(plgit->iItem, ColExtension, ext);
	int i = m_conflicts.FindKey(ext);
	if(i != -1)
	{
		CString msg;
		msg.Format(LS(IDS_FILEASSOC_CURRENTASSOC), m_conflicts.GetValueAt(i));
		::lstrcpyn(plgit->pszText, msg, plgit->cchTextMax);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLVKEYDOWN* plkd = (NMLVKEYDOWN*)pnmh;
	int selIndex = m_list.GetSelectedIndex();
	if(plkd->wVKey == VK_DELETE && selIndex != -1)
	{
		RemoveExtension(selIndex);
	}
	return 0;
}

DWORD COptionsPageFileAssoc::OnPrePaint(int idCtrl, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	DWORD ret = CDRF_DODEFAULT;
	if(idCtrl == IDC_LIST)
	{
		ret = CDRF_NOTIFYITEMDRAW;
	}
	return ret;
}

DWORD COptionsPageFileAssoc::OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	DWORD ret = CDRF_DODEFAULT;
	CString ext;
	m_list.GetItemText(lpNMCustomDraw->dwItemSpec, ColExtension, ext);
	int i = m_conflicts.FindKey(ext);
	if(i != -1)
	{
		CDCHandle dc(lpNMCustomDraw->hdc);
		dc.SelectFont(m_boldFont);
		ret = CDRF_NEWFONT | CDRF_NOTIFYSUBITEMDRAW;
	}
	return ret;
}

DWORD COptionsPageFileAssoc::OnSubItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	NMLVCUSTOMDRAW* lvcd = (NMLVCUSTOMDRAW*)lpNMCustomDraw;
	if(lvcd->iSubItem == ColConflict)
		lvcd->clrText = m_colors[1];
	else
		lvcd->clrText = m_colors[0];
	return CDRF_DODEFAULT;
}

void COptionsPageFileAssoc::SetMode(Mode mode, bool setExt /*= true*/, LPCTSTR ext /*= NULL*/)
{
	if(m_mode == ModeChanging)
		return;

	m_mode = ModeChanging;

	if(setExt)
	{
		SetDlgItemText(IDC_EXTENSION, ext);
	}
	m_buttonAddEdit.SetWindowText(mode == ModeAdd ? _T("&Add") : _T("&Edit"));
	m_buttonAddEdit.EnableWindow(mode != ModeNone);
	m_buttonRemove.EnableWindow(m_list.GetSelectedIndex() > -1);

	if(mode != ModeEdit && m_list.GetSelectedIndex() > -1)
	{
		m_list.SetItemState(m_list.GetSelectedIndex(), 0, LVIS_SELECTED);
	}

	m_mode = mode;
}

void COptionsPageFileAssoc::ListItemToFileAssoc(int index, FileAssoc& fa)
{
	CString ext;
	CString method;
	m_list.GetItemText(index, ColExtension, ext);
	m_list.GetItemText(index, ColMethod, method);

	fa.SetExtensionAndVerb(ext, method);
}

void COptionsPageFileAssoc::RemoveExtension(int index)
{
	FileAssoc fa;
	ListItemToFileAssoc(index, fa);
	m_fam.UnsetAssociation(fa);

	m_list.DeleteItem(index);
}

/**
 * Find out if the pnotepad context menu extension is registered.
 */
bool COptionsPageFileAssoc::checkExplorerContextMenu()
{
	ssreg::CSRegistry reg;
	tstring key;

	if (g_Context.OSVersion.dwMajorVersion >= 6)
	{
		// Vista or later, we write to the per-user classes location:
		key = _T("Software\\Classes\\*\\shell\\pnotepad");
	}
	else
	{
		reg.SetRootKey(HKEY_CLASSES_ROOT);
		key = _T("*\\shell\\pnotepad");
	}

	if (reg.OpenKey(key.c_str(), false))
	{
		reg.CloseKey();
		return true;
	}

	return false;
}

void COptionsPageFileAssoc::addContextMenu()
{
	tstring appName(LS(IDS_EXPLORERCONTEXTMENU));
		
	tstring appPath;
	OPTIONS->GetPNPath(appPath);

	CFileName fn(_T("pn.exe"));
	fn.Root(appPath.c_str());

	appPath = fn.c_str();
	appPath += _T(" \"%1\"");

	tstring key;
	ssreg::CSRegistry reg;

	if (g_Context.OSVersion.dwMajorVersion >= 6)
	{
		// Vista or later, we write to the per-user classes location:
		key = _T("Software\\Classes\\*\\shell\\pnotepad");
	}
	else
	{
		reg.SetRootKey(HKEY_CLASSES_ROOT);
		key = _T("*\\shell\\pnotepad");
	}

	if (reg.OpenKey(key.c_str()))
	{
		reg.WriteString(NULL, appName.c_str());
		reg.CloseKey();
	}

	key += L"\\Command";
	if (reg.OpenKey(key.c_str()))
	{
		reg.WriteString(NULL, appPath.c_str());
		reg.CloseKey();
	}

	m_bExplorerMenuRegistered = true;
	GetDlgItem(IDC_ENABLEEXPLORERCONTEXT).SetWindowText(LS(IDS_OPTIONS_REMOVEEDITWITH));
}

void COptionsPageFileAssoc::removeContextMenu()
{
	tstring key;
	ssreg::CSRegistry reg;

	if (g_Context.OSVersion.dwMajorVersion >= 6)
	{
		// Vista or later, we write to the per-user classes location:
		key = _T("Software\\Classes\\*\\shell\\pnotepad");
	}
	else
	{
		reg.SetRootKey(HKEY_CLASSES_ROOT);
		key = _T("*\\shell\\pnotepad");
	}

	if (reg.DeleteKey(key.c_str()))
	{
		m_bExplorerMenuRegistered = false;
		GetDlgItem(IDC_ENABLEEXPLORERCONTEXT).SetWindowText(LS(IDS_OPTIONS_ADDEDITWITH));
	}
}