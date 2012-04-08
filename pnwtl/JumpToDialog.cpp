/**
 * @file jumptodialog.cpp
 * @brief Jump To Dialog Implementation
 * @author Simon Steele
 * @note Copyright (c) 2004-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "jumpto.h"
#include "jumptodialog.h"
#include "jumpview.h"
#include "include/liquidmetal.h"

#define COLWIDTH_PARENT 90
#define COLWIDTH_LINE	60
#define COLWIDTH_TAG	153

CJumpToDialog::CJumpToDialog(CChildFrame* pChild) : edtTag(this, 1)
{
	m_pChild = pChild;
}

LRESULT CJumpToDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	list.Attach(GetDlgItem(IDC_JUMPTOLIST));

	btnOk.Attach(GetDlgItem(IDOK));
	btnCancel.Attach(GetDlgItem(IDCANCEL));

	edtTag.SubclassWindow(GetDlgItem(IDC_JUMPTOTEXT));

	list.InsertColumn(0, _T("Tag"), LVCFMT_LEFT, COLWIDTH_TAG, 0);
	list.InsertColumn(1, _T("Parent"), LVCFMT_LEFT, COLWIDTH_PARENT, 0);
	list.InsertColumn(2, _T("Line"), LVCFMT_LEFT, COLWIDTH_LINE, 0);

	images.CreateFromImage(IDB_TAGTYPES, 16, 1, RGB(255,0,255), IMAGE_BITMAP);
	list.SetImageList( images.m_hImageList, LVSIL_SMALL );

	int colOrder[3] = {1, 0, 2};
	list.SetColumnOrderArray(3, colOrder);

	list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	CRect rc, rcList, rcBtn, rcCancel;
	GetClientRect(&rc);
	list.GetWindowRect(&rcList);
	ScreenToClient(&rcList);
	listGaps.cx = rc.Width() - rcList.right;
	listGaps.cy = rc.Height() - rcList.bottom;
	
	btnOk.GetWindowRect(&rcBtn);
	btnCancel.GetWindowRect(&rcCancel);
	ScreenToClient(&rcBtn);
	ScreenToClient(&rcCancel);
	buttonGap = rcBtn.top - rcList.bottom;
	buttonWidth = rcBtn.Width();
	buttonGapX = rcCancel.left - rcBtn.right;

	JumpToHandler::GetInstance()->FindTags(m_pChild, this);
	std::sort(m_targets.begin(), m_targets.end(), [] (const Target& l, const Target& r) { return stricmp(l.Tag.c_str(), r.Tag.c_str()) < 0; });
	display(m_targets);

	return 0;
}

LRESULT CJumpToDialog::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	int width = LOWORD(lParam);
	int height = HIWORD(lParam);

    CRect rc, rc2, rc3;
	
	// Size the list control
	list.GetWindowRect(&rc);
	ScreenToClient(&rc);
	rc.right = width - listGaps.cx;
	rc.bottom = height - listGaps.cy;
	list.SetWindowPos(HWND_TOP, &rc, SWP_NOMOVE | SWP_NOZORDER);

	// The edit box
	edtTag.GetWindowRect(&rc2);
	ScreenToClient(&rc2);
	rc2.right = rc.right;
	edtTag.SetWindowPos(HWND_TOP, &rc2, SWP_NOMOVE | SWP_NOZORDER);

	// Cancel button
	btnCancel.GetWindowRect(&rc3);
	ScreenToClient(&rc3);
	rc3.MoveToX(rc.right - buttonWidth);
	rc3.MoveToY(rc.bottom + buttonGap);
	btnCancel.SetWindowPos(HWND_TOP, &rc3, SWP_NOSIZE | SWP_NOZORDER);

	// Ok button
	btnOk.GetWindowRect(&rc2);
	ScreenToClient(&rc2);
	rc2.MoveToX(rc3.left - buttonGapX - buttonWidth);
	rc2.MoveToY(rc.bottom + buttonGap);
	btnOk.SetWindowPos(HWND_TOP, &rc2, SWP_NOSIZE | SWP_NOZORDER);	

	// Now size the tag column to fit.
	list.GetClientRect(&rc);	
	int cw = rc.Width() - list.GetColumnWidth(1) - list.GetColumnWidth(2);
	list.SetColumnWidth(0, cw);

	return 0;
}

LRESULT CJumpToDialog::OnListDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	transfer();
	if(line != -1)
	{
		EndDialog(IDOK);
	}

	return 0;
}

LRESULT CJumpToDialog::OnEditKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam == VK_DOWN)
	{
		int index = list.GetSelectedIndex() + 1;
		if(index >= list.GetItemCount())
			index--;
		list.SelectItem(index);
	}
	else if(wParam == VK_UP)
	{
		int index = list.GetSelectedIndex() - 1;
		if(index < 0)
			index = 0;
		list.SelectItem(index);
	}
	else
		bHandled = FALSE;

	return 0;
}

void CJumpToDialog::OnFound(int count, LPMETHODINFO methodInfo)
{
	int image(0);
	if(methodInfo->type <= TAG_MAX)
	{
		image = jumpToTagImages[methodInfo->type].imagesNumber;
	}

	if (methodInfo->parentName)
	{
		m_targets.push_back(Target(methodInfo->methodName, methodInfo->parentName, methodInfo->lineNumber, image));
	}
	else
	{
		m_targets.push_back(Target(methodInfo->methodName, methodInfo->lineNumber, image));
	}
}

LRESULT CJumpToDialog::OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(GetDlgItem(IDC_JUMPTOTEXT));
	filter((LPCTSTR)wt);
	
	return 0;
}

LRESULT CJumpToDialog::OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	transfer();
	EndDialog(wID);
	return 0;
}

LRESULT CJumpToDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}

int CJumpToDialog::GetLine()
{
	return line;
}

/**
 * Select the best match for what the user typed. This could potentially
 * also filter the list but that would make this dialog *much* more
 * expensive.
 */
void CJumpToDialog::filter(LPCTSTR text)
{
	m_filtered.clear();

	if (text == NULL || *text == NULL)
	{
		display(m_targets);
		return;
	}

	CT2CA partial(text);
	std::string prtl(partial);
	m_filtered = m_targets;

	// Sort the known words by their quicksilver ranks:
	LiquidMetal::QuickSilver qs(prtl);
	std::sort(m_filtered.begin(), m_filtered.end(), [&qs](const Target& l, const Target& r) -> bool { return qs.Score(l.Tag) > qs.Score(r.Tag); });

	// Remove anything with a rank of less than 0.1:
	auto i = std::lower_bound(m_filtered.begin(), m_filtered.end(), 0.1, [&qs, &prtl](const Target& val, const double min) -> bool { return qs.Score(val.Tag) > min; });
	m_filtered.erase(i, m_filtered.end());
	
	/*Target t(static_cast<const char*>(partial), "", 0, 0);
	size_t length(t.Tag.size());
	auto i = std::lower_bound(m_targets.begin(), m_targets.end(), t, [length](const Target& l, const Target& r) -> bool { 
		if (_strnicmp(l.Tag.c_str(), r.Tag.c_str(), length) < 0)
		{
			return true;
		}
		
		return false;
	});
	for (; i != m_targets.end(); ++i)
	{
		if (_strnicmp(static_cast<const char*>(partial), i->Tag.c_str(), length) != 0)
		{
			break;
		}

		m_filtered.push_back(*i);
	}*/

	display(m_filtered);
}

/**
 * Transfer the selected item into the line member. 
 * Set to -1 if the selection isn't valid.
 */
void CJumpToDialog::transfer()
{
	int selIndex = list.GetSelectedIndex();
	if(selIndex != -1)
	{
		Target* target = reinterpret_cast<Target*>(list.GetItemData(selIndex));
		line = target->Line;
	}
	else
		line = -1;
}

/**
 * Display a set of targets.
 */
void CJumpToDialog::display(const std::vector<Target>& targets)
{
	list.DeleteAllItems();
	list.LockWindowUpdate();

	LVITEM lvi;
	memset(&lvi, 0, sizeof(LVITEM));

	int index(0);

	BOOST_FOREACH(const Target& target, targets)
	{
		CA2CT methodName(target.Tag.c_str());
	
		lvi.iItem = index++;
		lvi.iSubItem = 0;
		lvi.mask = LVIF_TEXT | LVIF_IMAGE;
		lvi.pszText = const_cast<TCHAR*>(static_cast<const TCHAR*>(methodName));
	
		int index = list.InsertItem(&lvi);
		lvi.mask = LVIF_TEXT;

		if (target.Parent.size())
		{
			CA2CT parentName(target.Parent.c_str());
			lvi.iItem = index;
			lvi.iSubItem = 1;
			lvi.pszText = const_cast<TCHAR*>(static_cast<const TCHAR*>(parentName));
			list.SetItem(&lvi);
		}

		_itot(target.Line, itoabuf, 10);
		lvi.iSubItem = 2;
		lvi.pszText = itoabuf;
		list.SetItem(&lvi);

		list.SetItemData(index, reinterpret_cast<DWORD_PTR>(&target));
	}

	if (list.GetItemCount() > 0)
	{
		list.SelectItem(0);
	}

	list.LockWindowUpdate(0);
}