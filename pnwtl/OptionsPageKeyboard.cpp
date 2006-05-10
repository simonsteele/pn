#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"

COptionsPageKeyboard::COptionsPageKeyboard(HMENU cmdSource, KeyMap* keyMap)
{
	m_pKeyMap = keyMap;
	m_hPrimaryCmdSource = cmdSource;
}

LRESULT COptionsPageKeyboard::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CSMenu menu(::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_MDICHILD)));
	m_list.Attach(GetDlgItem(IDC_KB_COMMANDS));
	m_list.SetViewType(LVS_REPORT);
	m_list.AddColumn(_T("Group"), 0);
	m_list.AddColumn(_T("Command"), 1);
	m_list.SetColumnWidth(0, 80);
	m_list.SetColumnWidth(1, 300);
	
	addItems(CSMenuHandle(menu), "", 0);

	return 0;
}

inline void fixText(char* buf, char* target)
{
	size_t l = strlen(buf);
	for(size_t ix(0); ix < l; ++ix)
	{
		if(*buf == '\t')
			break;
		if(*buf == '&')
		{
			buf++;
			continue;
		}
		else
		{
			*target++ = *buf++;
		}
	}
	*target = '\0';
}

int COptionsPageKeyboard::addItems(CSMenuHandle& menu, const char* group, int count)
{
	char buffer[256];
	char displayBuffer[256];
	MENUITEMINFO mii;
	memset(&mii, 0, sizeof(MENUITEMINFO));
	mii.cbSize = sizeof(MENUITEMINFO);
	mii.fMask = MIIM_STRING | MIIM_SUBMENU | MIIM_FTYPE;
	mii.dwTypeData = buffer;

	for(int i(0); i < menu.GetCount(); ++i)
	{
		mii.cch = 256;
		menu.GetItemInfo(i, &mii);

		// Skip separators
		if((mii.fType & MFT_SEPARATOR) != 0)
			continue;

		if(mii.hSubMenu != NULL)
		{
			std::string newgroup(group);
			if(newgroup.size())
				newgroup += ".";
			fixText(buffer, displayBuffer);
			newgroup += displayBuffer;
			count = addItems(CSMenuHandle(mii.hSubMenu), newgroup.c_str(), count);
		}
		else
		{
			fixText(buffer, displayBuffer);
			int ixItem = m_list.AddItem(count++, 0, group);
			m_list.SetItemText(ixItem, 1, displayBuffer);
		}
	}
	
	return count;
}

void COptionsPageKeyboard::OnOK()
{
}
void COptionsPageKeyboard::OnInitialise()
{
}

LPCTSTR COptionsPageKeyboard::GetTreePosition()
{
	return _T("General\\Keyboard");
}

void COptionsPageKeyboard::OnCancel()
{

}