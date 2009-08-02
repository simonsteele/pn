/**
 * @file optionspagekeyboard.cpp
 * @brief Options Dialog Keyboard Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsPageKeyboard.h"
#include "ExtApp.h"
#include "ScriptRegistry.h"

COptionsPageKeyboard::COptionsPageKeyboard(CommandDispatch* dispatcher)
{
	m_pDispatch = dispatcher;
	m_pKeyMap = new Commands::KeyMap(*dispatcher->GetCurrentKeyMap());
	m_bDirty = false;
	m_pCurrent = NULL;
}

COptionsPageKeyboard::~COptionsPageKeyboard()
{
	delete m_pKeyMap;
}

void COptionsPageKeyboard::OnOK()
{
	if(m_bDirty)
		m_pDispatch->SetCurrentKeyMap(m_pKeyMap);

	if(!m_bCreated)
		return;

	cleanUp();
}

void COptionsPageKeyboard::OnInitialise()
{
}

LPCTSTR COptionsPageKeyboard::GetTreePosition()
{
	return _T("General\\Keyboard");
}

bool COptionsPageKeyboard::IsDirty() const
{
	return m_bDirty;
}

void COptionsPageKeyboard::OnCancel()
{
	if(m_bCreated)
		cleanUp();
}

LRESULT COptionsPageKeyboard::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_list.Attach(GetDlgItem(IDC_KB_COMMANDS));
	m_list.SetViewType(LVS_REPORT);
	m_list.AddColumn(_T("Group"), 0);
	m_list.AddColumn(_T("Command"), 1);
	m_list.SetColumnWidth(0, 80);
	m_list.SetColumnWidth(1, 300);
	m_list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	CSMenu menu(::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_MDICHILD)));
	addItems(CSMenuHandle(menu), "", 0);
	addExtensions();

	m_shortcutlist.Attach(GetDlgItem(IDC_KB_ASSIGNEDLIST));
	m_hotkey.SubclassWindow(GetDlgItem(IDC_KB_HOTKEY));

	enableButtons();

	return 0;
}

LRESULT COptionsPageKeyboard::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(!m_pCurrent)
		return 0;

	WORD keycode, modifiers;
	m_hotkey.GetHotKey(keycode, modifiers);
	if(keycode == 0)
		return 0;

	modifiers = HKToAccelMod(modifiers);

	if(currentIsExtended())
	{
		// Find the script command...
		ScriptRegistry* registry = static_cast<ScriptRegistry*>( g_Context.ExtApp->GetScriptRegistry() );
		ExtendedCommandDetails* cd = static_cast<ExtendedCommandDetails*>(m_pCurrent);
		Script* script = registry->FindScript(cd->group.c_str(), cd->name.c_str());

		// Add the extension command details...
		ExtensionCommand cmd;
		cmd.command = script->ScriptRef;
		cmd.key = (unsigned char)keycode;
		cmd.modifiers = (unsigned char)modifiers;
		cmd.msg = 0;
		m_pKeyMap->AddExtended(cmd);
	}
	else
	{
		m_pKeyMap->AssignCmdKey(keycode, modifiers, m_pCurrent->command);
	}

	m_bDirty = true;

	updateSelection();

	return 0;
}

LRESULT COptionsPageKeyboard::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pCurrent == NULL)
		return 0;

	if(m_shortcutlist.GetSelCount() == 0)
		return 0;

	for(int i = 0; i < m_shortcutlist.GetCount(); ++i)
	{
		if(m_shortcutlist.GetSel(i))
		{
			int data = m_shortcutlist.GetItemData(i);
			if(currentIsExtended())
			{
				m_pKeyMap->RemoveExtended( data & 0x00ff, (data & 0xff00) >> 8 );
			}
			else
			{
				m_pKeyMap->RemoveCmdKey(
					m_pKeyMap->GetMappings()[data].key,
					m_pKeyMap->GetMappings()[data].modifiers,
					m_pKeyMap->GetMappings()[data].msg);
			}
		}
	}

	m_bDirty = true;

	updateSelection();

	return 0;
}

LRESULT COptionsPageKeyboard::OnHotKeyChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	WORD keycode, modifiers, real_modifiers(0);
	m_hotkey.GetHotKey(keycode, modifiers);

	real_modifiers = HKToAccelMod(modifiers);

	// Look for commands with this key assigned...
	const KeyToCommand* mappings = m_pKeyMap->GetMappings();
	size_t noof_mappings(m_pKeyMap->GetCount());

	tstring command_name;

	int command = m_pKeyMap->Find(keycode, real_modifiers);
	if(command != 0)
	{
		// We found a command with this key combination assigned, so now we have a command
		// we need to find the name of that command... :(
		command_name = findCommandName(command);
	}
	else
	{
		// Is there an extended command registered instead?
		const ExtensionCommand* cmd = m_pKeyMap->FindExtended((unsigned char)keycode, (unsigned char)real_modifiers);
		if(cmd != NULL)
		{
			command_name = findCommandName( cmd->command );
		}
	}

	if(command_name.size())
	{
		command_name = _T("Currently assigned to ") + command_name;
		GetDlgItem(IDC_KB_SHORTCUTINUSELABEL).SetWindowText(command_name.c_str());
		GetDlgItem(IDC_KB_SHORTCUTINUSELABEL).ShowWindow(SW_SHOW);
		return 0;
	}

	GetDlgItem(IDC_KB_SHORTCUTINUSELABEL).ShowWindow(SW_HIDE);

	return 0;
}

LRESULT COptionsPageKeyboard::OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLISTVIEW* plv = (LPNMLISTVIEW)pnmh;
	if(plv->uChanged == LVIF_STATE && (plv->uNewState & LVIS_SELECTED) )
	{
		//Selection changed...
		updateSelection();
		m_hotkey.SetHotKey(0, 0);
		GetDlgItem(IDC_KB_SHORTCUTINUSELABEL).ShowWindow(SW_HIDE);
	}

	return 0;
}

LRESULT COptionsPageKeyboard::OnKeySelChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();
	return 0;
}

inline void fixText(TCHAR* buf, TCHAR* target)
{
	size_t l = _tcslen(buf);
	for(size_t ix(0); ix < l; ++ix)
	{
		if(*buf == _T('\t'))
			break;
		if(*buf == _T('&'))
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
	TCHAR buffer[256];
	TCHAR displayBuffer[256];
	MENUITEMINFO mii;
	memset(&mii, 0, sizeof(MENUITEMINFO));
	mii.cbSize = sizeof(MENUITEMINFO);
	mii.fMask = MIIM_STRING | MIIM_SUBMENU | MIIM_FTYPE | MIIM_ID;
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
			CT2CA convtext(displayBuffer);
			newgroup += convtext;
			count = addItems(CSMenuHandle(mii.hSubMenu), newgroup.c_str(), count);
		}
		else
		{
			fixText(buffer, displayBuffer);
			CA2CT groupText(group);
			int ixItem = m_list.AddItem(count++, 0, groupText);
			m_list.SetItemText(ixItem, 1, displayBuffer);
			
			// Store info about the command...
			CommandDetails* cd = new CommandDetails;
			cd->type = cdtCommand;
			cd->command = mii.wID;
			m_list.SetItemData(ixItem, reinterpret_cast<DWORD_PTR>(cd));
		}
	}
	
	return count;
}

void COptionsPageKeyboard::addExtensions()
{
	ScriptRegistry* registry = static_cast<ScriptRegistry*>( g_Context.ExtApp->GetScriptRegistry() );
	const group_list_t& groups = registry->GetGroups();
	int count = m_list.GetItemCount();
	for(group_list_t::const_iterator i = groups.begin(); i != groups.end(); ++i)
	{
		std::string group("Scripts.");
		group += (*i)->GetName();
		for(script_list_t::const_iterator j = (*i)->GetScripts().begin();
			j != (*i)->GetScripts().end(); ++j)
		{
			CA2CT groupconv(group.c_str());
			CA2CT nameconv((*j)->Name.c_str());

			int ixItem = m_list.AddItem(count++, 0, groupconv);
			m_list.SetItemText(ixItem, 1, nameconv);
			
			// Store the details of this command
			ExtendedCommandDetails* ecd = new ExtendedCommandDetails;
			ecd->type = cdtExtended;
			ecd->group = (*i)->GetName();
			ecd->name = (*j)->Name;
			m_list.SetItemData(ixItem, reinterpret_cast<DWORD_PTR>(ecd));
		}
	}
}

void COptionsPageKeyboard::clear()
{
	m_shortcutlist.ResetContent();
	m_hotkey.SetHotKey(0,0);
	BOOL b;
	OnHotKeyChanged(0,0,0,b);
}

void COptionsPageKeyboard::enableButtons()
{
	int sel = m_list.GetSelectedIndex();
	m_hotkey.EnableWindow( sel != -1 );
	GetDlgItem(IDC_KB_ADD).EnableWindow( sel != -1 );
	
	int kbsel = m_shortcutlist.GetSelCount();
	GetDlgItem(IDC_KB_REMOVE).EnableWindow( kbsel != 0 );
}

/**
 * Given a command with an associated combination, this method
 * returns the textual name of that command - used for 
 * "already assigned to: %s"
 */
tstring COptionsPageKeyboard::findCommandName(DWORD command)
{
	for(int ix(0); ix < m_list.GetItemCount(); ++ix)
	{
		CommandDetails* cd = reinterpret_cast<CommandDetails*>( m_list.GetItemData(ix) );
		if(cd->type == cdtCommand && cd->command == command)
		{
			tstring res;
			CString s;
			m_list.GetItemText(ix, 0, s);
			res = s;
			res += _T(".");
			m_list.GetItemText(ix, 1, s);
			res += s;
			
			return res;
		}
	}

	return _T("");
}

/**
 * Given an extension command string (e.g. python:Script) what's the friendly
 * name?
 */
tstring COptionsPageKeyboard::findCommandName(const std::string& extcommand)
{
	ScriptRegistry* registry = static_cast<ScriptRegistry*>( g_Context.ExtApp->GetScriptRegistry() );

	for(int ix(0); ix < m_list.GetItemCount(); ++ix)
	{
		CommandDetails* cd = reinterpret_cast<CommandDetails*>( m_list.GetItemData(ix) );
		if(cd->type == cdtExtended)
		{
			ExtendedCommandDetails* ecd = static_cast<ExtendedCommandDetails*>( cd );
			Script* script = registry->FindScript(ecd->group.c_str(), ecd->name.c_str());
			if(script != NULL && script->ScriptRef == extcommand)
			{
				CA2CT extname(ecd->name.c_str());
				tstring result(_T("extension: "));
				result += extname;
				return result;
			}
		}
	}
	
	return _T("");
}

void COptionsPageKeyboard::updateSelection()
{
	clear();

	int sel = m_list.GetSelectedIndex();
	if(sel != -1)
	{
		CommandDetails* cd = reinterpret_cast<CommandDetails*>( m_list.GetItemData(sel) );
		switch(cd->type)
		{
		case cdtCommand:
			showCommandSelection(cd);
			break;
		case cdtExtended:
			showExtendedSelection(static_cast<ExtendedCommandDetails*>( cd ));
		}
		m_pCurrent = cd;
	}
	else
	{
		m_pCurrent = NULL;
	}

	enableButtons();
}

void COptionsPageKeyboard::showCommandSelection(CommandDetails* cd)
{
	size_t noof_mappings = m_pKeyMap->GetCount();
	const KeyToCommand* mappings = m_pKeyMap->GetMappings();

	for(size_t ixMap(0); ixMap < noof_mappings; ++ixMap)
	{
		if(mappings[ixMap].msg == cd->command)
		{
			int hkmods(0);
			if( mappings[ixMap].modifiers & FALT ) hkmods |= HOTKEYF_ALT;
			if( mappings[ixMap].modifiers & FCONTROL ) hkmods |= HOTKEYF_CONTROL;
			if( mappings[ixMap].modifiers & FSHIFT ) hkmods |= HOTKEYF_SHIFT;
			tstring sc = m_pDispatch->GetShortcutText(mappings[ixMap].key, hkmods);
			int ixLI = m_shortcutlist.AddString(sc.c_str());
			m_shortcutlist.SetItemData(ixLI, ixMap);
		}
	}
}

void COptionsPageKeyboard::showExtendedSelection(ExtendedCommandDetails* cd)
{
	const ExtensionCommands& cmds = m_pKeyMap->GetExtendedMappings();
	
	ScriptRegistry* registry = static_cast<ScriptRegistry*>( g_Context.ExtApp->GetScriptRegistry() );
	Script* script = registry->FindScript(cd->group.c_str(), cd->name.c_str());
	for(ExtensionCommands::const_iterator i = cmds.begin(); i != cmds.end(); ++i)
	{
		if( (*i).second.command == script->ScriptRef )
		{
			tstring sc = m_pDispatch->GetShortcutText( (*i).second.key, AccelToHKMod( (*i).second.modifiers ) );
			int ixLI = m_shortcutlist.AddString(sc.c_str());
			m_shortcutlist.SetItemData(ixLI, (*i).first);
		}
	}
}

void COptionsPageKeyboard::cleanUp()
{
	// Free up...
	for(int i = 0; i < m_list.GetItemCount(); ++i)
	{
		CommandDetails* cd = reinterpret_cast<CommandDetails*>( m_list.GetItemData(i) );
		delete cd;
	}
	m_list.DeleteAllItems();
}

bool COptionsPageKeyboard::currentIsExtended()
{
	if(m_pCurrent != NULL)
		return m_pCurrent->type == cdtExtended;
	return false;
}