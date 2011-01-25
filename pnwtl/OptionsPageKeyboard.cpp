/**
 * @file optionspagekeyboard.cpp
 * @brief Options Dialog Keyboard Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
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
	m_pScintillaMap = new Commands::KeyMap(*dispatcher->GetCurrentScintillaMap());
	m_bDirty = false;
	m_pCurrent = NULL;
}

COptionsPageKeyboard::~COptionsPageKeyboard()
{
	delete m_pKeyMap;
	delete m_pScintillaMap;
}

void COptionsPageKeyboard::OnOK()
{
	if(m_bDirty) {
		m_pDispatch->SetCurrentKeyMap(m_pKeyMap);
		m_pDispatch->SetCurrentScintillaMap(m_pScintillaMap);
	}

	if(!m_bCreated)
		return;

	cleanUp();
}

void COptionsPageKeyboard::OnInitialise()
{
}

tstring COptionsPageKeyboard::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_KEYBOARD);
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
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// List control
	m_list.Attach(GetDlgItem(IDC_KB_COMMANDS));
	m_list.SetViewType(LVS_REPORT);
	m_list.AddColumn(LS(IDS_HDR_KEYBOARD_GROUP), 0);
	m_list.AddColumn(LS(IDS_HDR_KEYBOARD_COMMAND), 1);
	m_list.SetColumnWidth(0, 80);
	m_list.SetColumnWidth(1, 300);
	m_list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	// Add items
	CSMenu menu(::LoadMenu(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDR_MDICHILD)));
	CSMenuHandle menuHandle(menu);
	
	addDynamicItems(menuHandle);
	addItems(menuHandle, _T(""), m_list.GetItemCount());
	addExtensions();
	addScintilla();

	// Key controls:
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
	else if (currentIsScintilla())
	{
		m_pScintillaMap->AssignCmdKey(keycode, modifiers, m_pCurrent->command);
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
			else if(currentIsScintilla())
			{
				m_pScintillaMap->RemoveCmdKey(
					m_pScintillaMap->GetMappings()[data].key,
					m_pScintillaMap->GetMappings()[data].modifiers,
					m_pScintillaMap->GetMappings()[data].msg);
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
	tstring command_name;

	int command = m_pKeyMap->Find(keycode, real_modifiers);
	
	if(command == 0)
		command = m_pScintillaMap->Find(keycode, real_modifiers);

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

	*target = _T('\0');
}

/**
 * Add the few items that we know of that are added to menus dynamically
 * meaning that we can't get them from the menu template.
 */
void COptionsPageKeyboard::addDynamicItems(CSMenuHandle& menu)
{
	TCHAR buffer[256];
	TCHAR displayBuffer[256];

	CMenuItemInfo file;
	file.fMask = MIIM_STRING;
	file.dwTypeData = buffer;
	file.cch = 256;
	menu.GetItemInfo(0, &file);

	fixText(buffer, displayBuffer);
	std::wstring group(displayBuffer);
	
	_tcsncpy(buffer, LS(IDS_FILE_NEW), 256);
	fixText(buffer, displayBuffer);
	addItem(0, group.c_str(), displayBuffer, ID_FILE_NEW);

	_tcsncpy(buffer, LS(IDS_FILE_NEW_PROJECT), 256);
	fixText(buffer, displayBuffer);
	addItem(1, group.c_str(), displayBuffer, ID_FILE_NEW_PROJECT);

	_tcsncpy(buffer, LS(IDS_FILE_NEW_WORKSPACE), 256);
	fixText(buffer, displayBuffer);
	addItem(2, group.c_str(), displayBuffer, ID_FILE_NEW_WORKSPACE);
}

void COptionsPageKeyboard::addItem(int index, LPCTSTR group, LPCTSTR item, DWORD command)
{
	int ixItem = m_list.AddItem(index, 0, group);
	m_list.SetItemText(ixItem, 1, item);
			
	// Store info about the command...
	CommandDetails* cd = new CommandDetails;
	cd->type = cdtCommand;
	cd->command = command;
	m_list.SetItemData(ixItem, reinterpret_cast<DWORD_PTR>(cd));
}

int COptionsPageKeyboard::addItems(CSMenuHandle& menu, LPCTSTR group, int count)
{
	TCHAR buffer[256];
	TCHAR displayBuffer[256];
	CMenuItemInfo mii;
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
			tstring newgroup(group);
			if(newgroup.size())
				newgroup += _T(".");
			
			fixText(buffer, displayBuffer);
			
			newgroup += displayBuffer;

			CSMenuHandle subMenu(mii.hSubMenu);
			count = addItems(subMenu, newgroup.c_str(), count);
		}
		else
		{
			fixText(buffer, displayBuffer);
			addItem(count++, group, displayBuffer, mii.wID);
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
		tstring group(LS(IDS_SHORTCUTS_SCRIPTS));
		group += _T(".");
		CA2CT nameConv((*i)->GetName());
		group += nameConv;
		for(script_list_t::const_iterator j = (*i)->GetScripts().begin();
			j != (*i)->GetScripts().end(); ++j)
		{
			CA2CT nameconv((*j)->Name.c_str());

			int ixItem = m_list.AddItem(count++, 0, group.c_str());
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
		case cdtScintilla:
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

	//loop over scintilla as well
	noof_mappings = m_pScintillaMap->GetCount();
	mappings = m_pScintillaMap->GetMappings();

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

bool COptionsPageKeyboard::currentIsScintilla()
{
	if(m_pCurrent != NULL)
		return m_pCurrent->type == cdtScintilla;
	return false;
}

struct EditorCommandTypes{
	TCHAR* name;
	int msg;
};

struct EditorCommandTypes ScintillaStrings[] = {
	{ _T("Line down extend"), SCI_LINEDOWNEXTEND },
	{ _T("Line down"), SCI_LINEDOWN },
	{ _T("Line scroll down"), SCI_LINESCROLLDOWN },
	{ _T("Line down rect extend"), SCI_LINEDOWNRECTEXTEND },
	{ _T("Line up"), SCI_LINEUP },
	{ _T("Line up extend"), SCI_LINEUPEXTEND },
	{ _T("Line scroll up"), SCI_LINESCROLLUP },
	{ _T("Line up rect extend"), SCI_LINEUPRECTEXTEND },
	{ _T("Para up"), SCI_PARAUP },
	{ _T("Para up extend"), SCI_PARAUPEXTEND },
	{ _T("Para down"), SCI_PARADOWN },
	{ _T("Para down extend"), SCI_PARADOWNEXTEND },
	{ _T("Char left"), SCI_CHARLEFT },
	{ _T("Char left extend"), SCI_CHARLEFTEXTEND },
	{ _T("Word left"), SCI_WORDLEFT },
	{ _T("Word left extend"), SCI_WORDLEFTEXTEND },
	{ _T("Char left rect extend"), SCI_CHARLEFTRECTEXTEND },
	{ _T("Char right"), SCI_CHARRIGHT },
	{ _T("Char right extend"), SCI_CHARRIGHTEXTEND },
	{ _T("Word right"), SCI_WORDRIGHT },
	{ _T("Word right extend"), SCI_WORDRIGHTEXTEND },
	{ _T("Char Right Rect Extend"), SCI_CHARRIGHTRECTEXTEND },
	{ _T("Word Part Left"), SCI_WORDPARTLEFT },
	{ _T("Word Part Left Extend"), SCI_WORDPARTLEFTEXTEND },
	{ _T("Word Part Right"), SCI_WORDPARTRIGHT },
	{ _T("Word Part Right Extend"), SCI_WORDPARTRIGHTEXTEND },
	{ _T("VC Home"), SCI_VCHOME },
	{ _T("VC Home Extend"), SCI_VCHOMEEXTEND },
	{ _T("Document Start"), SCI_DOCUMENTSTART },
	{ _T("Document Start Extend"), SCI_DOCUMENTSTARTEXTEND },
	{ _T("Home Display"), SCI_HOMEDISPLAY },
	{ _T("VC Home Rect Extend"), SCI_VCHOMERECTEXTEND },
	{ _T("Line End"), SCI_LINEEND },
	{ _T("Line End Extend"), SCI_LINEENDEXTEND },
	{ _T("Document End"), SCI_DOCUMENTEND },
	{ _T("Document End Extend"), SCI_DOCUMENTENDEXTEND },
	{ _T("Line End Display"), SCI_LINEENDDISPLAY },
	{ _T("Line End Rect Extend"), SCI_LINEENDRECTEXTEND },
	{ _T("Page Up"), SCI_PAGEUP },
	{ _T("Page Up Extend"), SCI_PAGEUPEXTEND },
	{ _T("Page Up Rect Extend"), SCI_PAGEUPRECTEXTEND },
	{ _T("Page Down"), SCI_PAGEDOWN },
	{ _T("Page Down Extend"), SCI_PAGEDOWNEXTEND },
	{ _T("Page Down Rect Extend"), SCI_PAGEDOWNRECTEXTEND },
	{ _T("Clear"), SCI_CLEAR },
//	{ _T("Cut"), SCI_CUT },
	{ _T("Del Word Right"), SCI_DELWORDRIGHT },
	{ _T("Del Line Right"), SCI_DELLINERIGHT },
	{ _T("Edit Toggle Overtype"), SCI_EDITTOGGLEOVERTYPE },
//	{ _T("Paste"), SCI_PASTE },
//	{ _T("Copy"), SCI_COPY },
	{ _T("Cancel"), SCI_CANCEL },
	{ _T("Delete Back"), SCI_DELETEBACK },
	{ _T("Del Word Left"), SCI_DELWORDLEFT },
//	{ _T("Undo"), SCI_UNDO },
	{ _T("Del Line Left"), SCI_DELLINELEFT },
//	{ _T("Redo"), SCI_REDO },
//	{ _T("Select All"), SCI_SELECTALL },
	{ _T("Tab"), SCI_TAB },
	{ _T("Back Tab"), SCI_BACKTAB },
	{ _T("Newline"), SCI_NEWLINE },
	{ _T("Zoom In"), SCI_ZOOMIN },
	{ _T("Zoom Out"), SCI_ZOOMOUT },
	{ _T("Set Zoom"), SCI_SETZOOM },
//	{ _T("Line Cut"), SCI_LINECUT },
//	{ _T("Line Delete"), SCI_LINEDELETE },
//	{ _T("Line Copy"), SCI_LINECOPY },
//	{ _T("Line Transpose"), SCI_LINETRANSPOSE },
	{ _T("Selection Duplicate"), SCI_SELECTIONDUPLICATE },
//	{ _T("Lower Case"), SCI_LOWERCASE },
//	{ _T("Upper Case"), SCI_UPPERCASE },
	{NULL, 0}
};

void COptionsPageKeyboard::addScintilla()
{
	int i = 0;
	int count = m_list.GetItemCount();

	while (ScintillaStrings[i].name)
	{
		int ixItem = m_list.AddItem(count++, 0, LS(IDS_SHORTCUTS_EDITOR));
		m_list.SetItemText(ixItem, 1, ScintillaStrings[i].name);
		
		// Store info about the command...
		CommandDetails* cd = new CommandDetails;
		cd->type = cdtScintilla;
		cd->command = ScintillaStrings[i].msg;
		m_list.SetItemData(ixItem, reinterpret_cast<DWORD_PTR>(cd));
		i++;
	}

}