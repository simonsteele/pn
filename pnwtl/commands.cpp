/**
 * @file commands.cpp
 * @brief Command Routing Stuff
 * @author Simon Steele
 * @note copyright (c) 2006 Simon Steele - http://untidy.net/
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "commands.h"
#include "extapp.h"

// Default Keyboard Mapping (predec)
KeyToCommand DefaultKeyMap[];

// Available Command ID ranges:
CmdIDRange IDRange1 = {20000, 21000, 0};
CmdIDRange* CommandDispatch::s_IDs[] = {&IDRange1, NULL};

void AccelFromCode(ACCEL& accel, const KeyToCommand* cmd)
{
	PNASSERT(cmd != 0);
	//PNASSERT(hotkey != 0);
	accel.cmd = cmd->msg;
	accel.key = cmd->key;
	accel.fVirt = FVIRTKEY | cmd->modifiers;
}

WORD HKToAccelMod(WORD modifiers)
{
	WORD real_modifiers(0);

	if( modifiers & HOTKEYF_ALT ) real_modifiers |= FALT;
	if( modifiers & HOTKEYF_CONTROL ) real_modifiers |= FCONTROL;
	if( modifiers & HOTKEYF_SHIFT) real_modifiers |= FSHIFT;

	return real_modifiers;
}

WORD AccelToHKMod(WORD modifiers)
{
	WORD real_modifiers(0);

	if( modifiers & FALT ) real_modifiers |= HOTKEYF_ALT;
	if( modifiers & FCONTROL ) real_modifiers |= HOTKEYF_CONTROL;
	if( modifiers & FSHIFT ) real_modifiers |= HOTKEYF_SHIFT;
	
	return real_modifiers;
}

namespace Commands
{

KeyMap::KeyMap(KeyToCommand* commands) : kmap(0), len(0), alloc(0), lastdispatcher(0)
{
	for (int i = 0; commands[i].key; i++)
	{
		internalAssign(commands[i].key,
			commands[i].modifiers,
			commands[i].msg);
	}
}

KeyMap::KeyMap(const KeyMap& copy) : kmap(0), len(0), alloc(0), lastdispatcher(0)
{
	len = copy.len;
	if(len)
	{
		KeyToCommand* kNew = new KeyToCommand[copy.len+5];
		if(!kNew)
			return;
		for (int k = 0; k < len; k++)
			kNew[k] = copy.kmap[k];
		
		alloc = copy.len+5;
		kmap = kNew;
	}
	
	// Copy extension commands
	extkmap = copy.extkmap;
	for(ExtensionCommands::iterator i = extkmap.begin(); i != extkmap.end(); ++i)
	{
		// Clear out any commands - we haven't registered them yet!
		(*i).second.msg = 0;
	}
}

KeyMap::~KeyMap()
{
	Clear();
}

void KeyMap::Clear()
{
	delete []kmap;
	kmap = 0;
	len = 0;
	alloc = 0;
	
	// Release command IDs.
	if(lastdispatcher != NULL)
	{
		for(ExtensionCommands::const_iterator i = extkmap.begin(); i != extkmap.end(); ++i)
		{
			if((*i).second.msg != 0)
				lastdispatcher->UnRegisterCallback((*i).second.msg);
		}
	}
}

void KeyMap::AssignCmdKey(int key, int modifiers, unsigned int msg)
{
	// Check there's no extended command with this setup...
	RemoveExtended(key, modifiers);
	// ...and assign
	internalAssign(key, modifiers, msg);
}

void KeyMap::RemoveCmdKey(int key, int modifiers, unsigned int msg)
{
	bool found = false;
	for(int ixArr(0); ixArr < len; ++ixArr)
	{
		if(!found && kmap[ixArr].key == key && kmap[ixArr].modifiers == modifiers && kmap[ixArr].msg == msg)
		{
			found = true;
		}
		if(found && ixArr != len-1)
		{
			kmap[ixArr] = kmap[ixArr+1];
		}
	}
	
	if(found)
		len--;
}

void KeyMap::AddExtended(const ExtensionCommand& command)
{
	// Make sure there's no classic command on this id already.
	unsigned int cmd = Find(command.key, command.modifiers);
	if(cmd != 0)
		RemoveCmdKey(command.key, command.modifiers, cmd);

	// Insert it
	extkmap.insert( ExtensionCommands::value_type( command.key | (command.modifiers << 8), command ) );
}

void KeyMap::RemoveExtended(unsigned char key, unsigned char modifiers)
{
	ExtensionCommands::iterator i = extkmap.find( key | (modifiers << 8) );
	if(i != extkmap.end())
		extkmap.erase( i );
}

unsigned int KeyMap::Find(int key, int modifiers)
{
	for (int i = 0; i < len; i++)
	{
		if ((key == kmap[i].key) && (modifiers == kmap[i].modifiers))
		{
			return kmap[i].msg;
		}
	}
	return 0;
}

const ExtensionCommand* KeyMap::FindExtended(unsigned char key, unsigned char modifiers)
{
	ExtensionCommands::const_iterator i = extkmap.find( key | (modifiers << 8) );
	if( i != extkmap.end() )
		return &(*i).second;
	return NULL;
}

size_t KeyMap::GetCount() const
{
	return len;
}

size_t KeyMap::GetExtendedCount() const
{
	return extkmap.size();
}

const KeyToCommand* KeyMap::GetMappings() const
{
	return kmap;
}

const ExtensionCommands& KeyMap::GetExtendedMappings() const
{
	return extkmap;
}

int KeyMap::MakeAccelerators(ACCEL* buffer, CommandDispatch* dispatcher)
{
	size_t i(0);
	for(; i < (size_t)len; ++i)
	{
		AccelFromCode(buffer[i], &kmap[i]);
	}

	for(ExtensionCommands::iterator j = extkmap.begin();
		j != extkmap.end(); ++j)
	{
		if( (*j).second.msg == 0 )
		{
			(*j).second.msg = dispatcher->RegisterCallback(-1, dispatcher, COMMANDS_RUNEXT, reinterpret_cast<LPVOID>( (*j).first ) );
		}

		AccelFromCode(buffer[i++], &((*j).second));
	}

	return i;
}

void KeyMap::internalAssign(int key, int modifiers, unsigned int msg)
{
	if ((len+1) >= alloc) {
		KeyToCommand *ktcNew = new KeyToCommand[alloc + 5];
		if (!ktcNew)
			return;
		for (int k = 0; k < len; k++)
			ktcNew[k] = kmap[k];
		alloc += 5;
		delete []kmap;
		kmap = ktcNew;
	}
	for (int keyIndex = 0; keyIndex < len; keyIndex++)
	{
		if ((key == kmap[keyIndex].key) && (modifiers == kmap[keyIndex].modifiers))
		{
			kmap[keyIndex].msg = msg;
			return;
		}
	}
	kmap[len].key = key;
	kmap[len].modifiers = modifiers;
	kmap[len].msg = msg;
	len++;
}

} // namespace Commands

/////////////////////////////////////////////////////////////////////////////
// CommandDispatch

std::string properCase(std::string instr)
{
	std::string::iterator i = instr.begin();
	i++;
	std::transform(i, instr.end(), i, tolower);
	return instr;
}

CommandDispatch::CommandDispatch()
{
	init();
	m_keyMap = new Commands::KeyMap(DefaultKeyMap);
}

CommandDispatch::CommandDispatch(LPCTSTR kbfile)
{
	init();
	m_keyMap = NULL;
	if(!Load(kbfile))
		m_keyMap = new Commands::KeyMap(DefaultKeyMap);
}

CommandDispatch::~CommandDispatch()
{
	for(MH_IT i = m_Handlers.begin(); i != m_Handlers.end(); ++i)
	{
		delete (*i).second;
	}

	delete m_keyMap;
	m_keyMap = NULL;
}

HACCEL CommandDispatch::GetAccelerators()
{
	size_t required = m_keyMap->GetCount() + m_keyMap->GetExtendedCount();
	ACCEL* accelerators = new ACCEL[required];
	
	// Get the accelerators from the standard command table...
	m_keyMap->MakeAccelerators(accelerators, this);

	HACCEL table = ::CreateAcceleratorTable(accelerators, required);

	delete [] accelerators;

	return table;
}

void CommandDispatch::UpdateMenuShortcuts(HMENU theMenu)
{
	CSMenuHandle menu(theMenu);

	TCHAR buffer[255];

	CMenuItemInfo mii;
	mii.fMask = MIIM_ID | MIIM_STRING;
	mii.dwTypeData = buffer;
	mii.cch = 255;

	Commands::KeyMap* theKeys = GetCurrentKeyMap();
	const KeyToCommand* keyArr = theKeys->GetMappings();
	for(size_t i(0); i < theKeys->GetCount(); i++)
	{
		mii.cch = 255;
		menu.GetItemInfo(keyArr[i].msg, &mii, FALSE);
		if(mii.wID == keyArr[i].msg)
		{
			// Remove any previous accelerator
			TCHAR* tabpos = _tcschr(buffer, _T('\t'));
			if(tabpos)
				*tabpos = NULL;

			// Set the new one
			tstring shortcut = _T('\t') + GetShortcutText(keyArr[i].key, ::AccelToHKMod(keyArr[i].modifiers));
			_tcscat(mii.dwTypeData, shortcut.c_str());
			::SetMenuItemInfo(menu, mii.wID, FALSE, &mii);
		}
	}
}

tstring CommandDispatch::GetShortcutText(int wCode, int wModifiers)
{
	tstring strKeyName;

	if (wCode != 0 || wModifiers != 0)
	{
		if (wModifiers & HOTKEYF_CONTROL)
		{
			strKeyName += m_keyNameCtrl;
			strKeyName += _T("+");
		}

		if (wModifiers & HOTKEYF_SHIFT)
		{
			strKeyName += m_keyNameShift;
			strKeyName += _T("+");
		}

		if (wModifiers & HOTKEYF_ALT)
		{
			strKeyName += m_keyNameAlt;
			strKeyName += _T("+");
		}

		switch(wCode) {
			// Keys which are "extended" (except for Return which is Numeric Enter as extended)
			case VK_INSERT:
			case VK_DELETE:
			case VK_HOME:
			case VK_END:
			case VK_NEXT:  // Page down
			case VK_PRIOR: // Page up
			case VK_LEFT:
			case VK_RIGHT:
			case VK_UP:
			case VK_DOWN:
				wModifiers |= HOTKEYF_EXT; // Add extended bit
		}	

		strKeyName += GetKeyName(wCode, (wModifiers & HOTKEYF_EXT) != 0);
	}

	return strKeyName;
}

tstring CommandDispatch::GetKeyName(UINT vk, bool extended)
{
	LONG lScan = MapVirtualKeyEx(vk, 0, GetKeyboardLayout(0)) << 16;

	// if it's an extended key, add the extended flag
	if (extended)
		lScan |= 0x01000000L;

	tstring str;
	GArray<TCHAR> tcbuf;
	
	int nBufferLen = 64;
	int nLen;
	do
	{
		nBufferLen *= 2;
		tcbuf.grow(nBufferLen);
		nLen = ::GetKeyNameText(lScan, &tcbuf[0], nBufferLen + 1);
	}
	while (nLen == nBufferLen);

	if(nLen > 1)
	{
		return properCase(tstring(&tcbuf[0]));
	}
	else
	{
		return tstring(&tcbuf[0]);
	}
}

/**
 * @return int The actual ID of the registered command
 */
int CommandDispatch::RegisterCallback(CommandEventHandler *pHandler, int iCommand, LPVOID data)
{
	return RegisterCallback(-1, pHandler, iCommand, data);
}

/**
 * @return int The actual ID of the registered command
 * @param iRealCommand set to -1 to generate a command id.
 */
int CommandDispatch::RegisterCallback(int iRealCommand, CommandEventHandler* pHandler, int iMappedCommand, LPVOID data)
{
	if(iRealCommand == -1)
		iRealCommand = GetNextID();

	EventRecord* pRecord = new EventRecord;
	pRecord->iID = iMappedCommand;
	pRecord->pHandler = pHandler;
	pRecord->data = data;

	m_Handlers.insert(m_Handlers.begin(), MH_VT(iRealCommand, pRecord));

	return iRealCommand;
}

void CommandDispatch::UnRegisterCallback(int iID)
{
	MH_IT i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		delete (*i).second;
		m_Handlers.erase(i);
		m_freeIds.push(iID);
	}
}

int CommandDispatch::GetNextID()
{
	if(m_freeIds.size())
	{
		int id = m_freeIds.top();
		m_freeIds.pop();
		return id;
	}

	if(m_pRange->current == 0)
	{
		m_pRange->current = m_pRange->start;
	}

	int ret = m_pRange->current;

	if(++m_pRange->current > m_pRange->end)
	{
		m_pRange++;
	}
	
	return ret;
}

void CommandDispatch::ReturnID(int id)
{
	m_freeIds.push(id);
}

/**
 * This method calls the predefined handler for a command.
 */
bool CommandDispatch::HandleCommand(int iID)
{
	bool bHandled = false;

	MH_CI i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		EventRecord* pRecord = (*i).second;
		if(pRecord->pHandler)
		{
			bHandled = pRecord->pHandler->SHandleDispatchedCommand(pRecord->iID, pRecord->data);
		}
	}

	return bHandled;
}

/**
 * This method specifies the handler for the command in the call, 
 * and doesn't look it up from the handler map.
 */
bool CommandDispatch::LocalHandleCommand(int iID, int iCommand, CommandEventHandler* pHandler)
{
	bool bHandled = false;

	MH_CI i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		EventRecord* pRecord = (*i).second;
		if(pRecord->iID == iCommand)
		{
			bHandled = pHandler->SHandleDispatchedCommand(pRecord->iID, pRecord->data);
		}
	}

	return bHandled;
}

void CommandDispatch::SetCurrentKeyMap(const Commands::KeyMap* keyMap)
{
	delete m_keyMap;
	m_keyMap = new Commands::KeyMap(*keyMap);
}

Commands::KeyMap* CommandDispatch::GetCurrentKeyMap() const
{
	return m_keyMap;
}

bool CommandDispatch::Load(LPCTSTR filename)
{
	FILE* kbfile = fopen(filename, "rb");
	if(!kbfile)
		return false;

	KeyboardFileHeader hdr = {0};
	if(fread(&hdr, sizeof(KeyboardFileHeader), 1, kbfile) != 1 
		|| memcmp(hdr.magic, "PNKEYS", 7) != 0
		|| hdr.commands < 1)
	{
		//The magic is bad...
		UNEXPECTED("Keyboard mappings file seems to be corrupt.");
		fclose(kbfile);
		return false;
	}

	KeyToCommand* loadedcmds = new KeyToCommand[hdr.commands+1];
	if(fread(loadedcmds, sizeof(KeyToCommand), hdr.commands, kbfile) != hdr.commands)
	{
		UNEXPECTED("Failed to load the correct number of commands from the keyboard mappings file.");
		fclose(kbfile);
		delete loadedcmds;
		return false;
	}

	StoredExtensionCommand* storedexts(NULL);
	if(hdr.extensions > 0)
	{
		storedexts = new StoredExtensionCommand[hdr.extensions];
		if(fread(storedexts, sizeof(StoredExtensionCommand), hdr.extensions, kbfile) != hdr.extensions)
		{
			UNEXPECTED("Failed to load the correct number of extension commands from the keyboard mappings file.");
			delete storedexts;
		}
	}

	fclose(kbfile);
	
	// Set up the end marker
	loadedcmds[hdr.commands].key = 0;
	loadedcmds[hdr.commands].modifiers = 0;
	loadedcmds[hdr.commands].msg = 0;

	// Load up the key map
	if(m_keyMap != NULL)
		delete m_keyMap;
	m_keyMap = new Commands::KeyMap(loadedcmds);
	
	// Don't need the loaded commands any more
	delete loadedcmds;

	// Add the extension commands
	if(storedexts)
	{
		for(size_t i = 0; i < hdr.extensions; ++i)
		{
			ExtensionCommand cmd;
			cmd.command = storedexts[i].command;
			cmd.key = storedexts[i].key;
			cmd.modifiers = storedexts[i].modifiers;
			cmd.msg = 0;

			m_keyMap->AddExtended(cmd);
		}

		delete [] storedexts;
	}

	return true;
}

void CommandDispatch::Save(LPCTSTR filename) const
{
	FILE* kbfile = fopen(filename, "wb");
	if(!kbfile)
	{
		UNEXPECTED("Failed to open keyboard shortcut configuration file for writing");
		return;
	}

	KeyboardFileHeader hdr = {0};
	memcpy(hdr.magic, "PNKEYS", 7);
	hdr.version = KEYBOARD_FILE_VERSION;
	hdr.commands = m_keyMap->GetCount();
	hdr.extensions = m_keyMap->GetExtendedCount();

	fwrite(&hdr, sizeof(KeyboardFileHeader), 1, kbfile);

	const KeyToCommand* mappings = m_keyMap->GetMappings();

	fwrite(mappings, sizeof(KeyToCommand), hdr.commands, kbfile);

	//Write shortcuts for scripts etc.
	const ExtensionCommands& extcmds = m_keyMap->GetExtendedMappings();
	for(ExtensionCommands::const_iterator i = extcmds.begin();
		i != extcmds.end();
		++i)
	{
		StoredExtensionCommand sec = { (*i).second.modifiers, (*i).second.key, 0 };
		strncpy(sec.command, (*i).second.command.c_str(), 149);
		fwrite(&sec, sizeof(StoredExtensionCommand), 1, kbfile);
	}

	fclose(kbfile);
}

bool CommandDispatch::SHandleDispatchedCommand(int iCommand, LPVOID data)
{
	if(iCommand == COMMANDS_RUNEXT)
	{
		// We've got a keyboard-shortcut event for an extension command:
		int keycmd = reinterpret_cast<int>(data);
		const ExtensionCommand* cmd = m_keyMap->FindExtended(keycmd & 0x00ff, (keycmd & 0xff00) >> 8);
		
		// Run the command...
		g_Context.ExtApp->RunExtensionCommand( cmd->command.c_str() );

		return true;
	}
	
	return false;
}

void CommandDispatch::init()
{
	int nRanges = 0;

	while(s_IDs[nRanges] != NULL)
	{
		nRanges++;
	}
	
	m_iRanges	= nRanges;
	m_pRange	= s_IDs[0];

	// We don't like Windows' ugly default names, but that's ok because
	// GetKeyName proper cases them. However, to avoid the cost of doing
	// that all the time for these common cases we cache them.
	m_keyNameCtrl = GetKeyName(VK_CONTROL, false);
	m_keyNameAlt = GetKeyName(VK_MENU, false);
	m_keyNameShift = GetKeyName(VK_SHIFT, false);
}

/////////////////////////////////////////////////////////////////////////////
// DefaultKeyMap

KeyToCommand DefaultKeyMap[] = {
	//File
	{K_CTRL,		'N',		ID_FILE_NEW},
	{K_CTRL,		'O',		ID_FILE_OPEN},
	{K_CTRL,		'S',		ID_FILE_SAVE},
	{K_CTRLSHIFT,	'S',		ID_FILE_SAVEALL},
	{K_ALT,			VK_RETURN,	ID_VIEW_FILEPROPERTIES},
	{K_CTRL,		'W',		ID_FILE_CLOSE},
	{K_CTRL,		VK_F4,		ID_FILE_CLOSE},
	{K_CTRL,		'P',		ID_FILE_PRINT},

	//Edit
	{K_ALT,			VK_BACK,	ID_EDIT_UNDO},
	{K_CTRL,		'Z',		ID_EDIT_UNDO},
	{K_CTRL,		'Y',		ID_EDIT_REDO},
	{K_CTRLSHIFT,	'X',		ID_EDIT_CUT},
	{K_SHIFT,	    VK_DELETE,	ID_EDIT_CUT},
	{K_CTRL,		'X',		ID_EDIT_CUT},
	{K_CTRL,		VK_INSERT,	ID_EDIT_COPY},
	{K_CTRL,		'C',		ID_EDIT_COPY},
	{K_CTRLALT,		'C',		ID_EDIT_COPYRTF},
	{K_CTRLSHIFT,	'V',		ID_EDIT_PASTE},
	{K_SHIFT,		VK_INSERT,	ID_EDIT_PASTE},
	{K_CTRL,		'V',		ID_EDIT_PASTE},
	{K_CTRLSHIFT,	'C',		ID_EDIT_CLIPBOARDSWAP},
	{K_CTRL,		'L',		ID_EDIT_CUTLINE},
	{K_CTRLSHIFT,	'T',		ID_EDIT_COPYLINE},
	{K_CTRLSHIFT,	'L',		ID_EDIT_DELETELINE},
	{K_CTRL,		'D',		ID_EDIT_DUPLICATELINE},
	{K_CTRL,		'T',		ID_EDIT_TRANSPOSELINES},
	{K_CTRL,		'U',		ID_EDIT_LOWERCASE},
	{K_CTRLSHIFT,	'U',		ID_EDIT_UPPERCASE},
	{K_CTRL,		'A',		ID_EDIT_SELECTALL},
	{K_CTRL,		'F',		ID_EDIT_FIND},
	{0,				VK_F3,		ID_EDIT_FINDNEXT},
	{K_SHIFT,		VK_F3,		ID_EDIT_FINDPREVIOUS},
	{K_CTRL,		'R',		ID_EDIT_REPLACE},
	{K_CTRL,		'H',		ID_EDIT_REPLACE},
	{K_CTRLSHIFT,	'F',		ID_EDIT_FINDINFILES},
	{K_CTRL,		VK_OEM_2,	ID_EDIT_QUICKFIND},
	{K_CTRL,		'G',		ID_EDIT_GOTO},
	{K_CTRL,		VK_OEM_4,	ID_EDIT_GOTOBRACE},
	{K_ALT,			'G',		ID_EDIT_JUMPTO},
	{K_CTRLSHIFT,	'H',		ID_EDIT_HEADERSWITCH},
	{K_CTRL,		' ',		ID_EDIT_AUTOCOMPLETE},
	{K_CTRL,		VK_OEM_PERIOD,		ID_COMMENTS_LINE},
	{K_CTRL,		VK_OEM_COMMA,		ID_COMMENTS_UNCOMMENT},

	// Bookmarks
	{K_CTRL,		VK_F2,		ID_BOOKMARKS_TOGGLE},
	{0,				VK_F2,		ID_BOOKMARKS_NEXT},
	{K_SHIFT,		VK_F2,		ID_BOOKMARKS_PREVIOUS},
	{K_CTRL,		'K',		ID_BOOKMARKS_NUMBERED_SET},
	{K_CTRL,		'Q',		ID_BOOKMARKS_NUMBERED_JUMP},

	// View
	{0,				VK_F8,		ID_VIEW_OUTPUT},
	{K_SHIFT,		VK_F8,		ID_VIEW_INDIVIDUALOUTPUT},
	{0,				VK_F6,		ID_NEXT_PANE},
	{K_SHIFT,		VK_F6,		ID_PREV_PANE},
	{K_ALT,			VK_RETURN,	ID_VIEW_FILEPROPERTIES},
	{K_ALT,			VK_F6,		ID_VIEW_WINDOWS_PROJECT},
	{K_ALT,			VK_F7,		ID_VIEW_WINDOWS_TEXTCLIPS},
	{K_ALT,			VK_F8,		ID_VIEW_WINDOWS_FINDRESULTS},
	{K_ALT,			VK_F9,		ID_VIEW_WINDOWS_CTAGS},
	{K_ALT,			VK_F10,		ID_VIEW_WINDOWS_SCRIPTS},
	//{K_ALT,			'G',		ID_VIEW_WINDOWS_CTAGS}, - Alt G to go back to Jump To!

	// View | Folding
	{K_CTRLALT,		VK_SUBTRACT,ID_VIEW_COLLAPSEALLFOLDS},
	{K_CTRLALT,		VK_ADD,		ID_VIEW_EXPANDALLFOLDS},
	{K_CTRL,		VK_MULTIPLY,ID_VIEW_TOGGLEFOLD},

	// Tools
	{K_CTRLSHIFT,	'K',		ID_TOOLS_STOPTOOLS},
	{0,				0,			0}
};