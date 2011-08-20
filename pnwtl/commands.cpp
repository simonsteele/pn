/**
 * @file commands.cpp
 * @brief Command Routing Stuff
 * @author Simon Steele
 * @note copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 * 
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "commands.h"
#include "editorcommands.h"
#include "extapp.h"

// Default Keyboard Mapping (predec)
KeyToCommand DefaultKeyMap[];
KeyToCommand DefaultScintillaMap[];

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

static int keyTranslate(int keyIn) {
	switch (keyIn) {
		case VK_DOWN:		return SCK_DOWN;
		case VK_UP:			return SCK_UP;
		case VK_LEFT:		return SCK_LEFT;
		case VK_RIGHT:		return SCK_RIGHT;
		case VK_HOME:		return SCK_HOME;
		case VK_END:		return SCK_END;
		case VK_PRIOR:		return SCK_PRIOR;
		case VK_NEXT:		return SCK_NEXT;
		case VK_DELETE:		return SCK_DELETE;
		case VK_INSERT:		return SCK_INSERT;
		case VK_ESCAPE:		return SCK_ESCAPE;
		case VK_BACK:		return SCK_BACK;
		case VK_TAB:		return SCK_TAB;
		case VK_RETURN:		return SCK_RETURN;
		case VK_ADD:		return SCK_ADD;
		case VK_SUBTRACT:	return SCK_SUBTRACT;
		case VK_DIVIDE:		return SCK_DIVIDE;
		case VK_OEM_2:		return '/';
		case VK_OEM_3:		return '`';
		case VK_OEM_4:		return '[';
		case VK_OEM_5:		return '\\';
		case VK_OEM_6:		return ']';
		default:			return keyIn;
	}
};
 
int CodeToScintilla(const KeyToCommand* cmd)
{
	int scintilla_modifiers(0);

	int modifiers = cmd->modifiers;

	if( modifiers & FALT ) scintilla_modifiers |= SCMOD_ALT;
	if( modifiers & FCONTROL ) scintilla_modifiers |= SCMOD_CTRL;
	if( modifiers & FSHIFT) scintilla_modifiers |= SCMOD_SHIFT;

	return keyTranslate((int)cmd->key) + (scintilla_modifiers << 16);
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

tstring properCase(tstring instr)
{
	tstring::iterator i = instr.begin();
	i++;
	std::transform(i, instr.end(), i, tolower);
	return instr;
}

CommandDispatch::CommandDispatch()
{
	init();
	m_keyMap = new Commands::KeyMap(DefaultKeyMap);
	m_ScintillaKeyMap = new Commands::KeyMap(DefaultScintillaMap);
}

CommandDispatch::CommandDispatch(LPCTSTR kbfile)
{
	init();
	m_keyMap = NULL;
	m_ScintillaKeyMap  = NULL;
	
	if (!Load(kbfile)) 
	{
		m_keyMap = new Commands::KeyMap(DefaultKeyMap);
	}

	if (m_ScintillaKeyMap == NULL)
	{
		m_ScintillaKeyMap = new Commands::KeyMap(DefaultScintillaMap);
	}
}

CommandDispatch::~CommandDispatch()
{
	for(MH_IT i = m_Handlers.begin(); i != m_Handlers.end(); ++i)
	{
		delete (*i).second;
	}

	for(std::list<Commands::EditorCommand*>::const_iterator i = m_editorCommands.begin();
		i != m_editorCommands.end();
		++i)
	{
		delete (*i);
	}
	
	delete m_keyMap;
	delete m_ScintillaKeyMap;
	m_keyMap = NULL;
	m_ScintillaKeyMap = NULL;
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
	LONG lScan = MapVirtualKeyEx(vk, MAPVK_VK_TO_VSC, GetKeyboardLayout(0)) << 16;

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

void CommandDispatch::SetCurrentScintillaMap(const Commands::KeyMap* keyMap)
{
	delete m_ScintillaKeyMap;
	m_ScintillaKeyMap = new Commands::KeyMap(*keyMap);
}


Commands::KeyMap* CommandDispatch::GetCurrentKeyMap() const
{
	return m_keyMap;
}

Commands::KeyMap* CommandDispatch::GetCurrentScintillaMap() const
{
	return m_ScintillaKeyMap;
}

static Commands::KeyMap* readKeyMap(int commands, FILE* kbfile)
{
	KeyToCommand* loadedcmds = new KeyToCommand[commands+1];
	if(fread(loadedcmds, sizeof(KeyToCommand), commands, kbfile) != static_cast<size_t>(commands))
	{
		UNEXPECTED(_T("Failed to load the correct number of commands from the keyboard mappings file."));
		delete loadedcmds;
		return NULL;
	}

	// Set up the end marker
	loadedcmds[commands].key = 0;
	loadedcmds[commands].modifiers = 0;
	loadedcmds[commands].msg = 0;

	Commands::KeyMap* keyMap = new Commands::KeyMap(loadedcmds);
	
	// Don't need the loaded commands any more
	delete loadedcmds;

	return keyMap;
}

bool CommandDispatch::Load(LPCTSTR filename)
{
	int res = 0;
	FILE* kbfile = _tfopen(filename, _T("rb"));
	if(!kbfile)
		return false;

	KeyboardFileHeader hdr = {0};
	KeyboardFileHeaderLengths lengths = {0};
	if(fread(&hdr, KEYBOARD_FILE_HEADER_SIZE, 1, kbfile) != 1 
		|| memcmp(hdr.magic, "PNKEYS", 7) != 0)
	{
		//The magic is bad...
		UNEXPECTED(_T("Keyboard mappings file seems to be corrupt."));
		fclose(kbfile);
		return false;
	}

	if (hdr.version == KEYBOARD_FILE_V1)
	{
		res = fread(&lengths, KEYBOARD_FILE_V1_HEADER_SIZE, 1, kbfile);
	} 
	else if (hdr.version == KEYBOARD_FILE_V2) 
	{
		res = fread(&lengths, KEYBOARD_FILE_V2_HEADER_SIZE, 1, kbfile);
	} 
	else 
	{
		UNEXPECTED(_T("Keyboard mappings file seems to be corrupt."));
		fclose(kbfile);
		return false;
	}

	if (res != 1 || lengths.commands < 1)
	{
		UNEXPECTED(_T("Keyboard mappings file seems to be corrupt."));
		fclose(kbfile);
		return false;
	}

	if (m_keyMap)
		delete m_keyMap;
	
	m_keyMap = readKeyMap(lengths.commands, kbfile);
	
	if (!m_keyMap)
	{
		fclose(kbfile);
		return false;
	}

	if (hdr.version >= KEYBOARD_FILE_V2) // Scintilla enabled
	{ 		
		if (m_ScintillaKeyMap)
			delete m_ScintillaKeyMap;
		
		m_ScintillaKeyMap = readKeyMap(lengths.scintilla, kbfile);
		
		if (!m_ScintillaKeyMap)
		{
			fclose(kbfile);
			return false;
		}
	}

	StoredExtensionCommand* storedexts(NULL);
	if(lengths.extensions > 0)
	{
		storedexts = new StoredExtensionCommand[lengths.extensions];
		if(fread(storedexts, sizeof(StoredExtensionCommand), lengths.extensions, kbfile) != lengths.extensions)
		{
			UNEXPECTED(_T("Failed to load the correct number of extension commands from the keyboard mappings file."));
			delete storedexts;
		}
	}

	fclose(kbfile);

	// Add the extension commands
	if(storedexts)
	{
		for(size_t i = 0; i < lengths.extensions; ++i)
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
	FILE* kbfile = _tfopen(filename, _T("wb"));
	if(!kbfile)
	{
		UNEXPECTED(_T("Failed to open keyboard shortcut configuration file for writing"));
		return;
	}

	KeyboardFileHeader hdr = {0};
	KeyboardFileHeaderLengths lengths = {0};
	memcpy(hdr.magic, "PNKEYS", 7);
	hdr.version = KEYBOARD_FILE_VERSION;
	lengths.commands = m_keyMap->GetCount();
	lengths.scintilla = m_ScintillaKeyMap->GetCount();
	lengths.extensions = m_keyMap->GetExtendedCount();

	fwrite(&hdr, KEYBOARD_FILE_HEADER_SIZE, 1, kbfile);
	fwrite(&lengths, KEYBOARD_FILE_V2_HEADER_SIZE, 1, kbfile);

	const KeyToCommand* mappings = m_keyMap->GetMappings();

	fwrite(mappings, sizeof(KeyToCommand), lengths.commands, kbfile);

	mappings = m_ScintillaKeyMap->GetMappings();

	fwrite(mappings, sizeof(KeyToCommand), lengths.scintilla, kbfile);

	
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

	Commands::GetEditorCommands(m_editorCommands);
	BOOST_FOREACH(Commands::EditorCommand* cmd, m_editorCommands)
	{
		RegisterCallback(cmd->GetCommandID(), NULL, PN_COMMAND_EDITOR, reinterpret_cast<LPVOID>(cmd));
	}
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
	{K_CTRL,		VK_F3,		ID_SEARCH_FINDNEXTCURRENTWORD},
	{K_CTRLSHIFT,	VK_F3,		ID_SEARCH_FINDPREVIOUSCURRENTWORD},
	{K_CTRL,		'R',		ID_EDIT_REPLACE},
	{K_CTRL,		'H',		ID_EDIT_REPLACE},
	{K_CTRLSHIFT,	'F',		ID_EDIT_FINDINFILES},
	{K_CTRL,		VK_OEM_2,	ID_EDIT_QUICKFIND},
	{K_CTRL,		'G',		ID_EDIT_GOTO},
	{K_ALT,			VK_OEM_4,	ID_EDIT_GOTOBRACE},
	{K_ALT,			'G',		ID_EDIT_JUMPTO},
	{K_CTRLSHIFT,	'H',		ID_EDIT_HEADERSWITCH},
	{K_CTRL,		' ',		ID_EDIT_AUTOCOMPLETE},
	{K_CTRLALT,		' ',        ID_EDIT_INSERTCLIP},
	{K_CTRL,		VK_OEM_PERIOD,		ID_COMMENTS_LINE},
	{K_CTRL,		VK_OEM_COMMA,		ID_COMMENTS_UNCOMMENT},
	{K_ALT,			'D',		ID_SELECTION_DUPLICATE},
	{K_ALTSHIFT,	'W',		ID_SELECTION_STRIPTRAILING},
	{K_ALT,			'X',		ID_EDIT_FOCUSCOMMAND},

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
	{K_SHIFT,		VK_ESCAPE,  ID_WINDOWS_CURRENTEDITOR},

	// View | Folding
	{K_CTRLALT,		VK_SUBTRACT,ID_VIEW_COLLAPSEALLFOLDS},
	{K_CTRLALT,		VK_ADD,		ID_VIEW_EXPANDALLFOLDS},
	{K_CTRL,		VK_MULTIPLY,ID_VIEW_TOGGLEFOLD},

	// Tools
	{K_CTRLSHIFT,	'K',		ID_TOOLS_STOPTOOLS},
	{0,				0,			0}
};

KeyToCommand DefaultScintillaMap[] = {
    {K_SHIFT,       VK_DOWN, 	SCI_LINEDOWNEXTEND},
    {0,	            VK_DOWN, 	SCI_LINEDOWN},
    {K_CTRL,	    VK_DOWN, 	SCI_LINESCROLLDOWN},
    {K_ALTSHIFT,    VK_DOWN,    SCI_LINEDOWNRECTEXTEND},
    {0,       	    VK_UP, 		SCI_LINEUP},
    {K_SHIFT,	    VK_UP, 		SCI_LINEUPEXTEND},
    {K_CTRL,	    VK_UP, 		SCI_LINESCROLLUP},
    {K_ALTSHIFT,	VK_UP, 		SCI_LINEUPRECTEXTEND},
    {K_CTRL,	    VK_OEM_4/*'['*/,		SCI_PARAUP},
	{K_CTRLSHIFT,	VK_OEM_4/*'['*/,		SCI_PARAUPEXTEND},
    {K_CTRL,	    VK_OEM_6/*']'*/,		SCI_PARADOWN},
    {K_CTRLSHIFT,	VK_OEM_6/*']'*/,		SCI_PARADOWNEXTEND},
    {0,       	    VK_LEFT,	SCI_CHARLEFT},
    {K_SHIFT,	    VK_LEFT,	SCI_CHARLEFTEXTEND},
    {K_CTRL,	    VK_LEFT,	SCI_WORDLEFT},
    {K_CTRLSHIFT,	VK_LEFT,	SCI_WORDLEFTEXTEND},
    {K_ALTSHIFT,	VK_LEFT,	SCI_CHARLEFTRECTEXTEND},
    {0,       	    VK_RIGHT,	SCI_CHARRIGHT},
    {K_SHIFT,	    VK_RIGHT,	SCI_CHARRIGHTEXTEND},
    {K_CTRL,	    VK_RIGHT,	SCI_WORDRIGHT},
    {K_CTRLSHIFT,	VK_RIGHT,	SCI_WORDRIGHTEXTEND},
    {K_ALTSHIFT,	VK_RIGHT,	SCI_CHARRIGHTRECTEXTEND},
    {K_CTRL,	    VK_OEM_2/*'/'*/,	 	SCI_WORDPARTLEFT},
    {K_CTRLSHIFT,	VK_OEM_2/*'/'*/,		SCI_WORDPARTLEFTEXTEND},
    {K_CTRL,	    VK_OEM_5/*'\\'*/,		SCI_WORDPARTRIGHT},
    {K_CTRLSHIFT,	VK_OEM_5/*'\\'*/,		SCI_WORDPARTRIGHTEXTEND},
    {0,       	    VK_HOME,	SCI_VCHOME},
    {K_SHIFT, 	    VK_HOME, 	SCI_VCHOMEEXTEND},
    {K_CTRL, 	    VK_HOME, 	SCI_DOCUMENTSTART},
    {K_CTRLSHIFT,   VK_HOME, 	SCI_DOCUMENTSTARTEXTEND},
    {K_ALT, 	    VK_HOME, 	SCI_HOMEDISPLAY},
    {K_ALTSHIFT,	VK_HOME,	SCI_VCHOMERECTEXTEND},
    {0,       	    VK_END,	 	SCI_LINEEND},
    {K_SHIFT, 	    VK_END,	 	SCI_LINEENDEXTEND},
    {K_CTRL, 	    VK_END, 	SCI_DOCUMENTEND},
    {K_CTRLSHIFT,   VK_END, 	SCI_DOCUMENTENDEXTEND},
    {K_ALT, 	    VK_END, 	SCI_LINEENDDISPLAY},
    {K_ALTSHIFT,	VK_END,		SCI_LINEENDRECTEXTEND},
    {0,       	    VK_PRIOR,	SCI_PAGEUP},
    {K_SHIFT, 	    VK_PRIOR,	SCI_PAGEUPEXTEND},
    {K_ALTSHIFT,	VK_PRIOR,	SCI_PAGEUPRECTEXTEND},
    {0,        	    VK_NEXT, 	SCI_PAGEDOWN},
    {K_SHIFT, 	    VK_NEXT, 	SCI_PAGEDOWNEXTEND},
    {K_ALTSHIFT,	VK_NEXT,	SCI_PAGEDOWNRECTEXTEND},
    {0,       	    VK_DELETE, 	SCI_CLEAR},
//  {K_SHIFT,	    VK_DELETE, 	SCI_CUT},
    {K_CTRL,	    VK_DELETE, 	SCI_DELWORDRIGHT},
    {K_CTRLSHIFT,   VK_DELETE,	SCI_DELLINERIGHT},
    {0,       	    VK_INSERT, 	SCI_EDITTOGGLEOVERTYPE},
//  {K_SHIFT,	    VK_INSERT, 	SCI_PASTE},
//  {K_CTRL,	    VK_INSERT, 	SCI_COPY},
    {0,       	    VK_ESCAPE,  SCI_CANCEL},
    {0,        	    VK_BACK,	SCI_DELETEBACK},
    {K_SHIFT, 	    VK_BACK,	SCI_DELETEBACK},
    {K_CTRL, 	    VK_BACK,	SCI_DELWORDLEFT},
//  {K_ALT,	        VK_BACK, 	SCI_UNDO},
    {K_CTRLSHIFT,	VK_BACK,	SCI_DELLINELEFT},
//    {K_CTRL,	    'Z', 		SCI_UNDO},
//  {K_CTRL,	    'Y', 		SCI_REDO},
//  {K_CTRL,	    'X', 		SCI_CUT},
//  {K_CTRL,	    'C', 		SCI_COPY},
//  {K_CTRL,	    'V', 		SCI_PASTE},
//  {K_CTRL,	    'A', 		SCI_SELECTALL},
    {0,       	    VK_TAB,		SCI_TAB},
    {K_SHIFT,	    VK_TAB,		SCI_BACKTAB},
    {0,       	    VK_RETURN, 	SCI_NEWLINE},
    {K_SHIFT,	    VK_RETURN, 	SCI_NEWLINE},
    {K_CTRL,	    VK_ADD, 	SCI_ZOOMIN},
    {K_CTRL,	    VK_SUBTRACT,SCI_ZOOMOUT},
    {K_CTRL,	    VK_DIVIDE,	SCI_SETZOOM},
//	{K_CTRL,	    'L', 		SCI_LINECUT},
//	{K_CTRLSHIFT,   'L', 		SCI_LINEDELETE},
//  {K_CTRLSHIFT,   'T', 		SCI_LINECOPY},
//  {K_CTRL,	    'T', 		SCI_LINETRANSPOSE},
    {K_CTRL,	    'D', 		SCI_SELECTIONDUPLICATE},
//  {K_CTRL,	    'U', 		SCI_LOWERCASE},
//  {K_CTRLSHIFT,   'U', 		SCI_UPPERCASE},
    {0,0,0},
};