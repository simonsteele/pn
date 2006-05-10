/**
 * @file commands.cpp
 * @brief Command Routing Stuff
 * @author Simon Steele
 * @note copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "commands.h"

// Default Keyboard Mapping (predec)
KeyToCommand DefaultKeyMap[];

// Available Command ID ranges:
CmdIDRange IDRange1 = {20000, 21000, 0};
CmdIDRange* CommandDispatch::s_IDs[] = {&IDRange1, NULL};

void AccelFromCode(ACCEL& accel, KeyToCommand* cmd)
{
	PNASSERT(cmd != 0);
	//PNASSERT(hotkey != 0);
	accel.cmd = cmd->msg;
	accel.key = cmd->key;
	accel.fVirt = FVIRTKEY | FNOINVERT | cmd->modifiers;
}

KeyMap::KeyMap() : kmap(0), len(0), alloc(0) {
	for (int i = 0; DefaultKeyMap[i].key; i++) {
		AssignCmdKey(DefaultKeyMap[i].key,
			DefaultKeyMap[i].modifiers,
			DefaultKeyMap[i].msg);
	}
}

KeyMap::~KeyMap() {
	Clear();
}

void KeyMap::Clear() {
	delete []kmap;
	kmap = 0;
	len = 0;
	alloc = 0;
}

void KeyMap::AssignCmdKey(int key, int modifiers, unsigned int msg) {
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
	for (int keyIndex = 0; keyIndex < len; keyIndex++) {
		if ((key == kmap[keyIndex].key) && (modifiers == kmap[keyIndex].modifiers)) {
			kmap[keyIndex].msg = msg;
			return;
		}
	}
	kmap[len].key = key;
	kmap[len].modifiers = modifiers;
	kmap[len].msg = msg;
	len++;
}

unsigned int KeyMap::Find(int key, int modifiers) {
	for (int i = 0; i < len; i++) {
		if ((key == kmap[i].key) && (modifiers == kmap[i].modifiers)) {
			return kmap[i].msg;
		}
	}
	return 0;
}

int KeyMap::GetCount() const
{
	return len;
}

int KeyMap::MakeAccelerators(ACCEL* buffer) const
{
	for(int i = 0; i < len; ++i)
	{
		AccelFromCode(buffer[i], &kmap[i]);
	}

	return len;
}

/////////////////////////////////////////////////////////////////////////////
// CommandDispatch

CommandDispatch::CommandDispatch()
{
	int nRanges = 0;

	while(s_IDs[nRanges] != NULL)
	{
		nRanges++;
	}
	
	m_iRanges	= nRanges;
	m_pRange	= s_IDs[0];

	m_keyMap = new KeyMap();
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
	int required = m_keyMap->GetCount();
	ACCEL* accelerators = new ACCEL[required];
	
	// Get the accelerators from the standard command table...
	int offset = m_keyMap->MakeAccelerators(accelerators);

	HACCEL table = ::CreateAcceleratorTable(accelerators, required);

	delete [] accelerators;

	return table;
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

KeyMap* CommandDispatch::GetCurrentKeyMap() const
{
	return m_keyMap;
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
	{K_CTRL,		VK_F4,		ID_FILE_CLOSE},
	{K_CTRL,		'W',		ID_FILE_CLOSE},
	{K_CTRL,		'P',		ID_FILE_PRINT},

	//Edit
	{K_CTRL,		'Z',		ID_EDIT_UNDO},
	{K_ALT,			VK_BACK,	ID_EDIT_UNDO},
	{K_CTRL,		'Y',		ID_EDIT_REDO},
	{K_CTRL,		'X',		ID_EDIT_CUT},
	{K_CTRLSHIFT,	'X',		ID_EDIT_CUT},
	{K_SHIFT,	    VK_DELETE,	ID_EDIT_CUT},
	{K_CTRL,		'C',		ID_EDIT_COPY},
	{K_CTRL,		VK_INSERT,	ID_EDIT_COPY},
	{K_CTRLALT,		'C',		ID_EDIT_COPYRTF},
	{K_CTRL,		'V',		ID_EDIT_PASTE},
	{K_CTRLSHIFT,	'V',		ID_EDIT_PASTE},
	{K_SHIFT,		VK_INSERT,	ID_EDIT_PASTE},
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

	// Bookmarks
	{K_CTRL,		VK_F2,		ID_BOOKMARKS_TOGGLE},
	{0,				VK_F2,		ID_BOOKMARKS_NEXT},
	{K_CTRL,		'K',		ID_BOOKMARKS_NUMBERED_SET},
	{K_CTRL,		'Q',		ID_BOOKMARKS_NUMBERED_JUMP},

	// Folding
	{K_CTRL,		'*',		ID_VIEW_TOGGLEFOLD},
	{K_CTRLALT,		'-',		ID_VIEW_COLLAPSEALLFOLDS},
	{K_CTRLALT,		'+',		ID_VIEW_EXPANDALLFOLDS},

	// View
	{0,				VK_F8,		ID_VIEW_OUTPUT},
	{K_SHIFT,		VK_F8,		ID_VIEW_INDIVIDUALOUTPUT},
	{0,				VK_F6,		ID_NEXT_PANE},
	{K_SHIFT,		VK_F6,		ID_PREV_PANE},
	{K_CTRLALT,		VK_SUBTRACT,ID_VIEW_COLLAPSEALLFOLDS},
	{K_CTRLALT,		VK_ADD,		ID_VIEW_EXPANDALLFOLDS},
	{K_ALT,			VK_RETURN,	ID_VIEW_FILEPROPERTIES},
	{K_CTRL,		VK_MULTIPLY,ID_VIEW_TOGGLEFOLD},
	{K_ALT,			'G',		ID_VIEW_WINDOWS_CTAGS},

	// Tools
	{K_CTRLSHIFT,	'K',		ID_TOOLS_STOPTOOLS},
	{0,				0,			0}
};