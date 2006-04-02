#include "stdafx.h"
#include "resource.h"
#include "commands.h"

KeyToCommand DefaultKeyMap[];

typedef enum { ectNormal, ectTool, ectScript } ECommandType;

typedef struct tagCmd {
	ECommandType	Type;
	DWORD			Command;
	char*			Text;
} command_t;

typedef struct tagGroup {
	char*		Text;
	command_t*	Commands;
} group_t;

command_t file_commands[] = {
	{ectNormal,		ID_FILE_OPEN,		"Open"},
	{ectNormal,		ID_FILE_SAVE,		"Save"},
	{ectNormal,		ID_FILE_SAVE_AS,	"Save As"}
};

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

void AccelFromCode(ACCEL& accel, KeyToCommand* cmd)
{
	PNASSERT(cmd != 0);
	//PNASSERT(hotkey != 0);
	accel.cmd = cmd->msg;
	accel.key = cmd->key;
	accel.fVirt = FVIRTKEY | FNOINVERT | cmd->modifiers;
}

KeyToCommand DefaultKeyMap[] = {
	//File
	{K_CTRL,		'N',		ID_FILE_NEW},
	{K_CTRL,		'O',		ID_FILE_OPEN},
	{K_CTRL,		'S',		ID_FILE_SAVE},
	{K_CTRLSHIFT,	'S',		ID_FILE_SAVEALL},
	{K_ALT,			VK_RETURN,	ID_VIEW_FILEPROPERTIES},
	{K_CTRL,		VK_F4,		ID_FILE_CLOSE},
	{K_CTRL,		'P',		ID_FILE_PRINT},

	//Edit
	{K_CTRL,		'Z',		ID_EDIT_UNDO},
	{K_CTRL,		'Y',		ID_EDIT_REDO},
	{K_CTRL,		'X',		ID_EDIT_CUT},
	{K_SHIFT,	    VK_DELETE,	ID_EDIT_CUT},
	{K_CTRL,		'C',		ID_EDIT_COPY},
	{K_CTRL,		VK_INSERT,	ID_EDIT_COPY},
	{K_CTRLALT,		'C',		ID_EDIT_COPYRTF},
	{K_CTRL,		'V',		ID_EDIT_PASTE},
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
	{K_CTRL,		'/',		ID_EDIT_QUICKFIND},
	{K_CTRL,		'G',		ID_EDIT_GOTO},
	{K_ALT,			'G',		ID_EDIT_JUMPTO},

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

	// Tools
	{K_CTRLSHIFT,	'K',		ID_TOOLS_STOPTOOLS},
	{0,				0,			0}
};