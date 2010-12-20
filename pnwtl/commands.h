/**
 * @file commands.h
 * @brief Command Routing Stuff
 * @author Simon Steele
 * @note copyright (c) 2006 Simon Steele - http://untidy.net/
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef commands_h__included
#define commands_h__included

#define K_ALT		FALT
#define K_CTRL		FCONTROL
#define K_SHIFT		FSHIFT
#define K_CTRLALT	(K_ALT | K_CTRL)
#define K_CTRLSHIFT	(K_SHIFT | K_CTRL)
#define K_ALTSHIFT	(K_SHIFT | K_ALT)

WORD HKToAccelMod(WORD modifiers);
WORD AccelToHKMod(WORD modifiers);

/**
 * Simple keyboard command struct
 */
struct KeyToCommand
{
	unsigned char modifiers;
	unsigned char key;
	unsigned int msg;
};

int CodeToScintilla(const KeyToCommand* cmd);

/**
 * extension command for storage in the keyboard file
 */
struct StoredExtensionCommand
{
	unsigned char modifiers;
	unsigned char key;
	char command[150];
};

/**
 * This is for commands that have a command string (like scripts)
 */
class ExtensionCommand : public KeyToCommand
{
public:
	std::string command;
};

typedef std::map<unsigned short, ExtensionCommand> ExtensionCommands;

/**
 * Keyboard settings file header
 */
struct KeyboardFileHeader
{
	unsigned char magic[7];
	unsigned int version;
};

struct KeyboardFileHeaderLengths
{
	unsigned int commands;
	unsigned int extensions;
	unsigned int scintilla;
};

#define KEYBOARD_FILE_V1				1
#define KEYBOARD_FILE_V2				2
#define KEYBOARD_FILE_HEADER_SIZE		sizeof(KeyboardFileHeader)
#define KEYBOARD_FILE_V1_HEADER_SIZE	(2 * sizeof(unsigned int))
#define KEYBOARD_FILE_V2_HEADER_SIZE	sizeof(KeyboardFileHeaderLengths)

#define KEYBOARD_FILE_VERSION			2

class CommandDispatch;

namespace Commands {

class EditorCommand;

/**
 * Simple keyboard command map
 */
class KeyMap {
public:
	KeyMap(KeyToCommand* commands);
	KeyMap(const KeyMap& copy);
	~KeyMap();

	void Clear();
	
	void AssignCmdKey(int key, int modifiers, unsigned int msg);
	void RemoveCmdKey(int key, int modifiers, unsigned int msg);

	void AddExtended(const ExtensionCommand& command);
	void RemoveExtended(unsigned char key, unsigned char modifiers);
	
	unsigned int Find(int key, int modifiers);	// 0 returned on failure
	const ExtensionCommand* FindExtended(unsigned char key, unsigned char modifiers);
	
	size_t GetCount() const;
	size_t GetExtendedCount() const;
	
	const KeyToCommand* GetMappings() const;
	const ExtensionCommands& GetExtendedMappings() const;
	
	int MakeAccelerators(ACCEL* buffer, CommandDispatch* dispatcher);

private:
	void internalAssign(int key, int modifiers, unsigned int msg);

	KeyToCommand *kmap;
	CommandDispatch *lastdispatcher;
	ExtensionCommands extkmap;
	int len;
	int alloc;
	static const KeyToCommand MapDefault[];
};

}

typedef std::stack<DWORD> IDStack;

/**
 * @class CommandEventHandler
 * @brief Mixin class for implementing custom menu generation and handling...
 */
class CommandEventHandler
{
	public:
		virtual bool SHandleDispatchedCommand(int iCommand, LPVOID data) = 0;
};

typedef struct
{
	int start;
	int end;
	int current;
} CmdIDRange;

typedef struct
{
	CommandEventHandler*	pHandler;
	int					iID;
	LPVOID				data;
} EventRecord;

typedef std::map<int, EventRecord*> MAP_HANDLERS;
typedef MAP_HANDLERS::value_type MH_VT;
typedef MAP_HANDLERS::iterator MH_IT;
typedef MAP_HANDLERS::const_iterator MH_CI;

class CommandDispatch : public CommandEventHandler
{
	public:
		CommandDispatch();
		CommandDispatch(LPCTSTR kbfile);
		~CommandDispatch();

		HACCEL GetAccelerators();

		void UpdateMenuShortcuts(HMENU menu);

		tstring GetShortcutText(int wCode, int wModifiers);
		tstring GetKeyName(UINT vk, bool extended);

		int RegisterCallback(CommandEventHandler* pHandler, int iCommand, LPVOID data = NULL);
		int RegisterCallback(int iRealCommand, CommandEventHandler* pHandler, int iMappedCommand, LPVOID data = NULL);
		void UnRegisterCallback(int iID);

		int GetNextID();
		void ReturnID(int id);

		bool HandleCommand(int iID);
		bool LocalHandleCommand(int iID, int iCommand, CommandEventHandler* pHandler);

		void SetCurrentKeyMap(const Commands::KeyMap* keyMap);
		void SetCurrentScintillaMap(const Commands::KeyMap* keyMap);
		Commands::KeyMap* GetCurrentKeyMap() const;
		Commands::KeyMap* GetCurrentScintillaMap() const;
		
		bool Load(LPCTSTR filename);
		void Save(LPCTSTR filename) const;

// CommandEventHandler
	public:
		virtual bool SHandleDispatchedCommand(int iCommand, LPVOID data);

	private:
		void init();

		std::list<Commands::EditorCommand*> m_editorCommands;

		static CmdIDRange* s_IDs[];
		
		int			m_iRanges;
		CmdIDRange*	m_pRange;
		
		Commands::KeyMap* m_keyMap;
		Commands::KeyMap* m_ScintillaKeyMap;
		IDStack		m_freeIds;

		MAP_HANDLERS m_Handlers;

		tstring		m_keyNameCtrl;
		tstring		m_keyNameAlt;
		tstring		m_keyNameShift;
};


#define ROUTE_MENUCOMMANDS() \
	if(uMsg == WM_COMMAND) \
	{ \
		bHandled = TRUE; \
		if( m_pCmdDispatch->HandleCommand(LOWORD(wParam)) ) \
			return TRUE; \
		else \
			bHandled = FALSE; \
	}

#define LOCAL_MENUCOMMAND(id) \
	if(uMsg == WM_COMMAND /*&& lParam == 0*/) \
	{ \
		bHandled = TRUE; \
		if( m_pCmdDispatch->LocalHandleCommand(LOWORD(wParam), id, this) ) \
			return TRUE; \
		else \
			bHandled = FALSE; \
	}

#define BEGIN_MENU_HANDLER_MAP() \
	bool SHandleDispatchedCommand(int iCommand, LPVOID data) \
	{ 

#define HANDLE_MENU_COMMAND(id, handler) \
	if(id == iCommand) \
	{ \
		return handler(data); \
	}

#define END_MENU_HANDLER_MAP() \
		return false; \
	}

#endif // #ifndef commands_h__included