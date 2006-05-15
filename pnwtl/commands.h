/**
 * @file commands.h
 * @brief Command Routing Stuff
 * @author Simon Steele
 * @note copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
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

/**
 */
struct KeyToCommand {
	unsigned char modifiers;
	unsigned char key;
	unsigned int msg;
};

/**
 */
class KeyMap {
public:
	KeyMap();
	~KeyMap();
	void Clear();
	void AssignCmdKey(int key, int modifiers, unsigned int msg);
	unsigned int Find(int key, int modifiers);	// 0 returned on failure
	int GetCount() const;
	const KeyToCommand* GetMappings() const;
	int MakeAccelerators(ACCEL* buffer) const;
private:
	KeyToCommand *kmap;
	int len;
	int alloc;
	static const KeyToCommand MapDefault[];
};

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

class CommandDispatch : public Singleton<CommandDispatch, true>
{
	friend class Singleton<CommandDispatch, true>;

	public:
		~CommandDispatch();

		HACCEL GetAccelerators();

		int RegisterCallback(CommandEventHandler* pHandler, int iCommand, LPVOID data = NULL);
		int RegisterCallback(int iRealCommand, CommandEventHandler* pHandler, int iMappedCommand, LPVOID data = NULL);
		void UnRegisterCallback(int iID);

		int GetNextID();
		void ReturnID(int id);

		bool HandleCommand(int iID);
		bool LocalHandleCommand(int iID, int iCommand, CommandEventHandler* pHandler);

		KeyMap* GetCurrentKeyMap() const;

	private:
		CommandDispatch();

		static CmdIDRange* s_IDs[];
		
		int			m_iRanges;
		CmdIDRange*	m_pRange;
		
		KeyMap*		m_keyMap;
		IDStack		m_freeIds;

		MAP_HANDLERS m_Handlers;
};


#define ROUTE_MENUCOMMANDS() \
	if(uMsg == WM_COMMAND) \
	{ \
		bHandled = TRUE; \
		if( CommandDispatch::GetInstance()->HandleCommand(LOWORD(wParam)) ) \
			return TRUE; \
		else \
			bHandled = FALSE; \
	}

#define LOCAL_MENUCOMMAND(id) \
	if(uMsg == WM_COMMAND) \
	{ \
		bHandled = TRUE; \
		if( CommandDispatch::GetInstance()->LocalHandleCommand(LOWORD(wParam), id, this) ) \
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