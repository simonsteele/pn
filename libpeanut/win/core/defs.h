#ifndef PN_WIN_CORE_DEFS
#define PN_WIN_CORE_DEFS

#define PN_INITIALISEFRAME	(WM_APP+1)
#define PN_NOTIFY			(WM_APP+2)
#define PN_CHECKAGE			(WM_APP+3)
#define PN_OPTIONSUPDATED	(WM_APP+4)
#define PN_TOGGLEOUTPUT		(WM_APP+5)
#define PN_TOOLRUNUPDATE	(WM_APP+6)
#define PN_SCHEMECHANGED	(WM_APP+7)
#define PN_HANDLEHSCLICK	(WM_APP+8)
#define PN_ESCAPEPRESSED	(WM_APP+9)
#define PN_UPDATEFINDTEXT	(WM_APP+10)
#define PN_PROJECTNOTIFY	(WM_APP+11)
#define PN_FIFMATCH			(WM_APP+12)
#define PN_GOTOLINE			(WM_APP+13)
#define PN_GETMDICLIENTRECT (WM_APP+14)
#define PN_MDISETMENU		(WM_APP+15)
#define PN_REFRESHUPDATEUI  (WM_APP+16)
#define PN_UPDATECHILDUI	(WM_APP+17)
#define PN_CLOSEALLOTHER	(WM_APP+18)
#define PN_OVERWRITETARGET	(WM_APP+19)
#define PN_INSERTCLIP		(WM_APP+20)
#define PN_SETFOCUS			(WM_APP+21)
#define	PN_COMPLETECLIP		(WM_APP+22)
#define PN_INSERTCLIPTEXT   (WM_APP+23)
#define PN_SETSCHEME		(WM_APP+24)

// Command IDs used around the place...

#ifdef _UNICODE
	#define WIDEN2(x) L ## x
	#define WIDEN(x) WIDEN2(x)
	#define __WFILE__ WIDEN(__FILE__)
	#define __TFILE__ __WFILE__
#else
	#define __TFILE__ __FILE__
#endif


//#if defined(DEBUG_)
	#define UNEXPECTED(message) \
	{ \
		pn__Unexpected(__TFILE__, __LINE__, message); \
	}

	#define RETURN_UNEXPECTED(message, ret) \
	{ \
		pn__Unexpected(__TFILE__, __LINE__, message); \
		return ret; \
	}
/*#else
	#define UNEXPECTED(message) ;
	#define RETURN_UNEXPECTED(message, ret) return ret;
#endif*/

#define LOG(message) \
	::OutputDebugString(message)


#endif PN_WIN_CORE_DEFS
