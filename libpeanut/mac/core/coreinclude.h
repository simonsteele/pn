#ifndef PN_MAC_CORE_COREINCLUDES_
#define PN_MAC_CORE_COREINCLUDES_

#include <algorithm>

// TCHAR -------------------------------------------------------------------------------------------
typedef char TCHAR;
typedef const char* LPCTSTR;
typedef const char* LPCSTR;
typedef char* LPTSTR;

#define _tcscpy strcpy
#define _tcsstr strstr
#define _tcslen strlen
#define _tcscat strcat
#define _tcschr strchr
#define _tcscmp strcmp
#define _tcsicmp strcasecmp
#define _tcsnicmp strncasecmp
#define _ttoi atoi
#define _tcstoui64 strtoll
#define _istxdigit isxdigit
#define _istalpha isalpha
#define _T(s) s

#define _stricmp strcasecmp
#define _strnicmp strncasecmp
#define stricmp strcasecmp

// TCHAR File I/O
#define _fputts fputs
#define _tfopen fopen
#define _TEOF EOF

#define _snprintf snprintf
#define _sntprintf snprintf

// Unicode conversions:
class CharPassThru
{
public:
    CharPassThru(const char* s) : m_s(s) {}
    
    operator const char* () const { return m_s; };
private:
    const char* m_s;
};

#define CT2CA CharPassThru
#define CA2CT CharPassThru

// Global Bits -------------------------------------------------------------------------------------
#include <assert.h>

#define UNEXPECTED(x) pn__Unexpected(x)
#define RETURN_UNEXPECTED(x, y) pn__Unexpected(x); return y;

static void pn__Unexpected(LPCTSTR message) {
    std::cout << message;
}

#define PNASSERT assert

// Windowsy typedefs -------------------------------------------------------------------------------
typedef unsigned long LRESULT;
typedef unsigned long WPARAM;
typedef unsigned long LPARAM;
typedef unsigned long DWORD;
typedef unsigned int UINT;
typedef unsigned short WORD;
typedef unsigned char BYTE;
typedef void* LPVOID;
typedef unsigned int COLORREF;
typedef void* LPSECURITY_ATTRIBUTES;
typedef const wchar_t* LPCWSTR;

// TODO: Window handle type
typedef unsigned long HWND;
typedef unsigned long HINSTANCE;
typedef unsigned long HMODULE;

typedef unsigned long long	uint64_t;
typedef long long			int64_t;

// Response from CFBundleGetFunctionPointerForName is void*
typedef void* FARPROC;

#define FALSE 0
#define MAX_PATH 256

#define _strdup strdup

// Conveniences for Ports --------------------------------------------------------------------------

// TODO: Convert these to platformy services:
#define CLR_NONE ~0
#define COLOR_WINDOW 0
#define COLOR_WINDOWTEXT 1
#define COLOR_HIGHLIGHT 2
#define COLOR_HIGHLIGHTTEXT 3

#include <mach/mach.h>
#include <mach/mach_time.h>
#include <Carbon/Carbon.h>


// Some keys taken from extended keyboard layout here:
// http://boredzo.org/blog/wp-content/uploads/2007/05/IMTx-virtual-keycodes.pdf
// http://boredzo.org/blog/archives/2007-05-22/virtual-key-codes
// Some are probably wrong too...

#define VK_RETURN kVK_Return
#define VK_BACK kVK_Delete
#define VK_DELETE kVK_ForwardDelete
#define VK_INSERT kVK_Help
#define VK_OEM_2 kVK_ANSI_Slash
#define VK_OEM_3 kVK_ANSI_Grave
#define VK_OEM_5 kVK_ANSI_Backslash
#define VK_OEM_4 kVK_ANSI_LeftBracket
#define VK_OEM_6 kVK_ANSI_RightBracket
#define VK_OEM_PERIOD kVK_ANSI_Period
#define VK_OEM_COMMA kVK_ANSI_Comma
#define VK_ESCAPE kVK_Escape
#define VK_SUBTRACT kVK_ANSI_KeypadMinus
#define VK_ADD kVK_ANSI_KeypadPlus
#define VK_DIVIDE kVK_ANSI_KeypadDivide
#define VK_MULTIPLY kVK_ANSI_KeypadMultiply
#define VK_DOWN kVK_DownArrow
#define VK_UP kVK_UpArrow
#define VK_LEFT kVK_LeftArrow
#define VK_RIGHT kVK_RightArrow
#define VK_HOME kVK_Home
#define VK_END kVK_End
#define VK_PRIOR kVK_PageUp
#define VK_NEXT kVK_PageDown
#define VK_TAB kVK_Tab
#define VK_F1 kVK_F1
#define VK_F2 kVK_F2
#define VK_F3 kVK_F3
#define VK_F4 kVK_F4
#define VK_F5 kVK_F5
#define VK_F6 kVK_F6
#define VK_F7 kVK_F7
#define VK_F8 kVK_F8
#define VK_F9 kVK_F9
#define VK_F10 kVK_F10
#define VK_F11 kVK_F11
#define VK_F12 kVK_F12


// Predefined command IDs:
#define ID_FILE_NEW 40001
#define ID_FILE_OPEN 40002
#define ID_FILE_SAVE 40003
#define ID_FILE_CLOSE 40004
#define ID_FILE_PRINT 40005
#define ID_EDIT_CUT   40006
#define ID_EDIT_COPY  40007
#define ID_EDIT_PASTE 40008
#define ID_EDIT_UNDO  40009
#define ID_EDIT_REDO  40010
#define ID_EDIT_FIND  40011
#define ID_EDIT_REPLACE 40012
#define ID_NEXT_PANE 40013
#define ID_PREV_PANE 40014


class TimeBaseInfo
{
public:
    static mach_timebase_info_data_t& info()
    {
        // If this is the first time we've run, get the timebase.
        // We can use denom == 0 to indicate that sTimebaseInfo is 
        // uninitialised because it makes no sense to have a zero 
        // denominator is a fraction.
        
        if (sTimebaseInfo.denom == 0)
        {
            mach_timebase_info(&sTimebaseInfo);
        }
        
        return sTimebaseInfo;
    }
private:
    static mach_timebase_info_data_t sTimebaseInfo;
};

// http://developer.apple.com/library/mac/#qa/qa1398/_index.html
static unsigned long GetTickCount()
{
    mach_timebase_info_data_t& timeBaseInfo(TimeBaseInfo::info());
    // TODO: Verify this works, may need to move the millis divide to after the conversion to nano.
    uint64_t abstime = mach_absolute_time();
    uint64_t millis = abstime / 1000000;
    millis = millis * timeBaseInfo.numer / timeBaseInfo.denom;
    return (unsigned long)millis;
}

#define RGB(r, g, b) ((r & 0xff) | ((g & 0xff) << 8) | ((b & 0xff) << 16))
#define GetRValue(rgb)   ((BYTE) (rgb))
#define GetGValue(rgb)   ((BYTE) (((WORD) (rgb)) >> 8))
#define GetBValue(rgb)   ((BYTE) ((rgb) >> 16)) 

static COLORREF GetSysColor(int color)
{
    switch (color)
    {
        case COLOR_WINDOW:
            return RGB(255, 255, 255);
        case COLOR_WINDOWTEXT:
            return 0;
        case COLOR_HIGHLIGHT:
            return RGB(0xAA, 0xAA, 0xEE);
        case COLOR_HIGHLIGHTTEXT:
            return 0;
    }
    
    return 0;
}

#define LOG(x) printf(x)

#endif // PN_MAC_CORE_COREINCLUDES_
