#ifndef platform_h__included
#define platform_h__included

// special platform.h for lexer dll building...

#define PLAT_WIN 1

#ifdef PLAT_WIN



#include <windows.h>
#include <stdio.h>

#define WindowID HWND

#endif // PLAT_WIN

/**
 * Platform class used to retrieve system wide parameters such as double click speed
 * and chrome colour. Not a creatable object, more of a module with several functions.
 */
class Platform {
	// Private so Platform objects can not be copied
	Platform(const Platform &) {}
	Platform &operator=(const Platform &) { return *this; }
public:
	// Should be private because no new Platforms are ever created
	// but gcc warns about this
	Platform() {}
	~Platform() {}
	//static ColourDesired Chrome();
	//static ColourDesired ChromeHighlight();
	//static const char *DefaultFont();
	//static int DefaultFontSize();
	//static unsigned int DoubleClickTime();
	//static void DebugDisplay(const char *s);
	//static bool IsKeyDown(int key);
	static long SendScintilla(
		WindowID w, unsigned int msg, unsigned long wParam=0, long lParam=0);
	static bool IsDBCSLeadByte(int codePage, char ch);

	// These are utility functions not really tied to a platform
	//static int Minimum(int a, int b);
	//static int Maximum(int a, int b);
	// Next three assume 16 bit shorts and 32 bit longs
	//static long LongFromTwoShorts(short a,short b) {
	//	return (a) | ((b) << 16);
	//}
	//static short HighShortFromLong(long x) {
	//	return static_cast<short>(x >> 16);
	//}
	//static short LowShortFromLong(long x) {
	//	return static_cast<short>(x & 0xffff);
	//}
	static void DebugPrintf(const char *format, ...);
	//static bool ShowAssertionPopUps(bool assertionPopUps_);
	//static void Assert(const char *c, const char *file, int line);
	//static int Clamp(int val, int minVal, int maxVal);
};

#endif