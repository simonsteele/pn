// Windows specific Platform implementations...
#include "platform.h"

#ifdef PLAT_WIN



long Platform::SendScintilla(WindowID w, unsigned int msg, unsigned long wParam, long lParam) {
	return ::SendMessage(reinterpret_cast<HWND>(w), msg, wParam, lParam);
}

bool Platform::IsDBCSLeadByte(int codePage, char ch) {
	return ::IsDBCSLeadByteEx(codePage, ch) != 0;
}

void Platform::DebugPrintf(const char *, ...) {
}

#endif // PLAT_WIN