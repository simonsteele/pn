/**
 * @file outputscintilla.h
 * @brief Simple RegEx based output lexer wrapped in a scintilla.
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef rescintilla_h__included_B21F3B09_1E2B_465f_8E09_95527833AC9A
#define rescintilla_h__included_B21F3B09_1E2B_465f_8E09_95527833AC9A

#include "scintillaimpl.h"

class ScintillaAccessor;

namespace PCRE {
	class RegExp;
}

#define SCE_CUSTOM_ERROR	20

/**
 * @brief simple RE parsing scintilla for output window.
 */
class REScintilla : public CScintillaImpl
{
	typedef CScintillaImpl baseClass;
public:
	REScintilla();
	~REScintilla();

	void SetRE(LPCTSTR regex, bool bClearStyling = true);
	void ExtendStyleRange(int startPos, int style, TextRange* tr);

	PCRE::RegExp* GetRE() const;

	virtual int HandleNotify(LPARAM lParam);

protected:
	void handleStyleNeeded(ScintillaAccessor& styler, int startPos, int length);
	void customColouriseLine(ScintillaAccessor& styler, char *lineBuffer, int length, int endLine);

protected:
	tstring			m_customre;
	PCRE::RegExp*	m_pRE;
	bool			schemeLoaded;
};

/**
 * @brief Build regular expressions for tool output matching.
 */
class CToolREBuilder : public CustomFormatStringBuilder<CToolREBuilder>
{
	public:
		void OnFormatChar(TCHAR thechar);
};

#endif //#ifndef rescintilla_h__included_B21F3B09_1E2B_465f_8E09_95527833AC9A