/**
 * @file ScintillaImpl.h
 * @brief Define further functionality for a scintilla wrapper.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef scintillaimpl_h__included
#define scintillaimpl_h__included

#include "scintillaif.h"

/**
 * @class CScintillaImpl
 * @brief Implement useful Scintilla functionality...
 */
class CScintillaImpl : public CScintilla
{
public:
	CScintillaImpl();

	int FindNext(SFindOptions* pOptions);
	bool ReplaceOnce(SReplaceOptions* pOptions);
	int ReplaceAll(SReplaceOptions* pOptions);
	void HighlightAll(SFindOptions* pOptions);

	void ToggleFold();
	void FoldAll();
	void UnFoldAll();

	void PrintDocument(SPrintOptions* pOptions, bool showDialog = true);

	virtual int HandleNotify(LPARAM lParam);

	typedef enum {fnNotFound, fnFound, fnReachedStart} FindNextResults;

protected:
	struct tagLastFindDetails
	{
		tstring findPhrase;
		long	startPos;
		long    lastPos;
		int		flags;
		int     result;
	} lastFindDetails;

	void IndentLine(int line, int indent);
	int GetIndentLevel(int line);
	
	void DumbIndent(char ch);

	bool FindMatchingBraces(int& CaretBrace, int& OtherBrace);
	void ManageBraceMatch();

	virtual LPCTSTR GetDocTitle(){return _T("");}
};

#endif // scintillaimpl_h__included