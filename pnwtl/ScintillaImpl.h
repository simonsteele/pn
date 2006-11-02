/**
 * @file ScintillaImpl.h
 * @brief Define further functionality for a scintilla wrapper.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
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
	typedef enum {fnNotFound, fnFound, fnReachedStart} FindNextResults;

	CScintillaImpl();

	int FindNext(SFindOptions* pOptions);
	bool ReplaceOnce(SReplaceOptions* pOptions);
	int ReplaceAll(SReplaceOptions* pOptions);
	//void HighlightAll(SFindOptions* pOptions); - doesn't work with all schemes...

	void ToggleFold();
	void FoldAll();
	void UnFoldAll();

	void PrintDocument(SPrintOptions* pOptions, bool showDialog = true);

	virtual int HandleNotify(LPARAM lParam);

	int GetWordCount();

	void IndentLine(int line, int indent);
	
	bool FindMatchingBraces(int& CaretBrace, int& OtherBrace);

	bool UnCommentLine(const CommentSpecRec& comments);
	bool UnCommentStream(const CommentSpecRec& comments);

protected:
	struct tagLastFindDetails
	{
		tstring findPhrase;
		long	startPos;
		long    lastPos;
		int		flags;
		int     result;
		bool	direction;
	} lastFindDetails;

	void DumbIndent(char ch);

	void ManageBraceMatch();

	virtual LPCTSTR GetDocTitle(){return _T("");}

	
	//*****************************************************************************
	//* Code added by Manuel Sandoval webmailusr-msn@yahoo.com
	//* Support for autocomplete
	//*****************************************************************************

public:
	typedef CSimpleArray<CString> CStringArray;
	void AddToAutoComplete(CString FullTag, CString TagName);  //Called in: CJumpTreeCtrl::OnFound: Add new defined autocomplete tags
	void ResetAutoComplete();           //Called in: CChildFrame::SaveFile: Clear new defined autocomplete tags
	void InitAutoComplete(Scheme *sch); //Called in: CTextView::SetScheme: Initialize default autocomplete tags

protected:	
	bool StartAutoComplete();
	int HandleNotify2(LPARAM lParam);
	int GetCaretInLine();
	CharacterRange GetSelection();
	void InsertChar(long nPos, char nChar);
	CString GetLineText(int nLine=-1);
	void SmartTag();
	CString GetSelText2();
	bool StartCallTip();
	void SetLineNumberWidth();
	void RangeExtendAndGrab(char *sel, int len, int &selStart, int &selEnd, int lengthDoc, bool (*ischarforsel)(char ch), bool stripEol = true);
	void ContinueCallTip();
	void FillFunctionDefinition(int pos = -1);
	const char *GetNearestWord(CStringArray arr, const char *wordStart, int searchLen, bool ignoreCase = false, CString wordCharacters = "/0", int wordIndex = -1);
	CString GetNearestWords(CStringArray  arr,const char *wordStart,int searchLen,bool ignoreCase= false,char otherSeparator='\0',bool exactLen=false);

	CStringArray  m_Api,m_KW;
	CString m_functionDefinition;
	CString m_currentCallTipWord;
	CString m_autoCompleteStartCharacters;
	CString m_calltipWordCharacters;
	CString m_strWordCharacters;
	CString m_calltipParametersEnd;
	CString m_calltipParametersStart;	
	CString m_calltipParametersSeparators;
	CString m_calltipEndDefinition;
	bool m_bAutoCompletion;
	bool m_bAutoCompleteIgnoreCase;	
	bool m_bSmartInsert;
	bool m_bSmartTag;
	bool m_bCallTipIgnoreCase;
	int m_lastPosCallTip;
	int m_nStartCalltipWord;
	int m_iAutoIndent;
	int m_nCurrentCallTip;
	int m_nMaxCallTips;
	int m_nBraceCount;
	Scheme *pScheme;
	SchemeDetails *pSchemeDetails;
	
	//*****************************************************************************
};

#endif // scintillaimpl_h__included