/**
 * @file ScintillaImpl.h
 * @brief Define further functionality for a scintilla wrapper.
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef scintillaimpl_h__included
#define scintillaimpl_h__included

#include "scintillaif.h"
#include "scintillaiterator.h"

namespace { class ISearchOptions; }
class IWordProvider;
class BaseAutoCompleteHandler;
typedef boost::shared_ptr<BaseAutoCompleteHandler> AutoCompleteHandlerPtr;

/**
 * @class CScintillaImpl
 * @brief Implement useful Scintilla functionality...
 */
class CScintillaImpl : public CScintilla
{
public:
	CScintillaImpl();
	~CScintillaImpl();

	int FindNext(extensions::ISearchOptions* pOptions);
	bool ReplaceOnce(extensions::ISearchOptions* pOptions);
	int ReplaceAll(extensions::ISearchOptions* pOptions);
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
	
	//*****************************************************************************
	//* Code added by Manuel Sandoval webmailusr-msn@yahoo.com
	//* Support for autocomplete
	//*****************************************************************************

	void AddToAutoComplete(CString FullTag, CString TagName);  //Called in: CJumpTreeCtrl::OnFound: Add new defined autocomplete tags
	void ResetAutoComplete();           //Called in: CChildFrame::SaveFile: Clear new defined autocomplete tags
	void InitAutoComplete(Scheme *sch); //Called in: CTextView::SetScheme: Initialize default autocomplete tags
	void ClearAutoComplete();

	virtual void SetKeyWords(int keywordSet, const char* keyWords);

	void AttemptAutoComplete();

	void SetAutoCompleteHandler(AutoCompleteHandlerPtr& handler);

	tstring GetSelText2();
	int GetCaretInLine();
	CharacterRange GetSelection();
	void InsertChar(long nPos, char nChar);
	tstring GetLineText(int nLine=-1);

public:
	// iterable:
	typedef ScintillaIterator iterator;
	typedef ScintillaIterator const_iterator;
	ScintillaIterator begin();
	ScintillaIterator end();

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

private:
	bool StartAutoComplete();
	void AutoCloseBraces(SCNotification* scn);
	
	void SmartTag();
	bool StartCallTip();
	void SetLineNumberWidth();
	void RangeExtendAndGrab(char *sel, int len, int &selStart, int &selEnd, int lengthDoc, bool (*ischarforsel)(char ch), bool stripEol = true);
	void ContinueCallTip();
	void FillFunctionDefinition(int pos = -1);

	tstring_array  m_Api/*, m_KW*/;
	tstring m_functionDefinition;
	tstring m_currentCallTipWord;
	tstring m_autoCompleteStartCharacters;
	tstring m_calltipWordCharacters;
	tstring m_strWordCharacters;
	tstring m_calltipParametersEnd;
	tstring m_calltipParametersStart;	
	tstring m_calltipParametersSeparators;
	tstring m_calltipEndDefinition;
	bool m_bAutoCompletion;
	bool m_bAutoCompleteIgnoreCase;	
	bool m_bAutoCompletionUseTags;
	bool m_bSmartTag;
	bool m_bCallTipIgnoreCase;
	bool m_bAutoActivate;
	int m_lastPosCallTip;
	int m_nStartCalltipWord;
	int m_iAutoIndent;
	int m_nCurrentCallTip;
	int m_nMaxCallTips;
	int m_nBraceCount;
	int m_nMinAutoCompleteChars;
	Scheme *m_pScheme;

	IWordProvider* m_autoComplete;
	AutoCompleteHandlerPtr m_autoCompleteHandler;
};

#endif // scintillaimpl_h__included