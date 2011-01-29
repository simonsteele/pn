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
class AutoCompleteManager;
typedef boost::shared_ptr<BaseAutoCompleteHandler> AutoCompleteHandlerPtr;
typedef boost::function<void (int start, int end)> MatchHandlerFn;

class TextRangeEx : public Scintilla::TextRange
{
public:
	TextRangeEx(int start, int end)
	{
		chrg.cpMin = start;
		chrg.cpMax = end;
	}

	int Length() const
	{
		return chrg.cpMax - chrg.cpMin;
	}
};

/**
 * @class CScintillaImpl
 * @brief Implement useful Scintilla functionality...
 */
class CScintillaImpl : public CScintilla
{
public:
	explicit CScintillaImpl();
	virtual ~CScintillaImpl();

	virtual int FindNext(extensions::ISearchOptions* pOptions);
	virtual bool ReplaceOnce(extensions::ISearchOptions* pOptions);
	virtual int ReplaceAll(extensions::ISearchOptions* pOptions);
	int FindAll(extensions::ISearchOptions* pOptions, MatchHandlerFn matchHandler);
	int FindAll(int start, int end, extensions::ISearchOptions* pOptions, MatchHandlerFn matchHandler);
	//void HighlightAll(SFindOptions* pOptions); - doesn't work with all schemes...

	void ToggleFold();
	void FoldAll();
	void UnFoldAll();

	void PrintDocument(SPrintOptions* pOptions, bool showDialog = true);

	virtual int HandleNotify(LPARAM lParam);

	int GetWordCount();

	void IndentLine(int line, int indent);
	
	bool FindMatchingBraces(int& CaretBrace, int& OtherBrace);

	bool UnCommentLine(const CommentSpecRec& comments, int line);
	bool UnCommentLine(const CommentSpecRec& comments);
	bool UnCommentStream(const CommentSpecRec& comments);
	
	//*****************************************************************************
	//* Code added by Manuel Sandoval webmailusr-msn@yahoo.com
	//* Support for autocomplete
	//*****************************************************************************

	void SetAutoCompleteManager(AutoCompleteManager* autoComplete);
	void AddToAutoComplete(CString FullTag, CString TagName);  //Called in: CJumpTreeCtrl::OnFound: Add new defined autocomplete tags
	void ResetAutoComplete();           //Called in: CChildFrame::SaveFile: Clear new defined autocomplete tags
	void InitAutoComplete(Scheme *sch); //Called in: CTextView::SetScheme: Initialize default autocomplete tags
	void ClearAutoComplete();

	virtual void SetKeyWords(int keywordSet, const char* keyWords);

	void AttemptAutoComplete();

	void SetAutoCompleteHandler(AutoCompleteHandlerPtr& handler);

	int GetCaretInLine();
	Scintilla::CharacterRange GetSelection();
	void InsertChar(long nPos, char nChar);
	std::string GetLineText(int nLine=-1);

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

	virtual tstring GetDocTitle() { return tstring(_T("")); }

private:
	bool StartAutoComplete(bool bForcefully);
	void AutoCloseBraces(Scintilla::SCNotification* scn);
	
	void SmartTag();
	bool StartCallTip();
	void SetLineNumberWidth();
	void RangeExtendAndGrab(char *sel, int len, int &selStart, int &selEnd, int lengthDoc, bool (*ischarforsel)(char ch), bool stripEol = true);
	void ContinueCallTip();
	void FillFunctionDefinition(int pos = -1);

	string_array  m_Api/*, m_KW*/;
	std::string m_functionDefinition;
	std::string m_currentCallTipWord;
	std::string m_autoCompleteStartCharacters;
	std::string m_calltipWordCharacters;
	std::string m_strWordCharacters;
	std::string m_calltipParametersEnd;
	std::string m_calltipParametersStart;	
	std::string m_calltipParametersSeparators;
	std::string m_calltipEndDefinition;
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

	boost::shared_ptr<IWordProvider> m_autoComplete;
	AutoCompleteHandlerPtr m_autoCompleteHandler;
	AutoCompleteManager* m_autoCompleteManager;
};

#endif // scintillaimpl_h__included