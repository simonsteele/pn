/**
 * @file searchoptions.cpp
 * @brief Implement SearchOptions
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef searchoptions_h__included
#define searchoptions_h__included

/**
 * Safe external version of SearchOptions
 */
class SearchOptions : public extensions::ISearchOptions
{
public:
	explicit SearchOptions() :
		m_found(false),
		m_noCursorMove(false),
		m_inSelection(false),
		m_matchWholeWord(false),
		m_matchCase(false),
		m_useRegExp(false),
		m_direction(true),
		m_loop(true),
		m_useSlashes(false),
		m_recurse(false),
		m_includeHidden(false)
	{
	}

	explicit SearchOptions(const extensions::ISearchOptions& other) :
		m_found(other.GetFound()),
		m_noCursorMove(other.GetNoCursorMove()),
		m_inSelection(other.GetReplaceInSelection()),
		m_matchWholeWord(other.GetMatchWholeWord()),
		m_matchCase(other.GetMatchCase()),
		m_useRegExp(other.GetUseRegExp()),
		m_direction(!other.GetSearchBackwards()),
		m_loop(other.GetLoopOK()),
		m_useSlashes(other.GetUseSlashes()),
		m_recurse(other.GetRecurse()),
		m_includeHidden(other.GetIncludeHidden()),
		m_findText(other.GetFindText()),
		m_replaceText(other.GetReplaceText()),
		m_fileExts(other.GetFileExts()),
		m_path(other.GetSearchPath())
	{
	}

	// Basic Options:
	const wchar_t* GetFindText() const { return m_findText.c_str(); }
	void SetFindText(const wchar_t* findText) { m_findText = findText; }
	
	bool GetMatchWholeWord() const { return m_matchWholeWord; }
	void SetMatchWholeWord(bool matchWholeWord) { m_matchWholeWord = matchWholeWord; }
	
	bool GetMatchCase() const { return m_matchCase; }
	void SetMatchCase(bool matchCase) { m_matchCase = matchCase; }
	
	bool GetUseRegExp() const { return m_useRegExp; }
	void SetUseRegExp(bool useRegExp) { m_useRegExp = useRegExp; }
	
	extensions::EFindWhere GetFindTarget() const { return m_findWhere; }
	void SetFindTarget(extensions::EFindWhere target) { m_findWhere = target; }
	
	bool GetSearchBackwards() const { return !m_direction; }
	void SetSearchBackwards(bool backwards) { m_direction = !backwards; }
	
	bool GetLoopOK() const { return m_loop; }
	void SetLoopOK(bool loop) { m_loop = loop; }
	
	bool GetUseSlashes() const { return m_useSlashes; }
	void SetUseSlashes(bool slashes) { m_useSlashes = slashes; }

	bool GetNoCursorMove() const { return m_noCursorMove; }
	void SetNoCursorMove(bool reposition) { m_noCursorMove = reposition; }

	// Replace Options:
	const wchar_t* GetReplaceText() const { return m_replaceText.c_str(); }
	void SetReplaceText(const wchar_t* text) { m_replaceText = text; }
	
	bool GetReplaceInSelection() const { return m_inSelection; }
	void SetReplaceInSelection(bool inSelection) { m_inSelection = inSelection; }

	// Find In Files Options:
	const wchar_t* GetFileExts() const { return m_fileExts.c_str(); }
	void SetFileExts(const wchar_t* extensions) { m_fileExts = extensions; }
	
	const wchar_t* GetSearchPath() const { return m_path.c_str(); }
	void SetSearchPath(const wchar_t* path) { m_path = path; }
	
	bool GetRecurse() const { return m_recurse; }
	void SetRecurse(bool recurse) { m_recurse = recurse; }

	bool GetIncludeHidden() const { return m_includeHidden; }
	void SetIncludeHidden(bool hidden) { m_includeHidden = hidden; }

	extensions::EFIFFileSet GetFileSet() const { return m_fileSet; }
	void SetFileSet(extensions::EFIFFileSet fileSet) { m_fileSet = fileSet; }
	
	// Result:
	virtual bool GetFound() const { return m_found; }
	virtual void SetFound(bool found) { m_found = found; }

private:
	std::wstring m_findText;
	bool m_matchWholeWord;
	bool m_matchCase;
	bool m_useRegExp;
	bool m_direction;		// true is down.
	bool m_loop;
	bool m_found;
	bool m_useSlashes;
	bool m_noCursorMove;
	extensions::EFindWhere m_findWhere;

	// Replace
	std::wstring m_replaceText;
	bool	m_inSelection;

	// Find In Files
	std::wstring m_fileExts;
	std::wstring m_path;
	bool m_recurse;
	bool m_includeHidden;
	extensions::EFIFFileSet m_fileSet;
};

#endif  // #ifndef searchoptions_h__included