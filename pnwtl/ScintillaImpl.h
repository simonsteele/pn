#ifndef scintillaimpl_h__included
#define scintillaimpl_h__included

/**
 * @class CScintillaImpl
 * @brief Implement useful Scintilla functionality...
 */
class CScintillaImpl : public CScintilla
{
public:
	bool FindNext(SFindOptions* pOptions);
	bool ReplaceOnce(SReplaceOptions* pOptions);
	void ReplaceAll(SReplaceOptions* pOptions);
	void HighlightAll(SFindOptions* pOptions);

	virtual int HandleNotify(LPARAM lParam);

protected:
	void IndentLine(int line, int indent);
	int GetIndentLevel(int line);
	
	void DumbIndent(char ch);

	bool FindMatchingBraces(int& CaretBrace, int& OtherBrace);
	void ManageBraceMatch();
};

#endif // scintillaimpl_h__included