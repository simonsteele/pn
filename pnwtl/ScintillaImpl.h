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
};

#endif // scintillaimpl_h__included