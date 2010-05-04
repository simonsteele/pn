/**
 * @file ScintillaIF.h
 * @brief Define scintilla wrapper class CScintilla
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef __SCINTILLAIF_H__
#define __SCINTILLAIF_H__

//#define STATIC_SCILEXER
#define WTL_SCINTILLA 1
//#define PLAT_WIN 1

#define SC_BOOKMARK 0
#define SC_NUMBERED_BOOKMARK 1

#include "third_party/scintilla/include/scintilla.h"

//! Block size for disk reading and writing.
const int blockSize = 131072;

//! Scintilla direct message function.
typedef long(__cdecl* scmsgfn)(void *ptr, long Msg, WPARAM wParam, LPARAM lParam);

typedef enum {efsVSNet, efsVSNetR, efsPlus, efsArrow} EFoldStyle;

struct RangeToFormat 
{
	void* hdc;
	void* hdcTarget;
	RECT rc;
	RECT rcPage;
	Scintilla::CharacterRange chrg;
};

class CScintilla;

/**
 * UndoGroup begins an undo collection when declared and ends it when destroyed.
 */
class UndoGroup
{
public:
	UndoGroup(CScintilla&);
	~UndoGroup();

private:
	CScintilla& m_sci;
};

/**
 * CScintilla is a no-framework Scintilla wrapper for C++, it still requires
 * assistance from a window loop in the containing window. The code for
 * many methods is derived from / taken from code found in Scite.
 */
class CScintilla
{
	public:
		/// Default constructor
		CScintilla();

		/// Destructor
		virtual ~CScintilla();

#ifndef WTL_SCINTILLA
		/// Create a Scintilla window inside parent hParent.
		HWND Create(HWND hParent, HINSTANCE hInst);

		HWND GetHwnd()
		{
			return m_scihWnd;
		}
#endif

		/**
		 * Is the control modified - gained from HandleNotify function.
		 * A using class should call IsScintillaNotify on a Notify message
		 * (WM_NOTIFY) and should then call HandleNotify if the result is
		 * true.
		 */
		bool IsScintillaNotify(LPARAM lParam);

		/**
		 * HandleNotify currently only updates the Modified flag
		 * of CScintilla.
		 */
		virtual int HandleNotify(LPARAM lParam);
		
		//! Returns whether the text in the control has been modified since the last save.
		bool GetModified();
	
		/**
		 * SPerform uses either SendMessage or the function pointer (Perform)
		 * to run scintilla commands on the relevant scintilla control.
		 */
		virtual long SPerform(long Msg, WPARAM wParam=0, LPARAM lParam=0)
		{
			if (Perform)
				return Perform(m_Pointer, Msg, wParam, lParam);
			else
				return ::SendMessage(m_scihWnd, Msg, wParam, lParam);
		}

		/// Load a file from "filename".
		virtual bool OpenFile(LPCTSTR filename);

		/// Save the contents of the control to "filename".
		virtual bool SaveFile(LPCTSTR filename);


		void SetInitialTabWidth(int ts){m_TabWidth = ts;}

		int GetSelLength(){return m_SelLength;}

		// The following are helper functions - they allow
		// the code using certain parts of Scintilla to look
		// a little cleaner.
		void GetSel(Scintilla::CharacterRange& cr);
		void EnsureRangeVisible(int begin, int end, bool enforcePolicy = true);
		void SetTarget(int begin, int end);
		void SetTarget(Scintilla::CharacterRange* cr);
		
		void GotoLineEnsureVisible(int line);

		void DefineMarker(int marker, int markerType, COLORREF fore, COLORREF back);

		void DefineNumberedBookmarks(int base = SC_NUMBERED_BOOKMARK, bool SetDefaultColours = true);
		void ToggleNumberedBookmark(int number, int base = SC_NUMBERED_BOOKMARK);
		void JumpToNumberedBookmark(int number, int base = SC_NUMBERED_BOOKMARK);

		void DefineBookmarks();
		void ToggleBookmark(int marker = SC_BOOKMARK);
		void NextBookmark();
		void PrevBookmark();
		void ClearAllBookmarks();
        
		// Folding (mostly Scite implementation)
		/// Set the Folding Margins
		void SetFoldingMargins(EFoldStyle style);
		/// Called when Fold Changed
		void FoldChanged(int line, int levelNow, int levelPrev);
		/// Called to Expand and Collapse folded sections
		void Expand(int &line, bool doExpand, bool force=false, int visLevels=0, int level=-1);
		/// Called when a margin is clicked on.
		bool MarginClick(int position, int modifiers);
		/// Call FoldAll() to collapse the entire document.
		void FoldAll();

		void DisableDirectAccess();
		bool EnableDirectAccess();

	protected:
	
		// Locally Written CScintilla members.
	
		//! Handle of the loaded scilexer.dll
		static HMODULE scidll;
		//! Reference counter.
		static int refs;
		//! Handle of the relevant scintilla window - inherit one if we're a WTL window.
		HWND	m_scihWnd;
		//! Used for Scintilla's GetDirectPointer
		void	*m_Pointer;
		//! Function pointer to Scintilla window message pump.
		scmsgfn	Perform;
		scmsgfn StoredPerform;
		
		//! Is text modified?
		bool m_Modified;
		//! Locally used method to get a range of text.
		void GetRange(int start, int end, char *text);

		int	m_TabWidth;

		int m_SelLength;

		int m_numberedBookmarks[10];
	
	// Python Wrapper-Generator Generated header...
	public:
		/**
		 * @name Public Wrapper Functions
		 * The functions below are defined and implemented automagically
		 * using the Python wrapper generator code contained in SHFace.py and
		 * SHFacer.py.
		 */
		//@{
		
		//++FuncDef
		/**
		 * Add text to the document.
		 */
		void AddText(int length, const char* text);
		/**
		 * Add array of cells to document.
		 */
		void AddStyledText(int length, char* c);
		/**
		 * Insert string at a position.
		 */
		void InsertText(long pos, const char* text);
		/**
		 * Delete all text in the document.
		 */
		void ClearAll();
		/**
		 * Set all style bytes to 0, remove all folding information.
		 */
		void ClearDocumentStyle();
		/**
		 * The number of characters in the document.
		 */
		int GetLength();
		/**
		 * Returns the character byte at the position.
		 */
		int GetCharAt(long pos);
		/**
		 * Returns the position of the caret.
		 */
		long GetCurrentPos();
		/**
		 * Returns the position of the opposite end of the selection to the caret.
		 */
		long GetAnchor();
		/**
		 * Returns the style byte at the position.
		 */
		int GetStyleAt(long pos);
		/**
		 * Redoes the next action on the undo history.
		 */
		void Redo();
		/**
		 * Choose between collecting actions into the undo
		 * history and discarding them.
		 */
		void SetUndoCollection(bool collectUndo);
		/**
		 * Select all the text in the document.
		 */
		void SelectAll();
		/**
		 * Remember the current position in the undo history as the position
		 * at which the document was saved.
		 */
		void SetSavePoint();
		/**
		 * Retrieve a buffer of cells.
		 * Returns the number of bytes in the buffer not including terminating NULs.
		 */
		int GetStyledText(Scintilla::TextRange* tr);
		/**
		 * Are there any redoable actions in the undo history?
		 */
		bool CanRedo();
		/**
		 * Retrieve the line number at which a particular marker is located.
		 */
		int MarkerLineFromHandle(int handle);
		/**
		 * Delete a marker.
		 */
		void MarkerDeleteHandle(int handle);
		/**
		 * Is undo history being collected?
		 */
		bool GetUndoCollection();
		/**
		 * Are white space characters currently visible?
		 * Returns one of SCWS_* constants.
		 */
		int GetViewWS();
		/**
		 * Make white space characters invisible, always visible or visible outside indentation.
		 */
		void SetViewWS(int viewWS);
		/**
		 * Find the position from a point within the window.
		 */
		long PositionFromPoint(int x, int y);
		/**
		 * Find the position from a point within the window but return
		 * INVALID_POSITION if not close to text.
		 */
		long PositionFromPointClose(int x, int y);
		/**
		 * Set caret to start of a line and ensure it is visible.
		 */
		void GotoLine(int line);
		/**
		 * Set caret to a position and ensure it is visible.
		 */
		void GotoPos(long pos);
		/**
		 * Set the selection anchor to a position. The anchor is the opposite
		 * end of the selection from the caret.
		 */
		void SetAnchor(long posAnchor);
		/**
		 * Retrieve the text of the line containing the caret.
		 * Returns the index of the caret on the line.
		 */
		int GetCurLine(int length, char* text);
		/**
		 * Retrieve the position of the last correctly styled character.
		 */
		long GetEndStyled();
		/**
		 * Convert all line endings in the document to one mode.
		 */
		void ConvertEOLs(int eolMode);
		/**
		 * Retrieve the current end of line mode - one of CRLF, CR, or LF.
		 */
		int GetEOLMode();
		/**
		 * Set the current end of line mode.
		 */
		void SetEOLMode(int eolMode);
		/**
		 * Set the current styling position to pos and the styling mask to mask.
		 * The styling mask can be used to protect some bits in each styling byte from modification.
		 */
		void StartStyling(long pos, int mask);
		/**
		 * Change style from current styling position for length characters to a style
		 * and move the current styling position to after this newly styled segment.
		 */
		void SetStyling(int length, int style);
		/**
		 * Is drawing done first into a buffer or direct to the screen?
		 */
		bool GetBufferedDraw();
		/**
		 * If drawing is buffered then each line of text is drawn into a bitmap buffer
		 * before drawing it to the screen to avoid flicker.
		 */
		void SetBufferedDraw(bool buffered);
		/**
		 * Change the visible size of a tab to be a multiple of the width of a space character.
		 */
		void SetTabWidth(int tabWidth);
		/**
		 * Retrieve the visible size of a tab.
		 */
		int GetTabWidth();
		/**
		 * Set the code page used to interpret the bytes of the document as characters.
		 * The SC_CP_UTF8 value can be used to enter Unicode mode.
		 */
		void SetCodePage(int codePage);
		/**
		 * In palette mode, Scintilla uses the environment's palette calls to display
		 * more colours. This may lead to ugly displays.
		 */
		void SetUsePalette(bool usePalette);
		/**
		 * Set the symbol used for a particular marker number.
		 */
		void MarkerDefine(int markerNumber, int markerSymbol);
		/**
		 * Set the foreground colour used for a particular marker number.
		 */
		void MarkerSetFore(int markerNumber, COLORREF fore);
		/**
		 * Set the background colour used for a particular marker number.
		 */
		void MarkerSetBack(int markerNumber, COLORREF back);
		/**
		 * Add a marker to a line, returning an ID which can be used to find or delete the marker.
		 */
		int MarkerAdd(int line, int markerNumber);
		/**
		 * Delete a marker from a line.
		 */
		void MarkerDelete(int line, int markerNumber);
		/**
		 * Delete all markers with a particular number from all lines.
		 */
		void MarkerDeleteAll(int markerNumber);
		/**
		 * Get a bit mask of all the markers set on a line.
		 */
		int MarkerGet(int line);
		/**
		 * Find the next line after lineStart that includes a marker in mask.
		 */
		int MarkerNext(int lineStart, int markerMask);
		/**
		 * Find the previous line before lineStart that includes a marker in mask.
		 */
		int MarkerPrevious(int lineStart, int markerMask);
		/**
		 * Define a marker from a pixmap.
		 */
		void MarkerDefinePixmap(int markerNumber, const char* pixmap);
		/**
		 * Set a margin to be either numeric or symbolic.
		 */
		void SetMarginTypeN(int margin, int marginType);
		/**
		 * Retrieve the type of a margin.
		 */
		int GetMarginTypeN(int margin);
		/**
		 * Set the width of a margin to a width expressed in pixels.
		 */
		void SetMarginWidthN(int margin, int pixelWidth);
		/**
		 * Retrieve the width of a margin in pixels.
		 */
		int GetMarginWidthN(int margin);
		/**
		 * Set a mask that determines which markers are displayed in a margin.
		 */
		void SetMarginMaskN(int margin, int mask);
		/**
		 * Retrieve the marker mask of a margin.
		 */
		int GetMarginMaskN(int margin);
		/**
		 * Make a margin sensitive or insensitive to mouse clicks.
		 */
		void SetMarginSensitiveN(int margin, bool sensitive);
		/**
		 * Retrieve the mouse click sensitivity of a margin.
		 */
		bool GetMarginSensitiveN(int margin);
		/**
		 * Clear all the styles and make equivalent to the global default style.
		 */
		void StyleClearAll();
		/**
		 * Set the foreground colour of a style.
		 */
		void StyleSetFore(int style, COLORREF fore);
		/**
		 * Set the background colour of a style.
		 */
		void StyleSetBack(int style, COLORREF back);
		/**
		 * Set a style to be bold or not.
		 */
		void StyleSetBold(int style, bool bold);
		/**
		 * Set a style to be italic or not.
		 */
		void StyleSetItalic(int style, bool italic);
		/**
		 * Set the size of characters of a style.
		 */
		void StyleSetSize(int style, int sizePoints);
		/**
		 * Set the font of a style.
		 */
		void StyleSetFont(int style, const char* fontName);
		/**
		 * Set a style to have its end of line filled or not.
		 */
		void StyleSetEOLFilled(int style, bool filled);
		/**
		 * Reset the default style to its state at startup
		 */
		void StyleResetDefault();
		/**
		 * Set a style to be underlined or not.
		 */
		void StyleSetUnderline(int style, bool underline);
		/**
		 * Set a style to be mixed case, or to force upper or lower case.
		 */
		void StyleSetCase(int style, int caseForce);
		/**
		 * Set the character set of the font in a style.
		 */
		void StyleSetCharacterSet(int style, int characterSet);
		/**
		 * Set a style to be a hotspot or not.
		 */
		void StyleSetHotSpot(int style, bool hotspot);
		/**
		 * Set the foreground colour of the selection and whether to use this setting.
		 */
		void SetSelFore(bool useSetting, COLORREF fore);
		/**
		 * Set the background colour of the selection and whether to use this setting.
		 */
		void SetSelBack(bool useSetting, COLORREF back);
		/**
		 * Set the foreground colour of the caret.
		 */
		void SetCaretFore(COLORREF fore);
		/**
		 * When key+modifier combination km is pressed perform msg.
		 */
		void AssignCmdKey(DWORD km, int msg);
		/**
		 * When key+modifier combination km is pressed do nothing.
		 */
		void ClearCmdKey(DWORD km);
		/**
		 * Drop all key mappings.
		 */
		void ClearAllCmdKeys();
		/**
		 * Set the styles for a segment of the document.
		 */
		void SetStylingEx(int length, const char* styles);
		/**
		 * Set a style to be visible or not.
		 */
		void StyleSetVisible(int style, bool visible);
		/**
		 * Get the time in milliseconds that the caret is on and off.
		 */
		int GetCaretPeriod();
		/**
		 * Get the time in milliseconds that the caret is on and off. 0 = steady on.
		 */
		void SetCaretPeriod(int periodMilliseconds);
		/**
		 * Set the set of characters making up words for when moving or selecting by word.
		 * First sets deaults like SetCharsDefault.
		 */
		void SetWordChars(const char* characters);
		/**
		 * Start a sequence of actions that is undone and redone as a unit.
		 * May be nested.
		 */
		void BeginUndoAction();
		/**
		 * End a sequence of actions that is undone and redone as a unit.
		 */
		void EndUndoAction();
		/**
		 * Set an indicator to plain, squiggle or TT.
		 */
		void IndicSetStyle(int indic, int style);
		/**
		 * Retrieve the style of an indicator.
		 */
		int IndicGetStyle(int indic);
		/**
		 * Set the foreground colour of an indicator.
		 */
		void IndicSetFore(int indic, COLORREF fore);
		/**
		 * Retrieve the foreground colour of an indicator.
		 */
		COLORREF IndicGetFore(int indic);
		/**
		 * Set the foreground colour of all whitespace and whether to use this setting.
		 */
		void SetWhitespaceFore(bool useSetting, COLORREF fore);
		/**
		 * Set the background colour of all whitespace and whether to use this setting.
		 */
		void SetWhitespaceBack(bool useSetting, COLORREF back);
		/**
		 * Divide each styling byte into lexical class bits (default: 5) and indicator
		 * bits (default: 3). If a lexer requires more than 32 lexical states, then this
		 * is used to expand the possible states.
		 */
		void SetStyleBits(int bits);
		/**
		 * Retrieve number of bits in style bytes used to hold the lexical state.
		 */
		int GetStyleBits();
		/**
		 * Used to hold extra styling information for each line.
		 */
		void SetLineState(int line, int state);
		/**
		 * Retrieve the extra styling information for a line.
		 */
		int GetLineState(int line);
		/**
		 * Retrieve the last line number that has line state.
		 */
		int GetMaxLineState();
		/**
		 * Is the background of the line containing the caret in a different colour?
		 */
		bool GetCaretLineVisible();
		/**
		 * Display the background of the line containing the caret in a different colour.
		 */
		void SetCaretLineVisible(bool show);
		/**
		 * Get the colour of the background of the line containing the caret.
		 */
		COLORREF GetCaretLineBack();
		/**
		 * Set the colour of the background of the line containing the caret.
		 */
		void SetCaretLineBack(COLORREF back);
		/**
		 * Set a style to be changeable or not (read only).
		 * Experimental feature, currently buggy.
		 */
		void StyleSetChangeable(int style, bool changeable);
		/**
		 * Display a auto-completion list.
		 * The lenEntered parameter indicates how many characters before
		 * the caret should be used to provide context.
		 */
		void AutoCShow(int lenEntered, const char* itemList);
		/**
		 * Remove the auto-completion list from the screen.
		 */
		void AutoCCancel();
		/**
		 * Is there an auto-completion list visible?
		 */
		bool AutoCActive();
		/**
		 * Retrieve the position of the caret when the auto-completion list was displayed.
		 */
		long AutoCPosStart();
		/**
		 * User has selected an item so remove the list and insert the selection.
		 */
		void AutoCComplete();
		/**
		 * Define a set of character that when typed cancel the auto-completion list.
		 */
		void AutoCStops(const char* characterSet);
		/**
		 * Change the separator character in the string setting up an auto-completion list.
		 * Default is space but can be changed if items contain space.
		 */
		void AutoCSetSeparator(int separatorCharacter);
		/**
		 * Retrieve the auto-completion list separator character.
		 */
		int AutoCGetSeparator();
		/**
		 * Select the item in the auto-completion list that starts with a string.
		 */
		void AutoCSelect(const char* text);
		/**
		 * Should the auto-completion list be cancelled if the user backspaces to a
		 * position before where the box was created.
		 */
		void AutoCSetCancelAtStart(bool cancel);
		/**
		 * Retrieve whether auto-completion cancelled by backspacing before start.
		 */
		bool AutoCGetCancelAtStart();
		/**
		 * Define a set of characters that when typed will cause the autocompletion to
		 * choose the selected item.
		 */
		void AutoCSetFillUps(const char* characterSet);
		/**
		 * Should a single item auto-completion list automatically choose the item.
		 */
		void AutoCSetChooseSingle(bool chooseSingle);
		/**
		 * Retrieve whether a single item auto-completion list automatically choose the item.
		 */
		bool AutoCGetChooseSingle();
		/**
		 * Set whether case is significant when performing auto-completion searches.
		 */
		void AutoCSetIgnoreCase(bool ignoreCase);
		/**
		 * Retrieve state of ignore case flag.
		 */
		bool AutoCGetIgnoreCase();
		/**
		 * Display a list of strings and send notification when user chooses one.
		 */
		void UserListShow(int listType, const char* itemList);
		/**
		 * Set whether or not autocompletion is hidden automatically when nothing matches.
		 */
		void AutoCSetAutoHide(bool autoHide);
		/**
		 * Retrieve whether or not autocompletion is hidden automatically when nothing matches.
		 */
		bool AutoCGetAutoHide();
		/**
		 * Set whether or not autocompletion deletes any word characters
		 * after the inserted text upon completion.
		 */
		void AutoCSetDropRestOfWord(bool dropRestOfWord);
		/**
		 * Retrieve whether or not autocompletion deletes any word characters
		 * after the inserted text upon completion.
		 */
		bool AutoCGetDropRestOfWord();
		/**
		 * Register an XPM image for use in autocompletion lists.
		 */
		void RegisterImage(int type, const char* xpmData);
		/**
		 * Clear all the registered XPM images.
		 */
		void ClearRegisteredImages();
		/**
		 * Retrieve the auto-completion list type-separator character.
		 */
		int AutoCGetTypeSeparator();
		/**
		 * Change the type-separator character in the string setting up an auto-completion list.
		 * Default is '?' but can be changed if items contain '?'.
		 */
		void AutoCSetTypeSeparator(int separatorCharacter);
		/**
		 * Set the number of spaces used for one level of indentation.
		 */
		void SetIndent(int indentSize);
		/**
		 * Retrieve indentation size.
		 */
		int GetIndent();
		/**
		 * Indentation will only use space characters if useTabs is false, otherwise
		 * it will use a combination of tabs and spaces.
		 */
		void SetUseTabs(bool useTabs);
		/**
		 * Retrieve whether tabs will be used in indentation.
		 */
		bool GetUseTabs();
		/**
		 * Change the indentation of a line to a number of columns.
		 */
		void SetLineIndentation(int line, int indentSize);
		/**
		 * Retrieve the number of columns that a line is indented.
		 */
		int GetLineIndentation(int line);
		/**
		 * Retrieve the position before the first non indentation character on a line.
		 */
		long GetLineIndentPosition(int line);
		/**
		 * Retrieve the column number of a position, taking tab width into account.
		 */
		int GetColumn(long pos);
		/**
		 * Show or hide the horizontal scroll bar.
		 */
		void SetHScrollBar(bool show);
		/**
		 * Is the horizontal scroll bar visible?
		 */
		bool GetHScrollBar();
		/**
		 * Show or hide indentation guides.
		 */
		void SetIndentationGuides(bool show);
		/**
		 * Are the indentation guides visible?
		 */
		bool GetIndentationGuides();
		/**
		 * Set the highlighted indentation guide column.
		 * 0 = no highlighted guide.
		 */
		void SetHighlightGuide(int column);
		/**
		 * Get the highlighted indentation guide column.
		 */
		int GetHighlightGuide();
		/**
		 * Get the position after the last visible characters on a line.
		 */
		int GetLineEndPosition(int line);
		/**
		 * Get the code page used to interpret the bytes of the document as characters.
		 */
		int GetCodePage();
		/**
		 * Get the foreground colour of the caret.
		 */
		COLORREF GetCaretFore();
		/**
		 * In palette mode?
		 */
		bool GetUsePalette();
		/**
		 * In read-only mode?
		 */
		bool GetReadOnly();
		/**
		 * Sets the position of the caret.
		 */
		void SetCurrentPos(long pos);
		/**
		 * Sets the position that starts the selection - this becomes the anchor.
		 */
		void SetSelectionStart(long pos);
		/**
		 * Returns the position at the start of the selection.
		 */
		long GetSelectionStart();
		/**
		 * Sets the position that ends the selection - this becomes the currentPosition.
		 */
		void SetSelectionEnd(long pos);
		/**
		 * Returns the position at the end of the selection.
		 */
		long GetSelectionEnd();
		/**
		 * Sets the print magnification added to the point size of each style for printing.
		 */
		void SetPrintMagnification(int magnification);
		/**
		 * Returns the print magnification.
		 */
		int GetPrintMagnification();
		/**
		 * Modify colours when printing for clearer printed text.
		 */
		void SetPrintColourMode(int mode);
		/**
		 * Returns the print colour mode.
		 */
		int GetPrintColourMode();
		/**
		 * Find some text in the document.
		 */
		long FindText(int flags, Scintilla::TextToFind* ft);
		/**
		 * On Windows, will draw the document into a display context such as a printer.
		 */
		long FormatRange(bool draw, long fr);
		/**
		 * Retrieve the display line at the top of the display.
		 */
		int GetFirstVisibleLine();
		/**
		 * Retrieve the contents of a line.
		 * Returns the length of the line.
		 */
		int GetLine(int line, char* text);
		/**
		 * Returns the number of lines in the document. There is always at least one.
		 */
		int GetLineCount();
		/**
		 * Sets the size in pixels of the left margin.
		 */
		void SetMarginLeft(int pixelWidth);
		/**
		 * Returns the size in pixels of the left margin.
		 */
		int GetMarginLeft();
		/**
		 * Sets the size in pixels of the right margin.
		 */
		void SetMarginRight(int pixelWidth);
		/**
		 * Returns the size in pixels of the right margin.
		 */
		int GetMarginRight();
		/**
		 * Is the document different from when it was last saved?
		 */
		bool GetModify();
		/**
		 * Select a range of text.
		 */
		void SetSel(long start, long end);
		/**
		 * Retrieve the selected text.
		 * Return the length of the text.
		 */
		int GetSelText(char* text);
		/**
		 * Retrieve the selected text.
		 */
		std::string GetSelText();
		/**
		 * Retrieve a range of text.
		 * Return the length of the text.
		 */
		int GetTextRange(Scintilla::TextRange* tr);
		/**
		 * Retrieve a range of text.
		 */
		std::string CScintilla::GetTextRange(int start, int end);
		/**
		 * Draw the selection in normal style or with selection highlighted.
		 */
		void HideSelection(bool normal);
		/**
		 * Retrieve the x value of the point in the window where a position is displayed.
		 */
		int PointXFromPosition(long pos);
		/**
		 * Retrieve the y value of the point in the window where a position is displayed.
		 */
		int PointYFromPosition(long pos);
		/**
		 * Retrieve the line containing a position.
		 */
		int LineFromPosition(long pos);
		/**
		 * Retrieve the position at the start of a line.
		 */
		long PositionFromLine(int line);
		/**
		 * Scroll horizontally and vertically.
		 */
		void LineScroll(int columns, int lines);
		/**
		 * Ensure the caret is visible.
		 */
		void ScrollCaret();
		/**
		 * Replace the selected text with the argument text.
		 */
		void ReplaceSel(const char* text);
		/**
		 * Set to read only or read write.
		 */
		void SetReadOnly(bool readOnly);
		/**
		 * Null operation.
		 */
		void Null();
		/**
		 * Will a paste succeed?
		 */
		bool CanPaste();
		/**
		 * Are there any undoable actions in the undo history?
		 */
		bool CanUndo();
		/**
		 * Delete the undo history.
		 */
		void EmptyUndoBuffer();
		/**
		 * Undo one action in the undo history.
		 */
		void Undo();
		/**
		 * Cut the selection to the clipboard.
		 */
		void Cut();
		/**
		 * Copy the selection to the clipboard.
		 */
		void Copy();
		/**
		 * Paste the contents of the clipboard into the document replacing the selection.
		 */
		void Paste();
		/**
		 * Clear the selection.
		 */
		void Clear();
		/**
		 * Replace the contents of the document with the argument text.
		 */
		void SetText(const char* text);
		/**
		 * Retrieve all the text in the document.
		 * Returns number of characters retrieved.
		 */
		int GetText(int length, char* text);
		/**
		 * Retrieve the number of characters in the document.
		 */
		int GetTextLength();
		/**
		 * Retrieve a pointer to a function that processes messages for this Scintilla.
		 */
		int GetDirectFunction();
		/**
		 * Retrieve a pointer value to use as the first argument when calling
		 * the function returned by GetDirectFunction.
		 */
		int GetDirectPointer();
		/**
		 * Set to overtype (true) or insert mode.
		 */
		void SetOvertype(bool overtype);
		/**
		 * Returns true if overtype mode is active otherwise false is returned.
		 */
		bool GetOvertype();
		/**
		 * Set the width of the insert mode caret.
		 */
		void SetCaretWidth(int pixelWidth);
		/**
		 * Returns the width of the insert mode caret.
		 */
		int GetCaretWidth();
		/**
		 * Sets the position that starts the target which is used for updating the
		 * document without affecting the scroll position.
		 */
		void SetTargetStart(long pos);
		/**
		 * Get the position that starts the target.
		 */
		long GetTargetStart();
		/**
		 * Sets the position that ends the target which is used for updating the
		 * document without affecting the scroll position.
		 */
		void SetTargetEnd(long pos);
		/**
		 * Get the position that ends the target.
		 */
		long GetTargetEnd();
		/**
		 * Replace the target text with the argument text.
		 * Text is counted so it can contain NULs.
		 * Returns the length of the replacement text.
		 */
		int ReplaceTarget(int length, const char* text);
		/**
		 * Replace the target text with the argument text after \d processing.
		 * Text is counted so it can contain NULs.
		 * Looks for \d where d is between 1 and 9 and replaces these with the strings
		 * matched in the last search operation which were surrounded by \( and \).
		 * Returns the length of the replacement text including any change
		 * caused by processing the \d patterns.
		 */
		int ReplaceTargetRE(int length, const char* text);
		/**
		 * Search for a counted string in the target and set the target to the found
		 * range. Text is counted so it can contain NULs.
		 * Returns length of range or -1 for failure in which case target is not moved.
		 */
		int SearchInTarget(int length, const char* text);
		/**
		 * Set the search flags used by SearchInTarget.
		 */
		void SetSearchFlags(int flags);
		/**
		 * Get the search flags used by SearchInTarget.
		 */
		int GetSearchFlags();
		/**
		 * Show a call tip containing a definition near position pos.
		 */
		void CallTipShow(long pos, const char* definition);
		/**
		 * Remove the call tip from the screen.
		 */
		void CallTipCancel();
		/**
		 * Is there an active call tip?
		 */
		bool CallTipActive();
		/**
		 * Retrieve the position where the caret was before displaying the call tip.
		 */
		long CallTipPosStart();
		/**
		 * Highlight a segment of the definition.
		 */
		void CallTipSetHlt(int start, int end);
		/**
		 * Set the background colour for the call tip.
		 */
		void CallTipSetBack(COLORREF back);
		/**
		 * Set the foreground colour for the call tip.
		 */
		void CallTipSetFore(COLORREF fore);
		/**
		 * Set the foreground colour for the highlighted part of the call tip.
		 */
		void CallTipSetForeHlt(COLORREF fore);
		/**
		 * Find the display line of a document line taking hidden lines into account.
		 */
		int VisibleFromDocLine(int line);
		/**
		 * Find the document line of a display line taking hidden lines into account.
		 */
		int DocLineFromVisible(int lineDisplay);
		/**
		 * Set the fold level of a line.
		 * This encodes an integer level along with flags indicating whether the
		 * line is a header and whether it is effectively white space.
		 */
		void SetFoldLevel(int line, int level);
		/**
		 * Retrieve the fold level of a line.
		 */
		int GetFoldLevel(int line);
		/**
		 * Find the last child line of a header line.
		 */
		int GetLastChild(int line, int level);
		/**
		 * Find the parent line of a child line.
		 */
		int GetFoldParent(int line);
		/**
		 * Make a range of lines visible.
		 */
		void ShowLines(int lineStart, int lineEnd);
		/**
		 * Make a range of lines invisible.
		 */
		void HideLines(int lineStart, int lineEnd);
		/**
		 * Is a line visible?
		 */
		bool GetLineVisible(int line);
		/**
		 * Show the children of a header line.
		 */
		void SetFoldExpanded(int line, bool expanded);
		/**
		 * Is a header line expanded?
		 */
		bool GetFoldExpanded(int line);
		/**
		 * Switch a header line between expanded and contracted.
		 */
		void ToggleFold(int line);
		/**
		 * Ensure a particular line is visible by expanding any header line hiding it.
		 */
		void EnsureVisible(int line);
		/**
		 * Set some style options for folding.
		 */
		void SetFoldFlags(int flags);
		/**
		 * Ensure a particular line is visible by expanding any header line hiding it.
		 * Use the currently set visibility policy to determine which range to display.
		 */
		void EnsureVisibleEnforcePolicy(int line);
		/**
		 * Sets whether a tab pressed when caret is within indentation indents.
		 */
		void SetTabIndents(bool tabIndents);
		/**
		 * Does a tab pressed when caret is within indentation indent?
		 */
		bool GetTabIndents();
		/**
		 * Sets whether a backspace pressed when caret is within indentation unindents.
		 */
		void SetBackSpaceUnIndents(bool bsUnIndents);
		/**
		 * Does a backspace pressed when caret is within indentation unindent?
		 */
		bool GetBackSpaceUnIndents();
		/**
		 * Sets the time the mouse must sit still to generate a mouse dwell event.
		 */
		void SetMouseDwellTime(int periodMilliseconds);
		/**
		 * Retrieve the time the mouse must sit still to generate a mouse dwell event.
		 */
		int GetMouseDwellTime();
		/**
		 * Get position of start of word.
		 */
		int WordStartPosition(long pos, bool onlyWordCharacters);
		/**
		 * Get position of end of word.
		 */
		int WordEndPosition(long pos, bool onlyWordCharacters);
		/**
		 * Sets whether text is word wrapped.
		 */
		void SetWrapMode(int mode);
		/**
		 * Retrieve whether text is word wrapped.
		 */
		int GetWrapMode();
		/**
		 * Sets the degree of caching of layout information.
		 */
		void SetLayoutCache(int mode);
		/**
		 * Retrieve the degree of caching of layout information.
		 */
		int GetLayoutCache();
		/**
		 * Sets the document width assumed for scrolling.
		 */
		void SetScrollWidth(int pixelWidth);
		/**
		 * Retrieve the document width assumed for scrolling.
		 */
		int GetScrollWidth();
		/**
		 * Measure the pixel width of some text in a particular style.
		 * NUL terminated text argument.
		 * Does not handle tab or control characters.
		 */
		int TextWidth(int style, const char* text);
		/**
		 * Sets the scroll range so that maximum scroll position has
		 * the last line at the bottom of the view (default).
		 * Setting this to false allows scrolling one page below the last line.
		 */
		void SetEndAtLastLine(bool endAtLastLine);
		/**
		 * Retrieve whether the maximum scroll position has the last
		 * line at the bottom of the view.
		 */
		int GetEndAtLastLine();
		/**
		 * Retrieve the height of a particular line of text in pixels.
		 */
		int TextHeight(int line);
		/**
		 * Show or hide the vertical scroll bar.
		 */
		void SetVScrollBar(bool show);
		/**
		 * Is the vertical scroll bar visible?
		 */
		bool GetVScrollBar();
		/**
		 * Append a string to the end of the document without changing the selection.
		 */
		void AppendText(int length, const char* text);
		/**
		 * Is drawing done in two phases with backgrounds drawn before faoregrounds?
		 */
		bool GetTwoPhaseDraw();
		/**
		 * In twoPhaseDraw mode, drawing is performed in two phases, first the background
		 * and then the foreground. This avoids chopping off characters that overlap the next run.
		 */
		void SetTwoPhaseDraw(bool twoPhase);
		/**
		 * Make the target range start and end be the same as the selection range start and end.
		 */
		void TargetFromSelection();
		/**
		 * Join the lines in the target.
		 */
		void LinesJoin();
		/**
		 * Split the lines in the target into lines that are less wide than pixelWidth
		 * where possible.
		 */
		void LinesSplit(int pixelWidth);
		/**
		 * Set the colours used as a chequerboard pattern in the fold margin
		 */
		void SetFoldMarginColour(bool useSetting, COLORREF back);
		/**
		 * Set the colours used as a chequerboard pattern in the fold margin
		 */
		void SetFoldMarginHiColour(bool useSetting, COLORREF fore);
		/**
		 * Move caret down one line.
		 */
		void LineDown();
		/**
		 * Move caret down one line extending selection to new caret position.
		 */
		void LineDownExtend();
		/**
		 * Move caret up one line.
		 */
		void LineUp();
		/**
		 * Move caret up one line extending selection to new caret position.
		 */
		void LineUpExtend();
		/**
		 * Move caret left one character.
		 */
		void CharLeft();
		/**
		 * Move caret left one character extending selection to new caret position.
		 */
		void CharLeftExtend();
		/**
		 * Move caret right one character.
		 */
		void CharRight();
		/**
		 * Move caret right one character extending selection to new caret position.
		 */
		void CharRightExtend();
		/**
		 * Move caret left one word.
		 */
		void WordLeft();
		/**
		 * Move caret left one word extending selection to new caret position.
		 */
		void WordLeftExtend();
		/**
		 * Move caret right one word.
		 */
		void WordRight();
		/**
		 * Move caret right one word extending selection to new caret position.
		 */
		void WordRightExtend();
		/**
		 * Move caret to first position on line.
		 */
		void Home();
		/**
		 * Move caret to first position on line extending selection to new caret position.
		 */
		void HomeExtend();
		/**
		 * Move caret to last position on line.
		 */
		void LineEnd();
		/**
		 * Move caret to last position on line extending selection to new caret position.
		 */
		void LineEndExtend();
		/**
		 * Move caret to first position in document.
		 */
		void DocumentStart();
		/**
		 * Move caret to first position in document extending selection to new caret position.
		 */
		void DocumentStartExtend();
		/**
		 * Move caret to last position in document.
		 */
		void DocumentEnd();
		/**
		 * Move caret to last position in document extending selection to new caret position.
		 */
		void DocumentEndExtend();
		/**
		 * Move caret one page up.
		 */
		void PageUp();
		/**
		 * Move caret one page up extending selection to new caret position.
		 */
		void PageUpExtend();
		/**
		 * Move caret one page down.
		 */
		void PageDown();
		/**
		 * Move caret one page down extending selection to new caret position.
		 */
		void PageDownExtend();
		/**
		 * Switch from insert to overtype mode or the reverse.
		 */
		void EditToggleOvertype();
		/**
		 * Cancel any modes such as call tip or auto-completion list display.
		 */
		void Cancel();
		/**
		 * Delete the selection or if no selection, the character before the caret.
		 */
		void DeleteBack();
		/**
		 * If selection is empty or all on one line replace the selection with a tab character.
		 * If more than one line selected, indent the lines.
		 */
		void Tab();
		/**
		 * Dedent the selected lines.
		 */
		void BackTab();
		/**
		 * Insert a new line, may use a CRLF, CR or LF depending on EOL mode.
		 */
		void NewLine();
		/**
		 * Insert a Form Feed character.
		 */
		void FormFeed();
		/**
		 * Move caret to before first visible character on line.
		 * If already there move to first character on line.
		 */
		void VCHome();
		/**
		 * Like VCHome but extending selection to new caret position.
		 */
		void VCHomeExtend();
		/**
		 * Magnify the displayed text by increasing the sizes by 1 point.
		 */
		void ZoomIn();
		/**
		 * Make the displayed text smaller by decreasing the sizes by 1 point.
		 */
		void ZoomOut();
		/**
		 * Delete the word to the left of the caret.
		 */
		void DelWordLeft();
		/**
		 * Delete the word to the right of the caret.
		 */
		void DelWordRight();
		/**
		 * Cut the line containing the caret.
		 */
		void LineCut();
		/**
		 * Delete the line containing the caret.
		 */
		void LineDelete();
		/**
		 * Switch the current line with the previous.
		 */
		void LineTranspose();
		/**
		 * Duplicate the current line.
		 */
		void LineDuplicate();
		/**
		 * Transform the selection to lower case.
		 */
		void LowerCase();
		/**
		 * Transform the selection to upper case.
		 */
		void UpperCase();
		/**
		 * Scroll the document down, keeping the caret visible.
		 */
		void LineScrollDown();
		/**
		 * Scroll the document up, keeping the caret visible.
		 */
		void LineScrollUp();
		/**
		 * Delete the selection or if no selection, the character before the caret.
		 * Will not delete the character before at the start of a line.
		 */
		void DeleteBackNotLine();
		/**
		 * Move caret to first position on display line.
		 */
		void HomeDisplay();
		/**
		 * Move caret to first position on display line extending selection to
		 * new caret position.
		 */
		void HomeDisplayExtend();
		/**
		 * Move caret to last position on display line.
		 */
		void LineEndDisplay();
		/**
		 * Move caret to last position on display line extending selection to new
		 * caret position.
		 */
		void LineEndDisplayExtend();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void HomeWrap();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void HomeWrapExtend();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void LineEndWrap();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void LineEndWrapExtend();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void VCHomeWrap();
		/**
		 * These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
		 * except they behave differently when word-wrap is enabled:
		 * They go first to the start / end of the display line, like (Home|LineEnd)Display
		 * The difference is that, the cursor is already at the point, it goes on to the start
		 * or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
		 */
		void VCHomeWrapExtend();
		/**
		 * Copy the line containing the caret.
		 */
		void LineCopy();
		/**
		 * Move the caret inside current view if it's not there already.
		 */
		void MoveCaretInsideView();
		/**
		 * How many characters are on a line, not including end of line characters?
		 */
		int LineLength(int line);
		/**
		 * Highlight the characters at two positions.
		 */
		void BraceHighlight(long pos1, long pos2);
		/**
		 * Highlight the character at a position indicating there is no matching brace.
		 */
		void BraceBadLight(long pos);
		/**
		 * Find the position of a matching brace or INVALID_POSITION if no match.
		 */
		long BraceMatch(long pos);
		/**
		 * Are the end of line characters visible?
		 */
		bool GetViewEOL();
		/**
		 * Make the end of line characters visible or invisible.
		 */
		void SetViewEOL(bool visible);
		/**
		 * Retrieve a pointer to the document object.
		 */
		int GetDocPointer();
		/**
		 * Change the document object used.
		 */
		void SetDocPointer(int pointer);
		/**
		 * Set which document modification events are sent to the container.
		 */
		void SetModEventMask(int mask);
		/**
		 * Retrieve the column number which text should be kept within.
		 */
		int GetEdgeColumn();
		/**
		 * Set the column number of the edge.
		 * If text goes past the edge then it is highlighted.
		 */
		void SetEdgeColumn(int column);
		/**
		 * Retrieve the edge highlight mode.
		 */
		int GetEdgeMode();
		/**
		 * The edge may be displayed by a line (EDGE_LINE) or by highlighting text that
		 * goes beyond it (EDGE_BACKGROUND) or not displayed at all (EDGE_NONE).
		 */
		void SetEdgeMode(int mode);
		/**
		 * Retrieve the colour used in edge indication.
		 */
		COLORREF GetEdgeColour();
		/**
		 * Change the colour used in edge indication.
		 */
		void SetEdgeColour(COLORREF edgeColour);
		/**
		 * Sets the current caret position to be the search anchor.
		 */
		void SearchAnchor();
		/**
		 * Find some text starting at the search anchor.
		 * Does not ensure the selection is visible.
		 */
		int SearchNext(int flags, const char* text);
		/**
		 * Find some text starting at the search anchor and moving backwards.
		 * Does not ensure the selection is visible.
		 */
		int SearchPrev(int flags, const char* text);
		/**
		 * Retrieves the number of lines completely visible.
		 */
		int LinesOnScreen();
		/**
		 * Set whether a pop up menu is displayed automatically when the user presses
		 * the wrong mouse button.
		 */
		void UsePopUp(bool allowPopUp);
		/**
		 * Is the selection rectangular? The alternative is the more common stream selection.
		 */
		bool SelectionIsRectangle();
		/**
		 * Set the zoom level. This number of points is added to the size of all fonts.
		 * It may be positive to magnify or negative to reduce.
		 */
		void SetZoom(int zoom);
		/**
		 * Retrieve the zoom level.
		 */
		int GetZoom();
		/**
		 * Create a new document object.
		 * Starts with reference count of 1 and not selected into editor.
		 */
		int CreateDocument();
		/**
		 * Extend life of document.
		 */
		void AddRefDocument(int doc);
		/**
		 * Release a reference to the document, deleting document if it fades to black.
		 */
		void ReleaseDocument(int doc);
		/**
		 * Get which document modification events are sent to the container.
		 */
		int GetModEventMask();
		/**
		 * Change internal focus flag.
		 */
		void SetEditorFocus(bool focus);
		/**
		 * Get internal focus flag.
		 */
		bool GetFocus();
		/**
		 * Change error status - 0 = OK.
		 */
		void SetStatus(int statusCode);
		/**
		 * Get error status.
		 */
		int GetStatus();
		/**
		 * Set whether the mouse is captured when its button is pressed.
		 */
		void SetMouseDownCaptures(bool captures);
		/**
		 * Get whether mouse gets captured.
		 */
		bool GetMouseDownCaptures();
		/**
		 * Sets the cursor to one of the SC_CURSOR* values.
		 */
		void SetCursor(int cursorType);
		/**
		 * Get cursor type.
		 */
		int GetCursor();
		/**
		 * Change the way control characters are displayed:
		 * If symbol is < 32, keep the drawn way, else, use the given character.
		 */
		void SetControlCharSymbol(int symbol);
		/**
		 * Get the way control characters are displayed.
		 */
		int GetControlCharSymbol();
		/**
		 * Move to the previous change in capitalisation.
		 */
		void WordPartLeft();
		/**
		 * Move to the previous change in capitalisation extending selection
		 * to new caret position.
		 */
		void WordPartLeftExtend();
		/**
		 * Move to the change next in capitalisation.
		 */
		void WordPartRight();
		/**
		 * Move to the next change in capitalisation extending selection
		 * to new caret position.
		 */
		void WordPartRightExtend();
		/**
		 * Set the way the display area is determined when a particular line
		 * is to be moved to by Find, FindNext, GotoLine, etc.
		 */
		void SetVisiblePolicy(int visiblePolicy, int visibleSlop);
		/**
		 * Delete back from the current position to the start of the line.
		 */
		void DelLineLeft();
		/**
		 * Delete forwards from the current position to the end of the line.
		 */
		void DelLineRight();
		/**
		 * Get and Set the xOffset (ie, horizonal scroll position).
		 */
		void SetXOffset(int newOffset);
		/**
		 * Get and Set the xOffset (ie, horizonal scroll position).
		 */
		int GetXOffset();
		/**
		 * Set the last x chosen value to be the caret x position.
		 */
		void ChooseCaretX();
		/**
		 * Set the focus to this Scintilla widget.
		 * GTK+ Specific.
		 */
		void GrabFocus();
		/**
		 * Set the way the caret is kept visible when going sideway.
		 * The exclusion zone is given in pixels.
		 */
		void SetXCaretPolicy(int caretPolicy, int caretSlop);
		/**
		 * Set the way the line the caret is on is kept visible.
		 * The exclusion zone is given in lines.
		 */
		void SetYCaretPolicy(int caretPolicy, int caretSlop);
		/**
		 * Set printing to line wrapped (SC_WRAP_WORD) or not line wrapped (SC_WRAP_NONE).
		 */
		void SetPrintWrapMode(int mode);
		/**
		 * Is printing line wrapped?
		 */
		int GetPrintWrapMode();
		/**
		 * Set a fore colour for active hotspots.
		 */
		void SetHotspotActiveFore(bool useSetting, COLORREF fore);
		/**
		 * Set a back colour for active hotspots.
		 */
		void SetHotspotActiveBack(bool useSetting, COLORREF back);
		/**
		 * Enable / Disable underlining active hotspots.
		 */
		void SetHotspotActiveUnderline(bool underline);
		/**
		 * Limit hotspots to single line so hotspots on two lines don't merge.
		 */
		void SetHotspotSingleLine(bool singleLine);
		/**
		 * Move caret between paragraphs (delimited by empty lines).
		 */
		void ParaDown();
		/**
		 * Move caret between paragraphs (delimited by empty lines).
		 */
		void ParaDownExtend();
		/**
		 * Move caret between paragraphs (delimited by empty lines).
		 */
		void ParaUp();
		/**
		 * Move caret between paragraphs (delimited by empty lines).
		 */
		void ParaUpExtend();
		/**
		 * Given a valid document position, return the previous position taking code
		 * page into account. Returns 0 if passed 0.
		 */
		long PositionBefore(long pos);
		/**
		 * Given a valid document position, return the next position taking code
		 * page into account. Maximum value returned is the last position in the document.
		 */
		long PositionAfter(long pos);
		/**
		 * Copy a range of text to the clipboard. Positions are clipped into the document.
		 */
		void CopyRange(long start, long end);
		/**
		 * Copy argument text to the clipboard.
		 */
		void CopyText(int length, const char* text);
		/**
		 * Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE) or
		 * by lines (SC_SEL_LINES).
		 */
		void SetSelectionMode(int mode);
		/**
		 * Get the mode of the current selection.
		 */
		int GetSelectionMode();
		/**
		 * Retrieve the position of the start of the selection at the given line (INVALID_POSITION if no selection on this line).
		 */
		long GetLineSelStartPosition(int line);
		/**
		 * Retrieve the position of the end of the selection at the given line (INVALID_POSITION if no selection on this line).
		 */
		long GetLineSelEndPosition(int line);
		/**
		 * Move caret down one line, extending rectangular selection to new caret position.
		 */
		void LineDownRectExtend();
		/**
		 * Move caret up one line, extending rectangular selection to new caret position.
		 */
		void LineUpRectExtend();
		/**
		 * Move caret left one character, extending rectangular selection to new caret position.
		 */
		void CharLeftRectExtend();
		/**
		 * Move caret right one character, extending rectangular selection to new caret position.
		 */
		void CharRightRectExtend();
		/**
		 * Move caret to first position on line, extending rectangular selection to new caret position.
		 */
		void HomeRectExtend();
		/**
		 * Move caret to before first visible character on line.
		 * If already there move to first character on line.
		 * In either case, extend rectangular selection to new caret position.
		 */
		void VCHomeRectExtend();
		/**
		 * Move caret to last position on line, extending rectangular selection to new caret position.
		 */
		void LineEndRectExtend();
		/**
		 * Move caret one page up, extending rectangular selection to new caret position.
		 */
		void PageUpRectExtend();
		/**
		 * Move caret one page down, extending rectangular selection to new caret position.
		 */
		void PageDownRectExtend();
		/**
		 * Move caret to top of page, or one page up if already at top of page.
		 */
		void StutteredPageUp();
		/**
		 * Move caret to top of page, or one page up if already at top of page, extending selection to new caret position.
		 */
		void StutteredPageUpExtend();
		/**
		 * Move caret to bottom of page, or one page down if already at bottom of page.
		 */
		void StutteredPageDown();
		/**
		 * Move caret to bottom of page, or one page down if already at bottom of page, extending selection to new caret position.
		 */
		void StutteredPageDownExtend();
		/**
		 * Move caret left one word, position cursor at end of word.
		 */
		void WordLeftEnd();
		/**
		 * Move caret left one word, position cursor at end of word, extending selection to new caret position.
		 */
		void WordLeftEndExtend();
		/**
		 * Move caret right one word, position cursor at end of word.
		 */
		void WordRightEnd();
		/**
		 * Move caret right one word, position cursor at end of word, extending selection to new caret position.
		 */
		void WordRightEndExtend();
		/**
		 * Set the set of characters making up whitespace for when moving or selecting by word.
		 * Should be called after SetWordChars.
		 */
		void SetWhitespaceChars(const char* characters);
		/**
		 * Reset the set of characters for whitespace and word characters to the defaults.
		 */
		void SetCharsDefault();
		/**
		 * Get currently selected item position in the auto-completion list
		 */
		int AutoCGetCurrent();
		/**
		 * Enlarge the document to a particular size of text bytes.
		 */
		void Allocate(int bytes);
		/**
		 * retrieves the value of the target encoded as UTF-8
		 */
		int TargetAsUTF8(char *s);
		/**
		 * Toggle Caret Sticky
		 */
		void ToggleCaretSticky();
		/**
		 * Get whether the sticky caret mode is enabled
		 */
		bool GetCaretSticky();
		/**
		 * Set whether the sticky caret mode is enabled
		 */
		void SetCaretSticky(bool sticky);
		/**
		 * Returns the position of a column on a line taking the width of tabs into account
		 */
		int FindColumn(int line, int column);
		/**
		 * Get an integer property value
		 */
		int GetPropertyInt(const char* key, int defaultVal);
		/**
		 * Start notifying the container of all key presses and commands.
		 */
		void StartRecord();
		/**
		 * Stop notifying the container of all key presses and commands.
		 */
		void StopRecord();
		/**
		 * Set the lexing language of the document.
		 */
		void SetLexer(int lexer);
		/**
		 * Retrieve the lexing language of the document.
		 */
		int GetLexer();
		/**
		 * Colourise a segment of the document using the current lexing language.
		 */
		void Colourise(long start, long end);
		/**
		 * Set up a value that may be used by a lexer for some optional feature.
		 */
		void SetProperty(const char* key, const char* value);
		/**
		 * Set up the key words used by the lexer.
		 * PN: virtual for use in storing keywords for autocompletion
		 */
		virtual void SetKeyWords(int keywordSet, const char* keyWords);
		/**
		 * Set the lexing language of the document based on string name.
		 */
		void SetLexerLanguage(const char* language);
		/**
		 * Load a lexer library (dll / so).
		 */
		void LoadLexerLibrary(const char* path);
		/**
		 * Enable/Disable convert-on-paste for line endings
		 */
		void SetPasteConvertEndings(bool convert);
		/**
		 * Get convert-on-paste setting
		 */
		bool GetPasteConvertEndings();
		/**
		 * Set current indicator number
		 */
		void SetIndicatorCurrent(int indicator);
		/**
		 * Set the value to use for the next indicator range
		 */
		void SetIndicatorValue(int value);
		/**
		 * Fill indicator range
		 */
		void IndicatorFillRange(int start, int length);
		/**
		 * Clear indicator range
		 */
		void IndicatorClearRange(int start, int length);
	//--
	//@}

		int GetSelections();

		void ClearSelections();
		void SetSelection(int caret, int anchor);
		void AddSelection(int caret, int anchor);
		void SetMainSelection(int selection);
		int GetMainSelection();

		void SetSelectionNCaret(int selection, int pos);
		int GetSelectionNCaret(int selection);
		void SetSelectionNCaretVirtualSpace(int selection, int space);
		int GetSelectionNCaretVirtualSpace(int selection);
		void SetSelectionNAnchor(int selection, int posAnchor);
		int GetSelectionNAnchor(int selection);
		void SetSelectionNAnchorVirtualSpace(int selection, int space);
		int GetSelectionNAnchorVirtualSpace(int selection);
		void SetSelectionNStart(int selection, int pos);
		int GetSelectionNStart(int selection);
		void SetSelectionNEnd(int selection, int pos);
		int GetSelectionNEnd(int selection);

		void SetRectangularSelectionCaret(int pos);
		int GetRectangularSelectionCaret();
		void SetRectangularSelectionCaretVirtualSpace(int space);
		int GetRectangularSelectionCaretVirtualSpace();
		void SetRectangularSelectionAnchor(int posAnchor);
		int GetRectangularSelectionAnchor();
		void SetRectangularSelectionAnchorVirtualSpace(int space);
		int GetRectangularSelectionAnchorVirtualSpace();

		void SetAdditionalSelAlpha(int alpha);
		int GetAdditionalSelAlpha();
		void SetAdditionalSelFore(int colour);
		void SetAdditionalSelBack(int colour);
		void SetAdditionalCaretFore(int colour);
		int GetAdditionalCaretFore();
		void SetAdditionalCaretsBlink(bool additionalCaretsBlink);
		bool GetAdditionalCaretsBlink();

		void SwapMainAnchorCaret();
		void RotateSelection();
};

/**
 * CRecordingScintilla is a class for recording messages passed to
 * the scintilla interface. It was originally developed so that
 * scintilla settings could be represented in a binary form.
 */
class CRecordingScintilla : public CScintilla
{
	protected:
		virtual void Record(long Msg, WPARAM wParam, LPARAM lParam) = 0;
	public:
		inline long SPerform(long Msg, WPARAM wParam=0, LPARAM lParam=0)
		{
			// Record Message...
			Record(Msg, wParam, lParam);
			return 0;
		}	
};

#endif