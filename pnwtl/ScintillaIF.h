/**
 * @file ScintillaIF.h
 * @brief Define scintilla wrapper class CScintilla
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef __SCINTILLAIF_H__
#define __SCINTILLAIF_H__

//#define STATIC_SCILEXER
#define WTL_SCINTILLA 1
#define PLAT_WIN 1

#include "scintilla.h"

//! Block size for disk reading and writing.
const int blockSize = 131072;

//! Scintilla direct message function.
typedef long(__cdecl* scmsgfn)(void *ptr, long Msg, WPARAM wParam, LPARAM lParam);

typedef enum {efsVSNet, efsVSNetR, efsPlus, efsArrow} EFoldStyle;

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
		~CScintilla();

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
		virtual inline long SPerform(long Msg, WPARAM wParam=0, LPARAM lParam=0)
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

		int GetSelLengh(){return m_SelLength;}

		// The following are helper functions - they allow
		// the code using certain parts of Scintilla to look
		// a little cleaner.
		void GetSel(CharacterRange& cr);
		void EnsureRangeVisible(int begin, int end);
		void SetTarget(int begin, int end);
		void SetTarget(CharacterRange* cr);
		
		void DefineMarker(int marker, int markerType, COLORREF fore, COLORREF back);

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
		
		//! Is text modified?
		bool m_Modified;
		//! Locally used method to get a range of text.
		void GetRange(int start, int end, char *text);

		int	m_TabWidth;

		int m_SelLength;
	
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
		 * Add text to the document
		 */
		void AddText(int length, const char* text);
		/**
		 * Add array of cells to document
		 */
		void AddStyledText(int length, char* c);
		/**
		 * Insert string at a position
		 */
		void InsertText(long pos, const char* text);
		/**
		 * Delete all text in the document
		 */
		void ClearAll();
		/**
		 * Set all style bytes to 0, remove all folding information
		 */
		void ClearDocumentStyle();
		/**
		 * The number of characters in the document
		 */
		int GetLength();
		/**
		 * Returns the character byte at the position
		 */
		int GetCharAt(long pos);
		/**
		 * Returns the position of the caret
		 */
		long GetCurrentPos();
		/**
		 * Returns the position of the opposite end of the selection to the caret
		 */
		long GetAnchor();
		/**
		 * Returns the style byte at the position
		 */
		int GetStyleAt(long pos);
		/**
		 * Redoes the next action on the undo history
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
		 * Returns the number of bytes in the buffer not including terminating nulls.
		 */
		int GetStyledText(TextRange* tr);
		/**
		 * Are there any redoable actions in the undo history.
		 */
		bool CanRedo();
		/**
		 * Retrieve the line number at which a particular marker is located
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
		int PositionFromPoint(int x, int y);
		/**
		 * Find the position from a point within the window but return
		 * INVALID_POSITION if not close to text.
		 */
		int PositionFromPointClose(int x, int y);
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
		 * The styling mask can be used to protect some bits in each styling byte from
		 * modification.
		 */
		void StartStyling(long pos, int mask);
		/**
		 * Change style from current styling position for length characters to a style
		 * and move the current styling position to after this newly styled segment.
		 */
		void SetStyling(int length, int style);
		/**
		 * Is drawing done first into a buffer or direct to the screen.
		 */
		bool GetBufferedDraw();
		/**
		 * If drawing is buffered then each line of text is drawn into a bitmap buffer
		 * before drawing it to the screen to avoid flicker.
		 */
		void SetBufferedDraw(bool buffered);
		/**
		 * Change the visible size of a tab to be a multiple of the width of a space
		 * character.
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
		 * In palette mode, Scintilla uses the environments palette calls to display
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
		 * Delete a marker from a line
		 */
		void MarkerDelete(int line, int markerNumber);
		/**
		 * Delete all markers with a particular number from all lines
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
		 * When key+modifier combination km do nothing.
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
		 * Set the set of characters making up words for when moving or selecting
		 * by word.
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
		 * Divide each styling byte into lexical class bits (default:5) and indicator
		 * bits (default:3). If a lexer requires more than 32 lexical states, then this
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
		 * Dsplay the background of the line containing the caret in a different colour.
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
		 * Retrieve the position of the caret when the auto-completion list was
		 * displayed.
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
		 * Change the separator character in the string setting up an auto-completion
		 * list. Default is space but can be changed if items contain space.
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
		 * Set whether or not autocompletion is hidden automatically when nothing matches
		 */
		void AutoCSetAutoHide(bool autoHide);
		/**
		 * Retrieve whether or not autocompletion is hidden automatically when nothing matches
		 */
		bool AutoCGetAutoHide();
		/**
		 * Set whether or not autocompletion deletes any word characters after the inserted text upon completion
		 */
		void AutoCSetDropRestOfWord(bool dropRestOfWord);
		/**
		 * Retrieve whether or not autocompletion deletes any word characters after the inserted text upon completion
		 */
		bool AutoCGetDropRestOfWord();
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
		long FindText(int flags, TextToFind* ft);
		/**
		 * On Windows will draw the document into a display context such as a printer.
		 */
		void FormatRange(bool draw, long fr);
		/**
		 * Retrieve the line at the top of the display.
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
		 * Retrieve a range of text.
		 * Return the length of the text.
		 */
		int GetTextRange(TextRange* tr);
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
		int PositionFromLine(int line);
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
		 * Are there any undoable actions in the undo history.
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
		 * Returns the width of the insert mode caret
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
		 * Text is counted so it can contain nulls.
		 * Returns the length of the replacement text.
		 */
		int ReplaceTarget(int length, const char* text);
		/**
		 * Replace the target text with the argument text after \d processing.
		 * Text is counted so it can contain nulls.
		 * Looks for \d where d is between 1 and 9 and replaces these with the strings
		 * matched in the last search operation which were surrounded by \( and \).
		 * Returns the length of the replacement text including any change
		 * caused by processing the \d patterns.
		 */
		int ReplaceTargetRE(int length, const char* text);
		/**
		 * Search for a counted string in the target and set the target to the found
		 * range. Text is counted so it can contain nulls.
		 * Returns length of range or -1 for failure in which case target is not moved.
		 */
		int SearchInTarget(int length, const char* text);
		/**
		 * Set the search flags used by SearchInTarget
		 */
		void SetSearchFlags(int flags);
		/**
		 * Get the search flags used by SearchInTarget
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
		 * Set some debugging options for folding
		 */
		void SetFoldFlags(int flags);
		/**
		 * Ensure a particular line is visible by expanding any header line hiding it.
		 * Use the currently set visibility policy to determine which range to display.
		 */
		void EnsureVisibleEnforcePolicy(int line);
		/**
		 * Sets whether a tab pressed when caret is within indentation indents
		 */
		void SetTabIndents(bool tabIndents);
		/**
		 * Does a tab pressed when caret is within indentation indent?
		 */
		bool GetTabIndents();
		/**
		 * Sets whether a backspace pressed when caret is within indentation unindents
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
		 * Nul terminated text argument.
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
		 * If selection is empty or all on one line replace the selection with a tab
		 * character.
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
		 * Will not delete the chraacter before at the start of a line.
		 */
		void DeleteBackNotLine();
		/**
		 * Move the caret inside current view if it's not there already
		 */
		void MoveCaretInsideView();
		/**
		 * How many characters are on a line, not including end of line characters.
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
		 * Are the end of line characters visible.
		 */
		bool GetViewEOL();
		/**
		 * Make the end of line characters visible or invisible
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
		 * Set the way the line the caret is on is kept visible.
		 */
		void SetCaretPolicy(int caretPolicy, int caretSlop);
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
		 * Is the selection a rectangular. The alternative is the more common stream selection.
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
		 * Change internal focus flag
		 */
		void SetFocus(bool focus);
		/**
		 * Get internal focus flag
		 */
		bool GetFocus();
		/**
		 * Change error status - 0 = OK
		 */
		void SetStatus(int statusCode);
		/**
		 * Get error status
		 */
		int GetStatus();
		/**
		 * Set whether the mouse is captured when its button is pressed
		 */
		void SetMouseDownCaptures(bool captures);
		/**
		 * Get whether mouse gets captured
		 */
		bool GetMouseDownCaptures();
		/**
		 * Sets the cursor to one of the SC_CURSOR* values
		 */
		void SetCursor(int cursorType);
		/**
		 * Get cursor type
		 */
		int GetCursor();
		/**
		 * Change the way control characters are displayed:
		 * If symbol is < 32, keep the drawn way, else, use the given character
		 */
		void SetControlCharSymbol(int symbol);
		/**
		 * Get the way control characters are displayed
		 */
		int GetControlCharSymbol();
		/**
		 * Move to the previous change in capitalistion
		 */
		void WordPartLeft();
		/**
		 * Move to the previous change in capitalistion extending selection to new caret position.
		 */
		void WordPartLeftExtend();
		/**
		 * Move to the change next in capitalistion
		 */
		void WordPartRight();
		/**
		 * Move to the next change in capitalistion extending selection to new caret position.
		 */
		void WordPartRightExtend();
		/**
		 * Set the way the display area is determined when a particular line is to be moved to.
		 */
		void SetVisiblePolicy(int visiblePolicy, int visibleSlop);
		/**
		 * Delete back from the current position to the start of the line
		 */
		void DelLineLeft();
		/**
		 * Delete forwards from the current position to the end of the line
		 */
		void DelLineRight();
		/**
		 * Get and Set the xOffset (ie, horizonal scroll position)
		 */
		void SetXOffset(int newOffset);
		/**
		 * Get and Set the xOffset (ie, horizonal scroll position)
		 */
		int GetXOffset();
		/**
		 * Set the focus to this Scintilla widget.
		 * GTK+ Specific
		 */
		void GrabFocus();
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
		 */
		void SetKeyWords(int keywordSet, const char* keyWords);
		/**
		 * Set the lexing language of the document based on string name.
		 */
		void SetLexerLanguage(const char* language);
	//--
	//@}
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