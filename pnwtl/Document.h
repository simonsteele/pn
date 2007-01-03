/**
 * @file Document.h
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47
#define document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47

class CChildFrame;

class Document : public extensions::IDocument, public extensions::ITextEditorEventSink
{
	friend class CChildFrame;

	typedef std::list<extensions::IDocumentEventSinkPtr> EventSinks;

	public:
		Document(LPCTSTR filename = NULL);
		virtual ~Document();

		void AddChildFrame(CChildFrame* pFrame);

		bool FileExists() const;

		long GetFileAge() const;
		tstring GetFileName(EGFNType type) const;
		CChildFrame* GetFrame() const;
		bool HasFile() const;

		void SetFileName(LPCTSTR filename);

		void OnDocClosing();

// IDocument members
	public:
		virtual const char* GetTitle() const;
		virtual const char* GetFileName() const;
		virtual const char* GetCurrentScheme() const;
		virtual HWND GetScintillaHWND() const;
		virtual bool GetModified() const;
		virtual bool GetCanSave() const;

		virtual bool Save(const char* filename, bool setFilename);

		virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam);
		virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam);

		virtual bool IsValid() const;

		virtual void AddEventSink(extensions::IDocumentEventSinkPtr sink);
		virtual void RemoveEventSink(extensions::IDocumentEventSinkPtr sink);

		virtual FindNextResult FindNext(extensions::ISearchOptions* options);
		virtual bool Replace(extensions::ISearchOptions* options);
		virtual int ReplaceAll(extensions::ISearchOptions* options);

		virtual void Close(bool dontAskUserIfUnsaved);

// ITextEditorEventSink members
	public:
		virtual void OnSchemeChange(const char* scheme);
		virtual void OnCharAdded(char c);

// Protected members for friend classes...
	protected:
		void SetValid(bool bValid);

	protected:
		CChildFrame*	m_pFrame;
		bool			m_bIsValid;
		tstring			m_sFilename;
		EventSinks		m_sinks;
};

#endif // #ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47