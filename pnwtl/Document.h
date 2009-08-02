/**
 * @file Document.h
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47
#define document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47

class CChildFrame;

class Document : public extensions::IDocument, public extensions::ITextEditorEventSink
{
	friend class CChildFrame;

	typedef std::vector<extensions::IDocumentEventSinkPtr> EventSinks;
	typedef std::vector<extensions::ITextEditorEventSinkPtr> EditEventSinks;

	public:
		Document(const wchar_t* filename = NULL);
		virtual ~Document();

		void AddChildFrame(CChildFrame* pFrame);

		bool FileExists() const;

		uint64_t GetFileAge() const;
		std::wstring GetFileName(EGFNType type) const;
		CChildFrame* GetFrame() const;
		bool HasFile() const;

		void SetFileName(const wchar_t* filename);

		void OnAfterLoad();
		void OnBeforeSave(const wchar_t* filename);
		void OnAfterSave();
		void OnDocClosing();
		void OnModifiedChanged(bool modified);
		void OnWriteProtectChanged(bool writeProtect);

// IDocument members
	public:
		virtual const wchar_t* GetTitle() const;
		virtual const wchar_t* GetFileName() const;
		virtual const char* GetCurrentScheme() const;
		virtual HWND GetScintillaHWND() const;
		virtual bool GetModified() const;
		virtual bool GetWriteProtect() const;
		virtual bool GetCanSave() const;

		virtual bool Save(const wchar_t* filename, bool setFilename);

		virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam);
		virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam);

		virtual bool IsValid() const;

		virtual void AddEventSink(extensions::IDocumentEventSinkPtr& sink);
		virtual void RemoveEventSink(extensions::IDocumentEventSinkPtr& sink);

		virtual void AddEventSink(extensions::ITextEditorEventSinkPtr& sink);
		virtual void RemoveEventSink(extensions::ITextEditorEventSinkPtr& sink);

		virtual FindNextResult FindNext(extensions::ISearchOptions* options);
		virtual bool Replace(extensions::ISearchOptions* options);
		virtual int ReplaceAll(extensions::ISearchOptions* options);

		virtual void Close(bool dontAskUserIfUnsaved);

		virtual void Activate();

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
		std::wstring	m_sFilename;
		tstring			m_sTitle;
		EventSinks		m_sinks;
		EditEventSinks	m_editSinks;
};

#endif // #ifndef document_h__included_D464731B_1039_49da_A86C_5CB5F08CDD47