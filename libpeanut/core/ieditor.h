
#ifndef libpeanut_ieditor_h
#define libpeanut_ieditor_h

class IEditor
{
public:
    virtual ~IEditor() {}
    
    virtual DWORD SendMessage(UINT msg, WPARAM wParam, LPARAM lParam) = 0;
    virtual DWORD SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam) = 0;
    
    virtual Scheme* GetCurrentScheme() = 0;
};

class IEditorFrame
{
public:
    virtual ~IEditorFrame() {}
    
    virtual DocumentPtr GetDocument() = 0;
    virtual IEditor* GetTextView() = 0;
    
    virtual FindNextResult FindNext(extensions::ISearchOptions* options) = 0;
    virtual bool Replace(extensions::ISearchOptions* options) = 0;
    virtual int ReplaceAll(extensions::ISearchOptions* options) = 0;
    
    virtual bool GetModified() const = 0;
    virtual bool GetWriteProtect() const = 0;
    virtual bool CanSave() const = 0;
    
    virtual void SetFocus() = 0;
    
    virtual bool SaveFile(LPCTSTR filename, bool setFilename, bool unused);
    virtual bool SaveFile(IFilePtr file, bool clearUndo);
};


#endif
