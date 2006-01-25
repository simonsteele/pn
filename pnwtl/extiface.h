#ifndef extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26
#define extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26

namespace extensions
{

#define PN_EXT_IFACE_VERSION	1

class IPN;
class IDocument;
class IAppEventSink;
class IDocumentEventSink;
class IScriptRegistry;

typedef boost::shared_ptr<IDocument> IDocumentPtr;
typedef boost::shared_ptr<IDocumentEventSink> IDocumentEventSinkPtr;
typedef boost::shared_ptr<IAppEventSink> IAppEventSinkPtr;

class IPN
{
public:
	virtual ~IPN(){}

	virtual unsigned int get_iface_version() const = 0;
	virtual const char* get_version() const = 0;
	
	virtual void AddEventSink(IAppEventSinkPtr sink) = 0;
	virtual void RemoveEventSink(IAppEventSinkPtr sink) = 0;

	virtual IScriptRegistry* GetScriptRegistry() = 0;
};

class IDocument
{
public:
	virtual ~IDocument(){}

	virtual const char* GetTitle() const = 0;
	virtual const char* GetFileName() const = 0;
	virtual const char* GetCurrentScheme() const = 0;

	virtual HWND GetScintillaHWND() const = 0;

	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam) = 0;
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam) = 0;

	virtual bool IsValid() const = 0;

	virtual void AddEventSink(IDocumentEventSinkPtr sink) = 0;
	virtual void RemoveEventSink(IDocumentEventSinkPtr sink) = 0;
};

class IAppEventSink
{
public:
	virtual ~IAppEventSink(){}

	virtual void on_new_document(IDocumentPtr doc) = 0;
};

class ITextEditorEventSink
{
public:
	virtual ~ITextEditorEventSink(){}

    virtual void OnCharAdded(char c) = 0;
};

class IDocumentEventSink : public ITextEditorEventSink
{
public:
	virtual ~IDocumentEventSink(){}

	virtual void OnSchemeChange(const char* scheme) = 0;
	virtual void OnDocClosing() = 0;
};

/**
 * Interface for something that can run scripts from the 
 * script manager
 */
class IScriptRunner
{
public:
	virtual void RunScript(const char* name) = 0;
};

/**
 * Interface for the script registry
 */
class IScriptRegistry
{
public:
	virtual void Add(const char* group, const char* name, const char* scriptref) = 0;

	virtual void RegisterRunner(const char* id, extensions::IScriptRunner* runner) = 0;
	virtual void RemoveRunner(const char* id) = 0;
	virtual extensions::IScriptRunner* GetRunner(const char* id) = 0;
};

// Make sure you export a function that looks like this:
// bool init_pn_extension(int iface_version, IPN* pn);
//  - return false if your iface_version does not match and you will
//    be safely unloaded.
// void exit_pn_extension();

// Maybe later: get_extension_info()...

typedef bool (__stdcall *pn_ext_init_fn)(int iface_version, IPN* pn);
typedef void (__stdcall *pn_ext_exit_fn)();

} // namespace extensions

#endif