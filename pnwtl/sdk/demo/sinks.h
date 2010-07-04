#ifndef SINKS_H
#define SINKS_H

class TextEventSink : public extensions::ITextEditorEventSink
{
public:
	virtual ~TextEventSink(){}

	virtual void OnCharAdded(char c)
	{
		char buf[2];
		buf[0] = c;
		buf[1] = '\0';
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Char: ");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(&buf[0]);
	}
};

class DocEventSink : public extensions::IDocumentEventSink
{
public:
	DocEventSink(extensions::IDocumentPtr doc) : m_doc(doc)
	{
	}

	virtual ~DocEventSink(){}

	/// Called when the scheme changes
	virtual void OnSchemeChange(const char* scheme)
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Scheme Changed:");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(scheme);
	}
	
	/// Called when the document closes
	virtual void OnDocClosing()
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Document Closing:");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(m_doc->GetTitle());
		m_doc.reset();
	}

	/// Called after a document is loaded
	virtual void OnAfterLoad()
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Document Loaded:");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(m_doc->GetTitle());
	}

	/// Called before the document is saved
	virtual void OnBeforeSave(const char* filename)
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Document about to be saved:");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(m_doc->GetTitle());
	}

	/// Called after the document is saved
	virtual void OnAfterSave()
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Document Saved:");
		g_pn->GetGlobalOutputWindow()->AddToolOutput(m_doc->GetTitle());
	}

	/// Called when the document modified state changes
	virtual void OnModifiedChanged(bool modified)
	{
		g_pn->GetGlobalOutputWindow()->AddToolOutput("Document Modified:");
		if (modified)
		{
			g_pn->GetGlobalOutputWindow()->AddToolOutput("True");
		}
		else
		{
			g_pn->GetGlobalOutputWindow()->AddToolOutput("False");
		}
	}

	virtual void OnWriteProtectChanged(bool)
	{
	}

private:
	extensions::IDocumentPtr m_doc;
};

class AppEventSink : public extensions::IAppEventSink
{
public:
	virtual ~AppEventSink(){}

	/// Called when a new document is opened/created
	virtual void OnNewDocument(extensions::IDocumentPtr& doc)
	{
		extensions::IDocumentEventSinkPtr docEvents(new DocEventSink(doc));
		doc->AddEventSink(docEvents);
		
		// Only sign up to TextEventSink if you really need to handle these events,
		// you'll get called on every character press.
		
		extensions::ITextEditorEventSinkPtr textEvents(new TextEventSink());
		doc->AddEventSink(textEvents);
	}
	
	/// Called when PN is closing (you are about to be unloaded!)
	virtual void OnAppClose()
	{
	}
};

#endif