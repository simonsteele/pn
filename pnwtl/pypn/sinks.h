/**
 * @file sinks.h
 * @brief Defines miscellaneous sinks used to register for events
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef sinks_h__included
#define sinks_h__included

#ifdef _MSC_VER
	#pragma once
#endif

class DocSink : public extensions::IDocumentEventSink, public extensions::ITextEditorEventSink
{
public:
	DocSink(extensions::IDocumentPtr& doc);
	virtual ~DocSink();

	virtual void OnDocClosing();
	virtual void OnSchemeChange(const char *scheme){}
	virtual void OnAfterLoad();
	virtual void OnBeforeSave(const wchar_t* filename);
	virtual void OnAfterSave();
	virtual void OnModifiedChanged(bool modified);
	virtual void OnWriteProtectChanged(bool writeProtect);

	virtual void OnCharAdded(char c);

private:
	extensions::IDocumentPtr m_doc;
};

#endif //#ifndef sinks_h__included