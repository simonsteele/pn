/**
 * @file sinks.h
 * @brief Defines miscellaneous sinks used to register for events
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef sinks_h__included
#define sinks_h__included

#ifdef _MSC_VER
	#pragma once
#endif

class DocSink : public extensions::IDocumentEventSink
{
public:
	DocSink(extensions::IDocumentPtr doc);
	virtual ~DocSink();

	virtual void OnDocClosing();
	virtual void OnCharAdded(char c);
	virtual void OnSchemeChange(const char *scheme){}

private:
	extensions::IDocumentPtr m_doc;
};

#endif //#ifndef sinks_h__included