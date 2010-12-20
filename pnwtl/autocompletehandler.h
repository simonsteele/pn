/**
 * @file autocompletehandler.h
 * @brief Define autocomplete behaviours
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef autocompletehandler_h__included
#define autocompletehandler_h__included

#pragma once

class BaseAutoCompleteHandler
{
public:
	virtual ~BaseAutoCompleteHandler(){}
	virtual bool AutoCSelection(Scintilla::SCNotification* notification) = 0;
};

typedef boost::shared_ptr<BaseAutoCompleteHandler> AutoCompleteHandlerPtr;

template <class T>
class AutoCompleteAdaptor : public BaseAutoCompleteHandler
{
public:
	typedef bool (T::*TCallback)(Scintilla::SCNotification* notification);
	AutoCompleteAdaptor(T* pT, TCallback callback) : m_t(pT), m_cb(callback){}
	virtual ~AutoCompleteAdaptor(){}

	virtual bool AutoCSelection(Scintilla::SCNotification* notification)
	{
		return (m_t->*m_cb)(notification);
	}
private:
	TCallback m_cb;
	T* m_t;
};

#endif // autocompletehandler_h__included