/**
 * @file jumpto.h
 * @brief Jump to method stuff...
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef jumpto_h__included
#define jumpto_h__included

#include "jumptointerface.h"
#include <map>

class CChildFrame;
class COutputView;

typedef extensions::ITagSink ITagSink;
typedef extensions::LPMETHODINFO LPMETHODINFO;

typedef std::map<std::string, extensions::ITagSource*> HANDLERS_MAP;

class JumpToHandler : public Singleton<JumpToHandler, true>
{
	friend class Singleton<JumpToHandler, true>;

	public:
		void FindTags(CChildFrame* pChildFrame, ITagSink* pNotifySink);
		
		void AddSource(extensions::ITagSource* source);

	protected:
		JumpToHandler();
		~JumpToHandler();

		HANDLERS_MAP handlers;
};

#endif