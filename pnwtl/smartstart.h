/**
 * @file smartstart.h
 * @brief Definition of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef smartstart_h__included
#define smartstart_h__included

typedef std::map<tstring, tstring> STRING_MAP;
typedef STRING_MAP::value_type SM_VT;
typedef STRING_MAP::iterator SM_IT;

class CTextView;

class SmartStart : public Singleton<SmartStart, SINGLETON_AUTO_DELETE>, XMLParseState
{
public:
	friend class Singleton<SmartStart, SINGLETON_AUTO_DELETE>;
	typedef enum {eContinue, eMatched, eGiveUp} EContinueState;

	virtual ~SmartStart();

	EContinueState	OnChar(CTextView* pView);
	STRING_MAP&		GetMap();

	void			Save();

//XMLParseState
public:
	virtual void startElement(LPCTSTR name, XMLAttributes& atts);
	virtual void endElement(LPCTSTR name){}
	virtual void characterData(LPCTSTR data, int len){}

protected:
	SmartStart();

protected:
	CTextView*	m_pView;
	STRING_MAP	m_Map;
	size_t		m_max;
	TCHAR*		m_buffer;
};

#endif // #ifndef smartstart_h__included