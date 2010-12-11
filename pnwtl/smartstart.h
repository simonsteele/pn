/**
 * @file smartstart.h
 * @brief Definition of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef smartstart_h__included
#define smartstart_h__included

class CTextView;

class SmartStart : public Singleton<SmartStart, SINGLETON_AUTO_DELETE>, XMLParseState
{
public:
	friend class Singleton<SmartStart, SINGLETON_AUTO_DELETE>;
	typedef enum {eContinue, eMatched, eGiveUp} EContinueState;

	virtual ~SmartStart();

	/// Use this function for character-by-character smartstart matching.
	EContinueState	OnChar(CTextView* pView);
	
	/// Use this function to scan the first m_max chars for smartstart matches.
	void			Scan(CTextView* pView);
	
	string_map&		GetMap();

	void			Save();

//XMLParseState
public:
	virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
	virtual void endElement(LPCTSTR name){}
	virtual void characterData(LPCTSTR data, int len){}

protected:
	SmartStart();

	void applyScheme(CTextView* pView, Scheme* pScheme);
	void update();

protected:
	CTextView*	m_pView;
	string_map	m_Map;
	size_t		m_max;
	char*		m_buffer;
};

#endif // #ifndef smartstart_h__included