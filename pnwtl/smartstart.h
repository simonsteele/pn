/**
 * @file smartstart.h
 * @brief Definition of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2012 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef smartstart_h__included
#define smartstart_h__included

class CScintillaImpl;

class SmartStart : public Singleton<SmartStart, SINGLETON_AUTO_DELETE>, XMLParseState
{
public:
	friend class Singleton<SmartStart, SINGLETON_AUTO_DELETE>;
	typedef enum {eContinue, eMatched, eGiveUp} EContinueState;

	virtual ~SmartStart();

	/// Use this function for character-by-character smartstart matching.
	EContinueState	OnChar(CScintillaImpl* pView);
	
	/// Use this function to scan the first m_max chars for smartstart matches.
	void			Scan(CScintillaImpl* pView);
	
	string_map&		GetMap();

	void			Save();

//XMLParseState
public:
	virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
	virtual void endElement(LPCTSTR name){}
	virtual void characterData(LPCTSTR data, int len){}

private:
	SmartStart();

	void applyScheme(CScintillaImpl* pView, Scheme* pScheme);
	void update();

	CScintillaImpl*	m_pView;
	string_map	m_Map;
	size_t		m_max;
	char*		m_buffer;
};

#endif // #ifndef smartstart_h__included