#ifndef smartstart_h__included
#define smartstart_h__included

typedef std::map<tstring, tstring> STRING_MAP;
typedef STRING_MAP::value_type SM_VT;
typedef STRING_MAP::iterator SM_IT;

class SmartStart : public Singleton<SmartStart, SINGLETON_AUTO_DELETE>, XMLParseState
{
public:
	friend class Singleton;
	typedef enum {eContinue, eMatched, eGiveUp} EContinueState;

	virtual ~SmartStart();

	EContinueState OnChar(CTextView* pView);

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