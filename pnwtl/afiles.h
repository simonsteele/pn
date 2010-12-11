/**
 * @file afiles.h
 * @brief Alternate Files definition
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef afiles_h__included
#define afiles_h__included

/**
 * This class contains a set of file extensions to map to another set.
 */
class AlternateFileSet
{
public:
	explicit AlternateFileSet(LPCTSTR set1, LPCTSTR set2);
	explicit AlternateFileSet(const AlternateFileSet& copy);
	~AlternateFileSet();

	void Set(LPCTSTR set1, LPCTSTR set2);

	LPCTSTR Set1() const;
	LPCTSTR Set2() const;

	void GetSet1String(tstring& str) const;
	void GetSet2String(tstring& str) const;

private:
	void set(TCHAR*&, LPCTSTR);
	void get(LPCTSTR, tstring&) const;

	void clear();

	TCHAR* set1;
	TCHAR* set2;
};

typedef std::list<AlternateFileSet*>	AFILES_LIST;
typedef AFILES_LIST::iterator			AFILES_IT;
typedef AFILES_LIST::const_iterator		AFILES_CIT;

class AlternateFiles : public Singleton<AlternateFiles, SINGLETON_AUTO_DELETE>, XMLParseState
{
public:
	friend class Singleton<AlternateFiles, SINGLETON_AUTO_DELETE>;

	virtual ~AlternateFiles();

	void Add(AlternateFileSet* pSet);
	void Remove(AlternateFileSet* pSet);
	void Clear();

	bool GetAlternate(LPCTSTR filename, tstring& afile) const;

	const AFILES_LIST& GetSets() const;
	void SetSets(const AFILES_LIST&);

	void Save() const;

//XMLParseState
public:
	virtual void startElement(XML_CSTR name, const XMLAttributes& atts);
	virtual void endElement(XML_CSTR name){}
	virtual void characterData(XML_CSTR data, int len){}

protected:
	AlternateFiles();

	bool extMatches(LPCTSTR pSet, LPCTSTR ext) const;
	LPCTSTR getMatchingSet(LPCTSTR pSet1, LPCTSTR pSet2, LPCTSTR ext) const;

protected:
	AFILES_LIST	sets;
	int			state;
};

#endif