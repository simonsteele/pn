/**
 * @file afiles.cpp
 * @brief Alternate Files Implementation
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "xmlparser.h"
#include <list>

#include "afiles.h"

#define AF_START	1
#define AF_SETS		2
#define AF_UNKNOWN	3

AlternateFiles* Singleton<AlternateFiles, SINGLETON_AUTO_DELETE>::s_pTheInstance = NULL;

AlternateFileSet::AlternateFileSet(LPCTSTR set1_, LPCTSTR set2_)
{
	set(set1, set1_);
	set(set2, set2_);
}

AlternateFileSet::AlternateFileSet(const AlternateFileSet& copy)
{
	tstring temp = _T("");
	copy.GetSet1String(temp);
	set(set1, temp.c_str());
	temp = _T("");
	copy.GetSet2String(temp);
	set(set2, temp.c_str());
}

AlternateFileSet::~AlternateFileSet()
{
	clear();
}

void AlternateFileSet::Set(LPCTSTR set1_, LPCTSTR set2_)
{
	set(set1, set1_);
	set(set2, set2_);
}

LPCTSTR AlternateFileSet::Set1() const
{
	return set1;
}

LPCTSTR AlternateFileSet::Set2() const
{
	return set2;
}

void AlternateFileSet::GetSet1String(tstring& str) const
{
	get(set1, str);
}

void AlternateFileSet::GetSet2String(tstring& str) const
{
	get(set2, str);
}

void AlternateFileSet::set(TCHAR*& pSet, LPCTSTR instr)
{
	pSet = new TCHAR[_tcslen(instr)+2];
	memset(pSet, 0, (_tcslen(instr)+2)*sizeof(TCHAR));

	TCHAR* p = pSet;
	_tcscpy(p, instr);
	
	p = _tcschr(p, _T(';'));

	while(p)
	{
		*p++ = NULL;
		if(*p)
			p = _tcschr(p, _T(';'));
	}
}

void AlternateFileSet::get(LPCTSTR setstr, tstring& outstr) const
{
	LPCTSTR p = setstr;
	while(*p)
	{
		outstr += p;
		
		p += _tcslen(p);
		p++;
		if(*p)
			outstr += _T(';');
	}
}

void AlternateFileSet::clear()
{
	delete [] set1;
	delete [] set2;
}

/**
 * Protected constructor.
 */
AlternateFiles::AlternateFiles()
{
	state = AF_START;

	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("AlternateFiles.xml");

	if(!FileExists(path.c_str()))
	{
		// Some simple defaults...
		sets.push_back( new AlternateFileSet(_T(".cxx;.c;.cpp;.cc"), _T(".h;.hpp;.hh")) );
		sets.push_back( new AlternateFileSet(_T(".dfm"), _T(".pas")) );
		sets.push_back( new AlternateFileSet(_T(".d"), _T(".di")) );
	}
	else
	{
		XMLParser parser;
		parser.SetParseState(this);

		try
		{
			parser.LoadFile(path.c_str());
		}
		catch (XMLParserException& ex)
		{
			tstring stat = _T("Error loading AlternateFiles.xml: ");
			stat += ex.GetMessage();
			g_Context.m_frame->SetStatusText(stat.c_str());
		}
	}
}

/**
 * Cleanup
 */
AlternateFiles::~AlternateFiles()
{
	Clear();
}

void AlternateFiles::Add(AlternateFileSet* pSet)
{
	sets.push_back(pSet);
}

void AlternateFiles::Remove(AlternateFileSet* pSet)
{
	sets.remove(pSet);
}

void AlternateFiles::Clear()
{
	AFILES_IT i;
	for(i = sets.begin(); i != sets.end(); ++i)
	{
		delete (*i);
	}
	sets.clear();
}

bool AlternateFiles::GetAlternate(LPCTSTR filename, tstring& afile) const
{
	CFileName fn(filename);
	tstring ext = fn.GetExtension();
	tstring newfn;
	tstring tryfn;
	tstring path;
	
	fn.GetFileName_NoExt(newfn);
	fn.GetPath(path);
	newfn = path + newfn;

	if(ext.length() < 2) //.x
		return 0;

	const TCHAR* altexts = NULL;

	AFILES_CIT i;
	for(i = sets.begin(); i != sets.end(); ++i)
	{
		altexts = getMatchingSet( (*i)->Set1(), (*i)->Set2(), ext.c_str() );
		if(altexts)
			break;
	}

	if(!altexts)
		return 0;

	while(*altexts)
	{
		// Quick sanity check to make sure we don't get the same file.
		if( _tcsicmp(ext.c_str(), altexts) != 0 )
		{
			tryfn = newfn;
			tryfn += altexts;

			if(FileExists(tryfn.c_str()))
			{
				afile = tryfn;
				return true;
			}
		}

		altexts += _tcslen(altexts);
		altexts++;
	}

	return false;
}

const AFILES_LIST& AlternateFiles::GetSets() const
{
	return sets;
}

void AlternateFiles::SetSets(const AFILES_LIST& sets_)
{
	sets = sets_;
}

void AlternateFiles::Save() const
{
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("AlternateFiles.xml");

	CFile f;
	if( f.Open(path.c_str(), CFile::modeWrite | CFile::modeBinary) )
	{
		f.Write(_T("<?xml version=\"1.0\"?>\r\n<AlternateFiles>\r\n"), 41 * sizeof(TCHAR));

		tstring sout;
		for(AFILES_CIT i = sets.begin(); i != sets.end(); ++i)
		{
			tstring temp1;
			tstring temp2;
			(*i)->GetSet1String(temp1);
			(*i)->GetSet2String(temp2);

			sout = _T("\t<Set ext1=\"");
			XMLSafeString(temp1.c_str(), sout);
			sout += _T("\" ext2=\"");
			XMLSafeString(temp2.c_str(), sout);
			sout += _T("\" />\r\n");
			f.Write((void*)sout.c_str(), sout.length() * sizeof(TCHAR));
		}

		f.Write(_T("</AlternateFiles>"), 17 * sizeof(TCHAR));
	}
}

bool AlternateFiles::extMatches(LPCTSTR pSet, LPCTSTR ext) const
{
	LPCTSTR p = pSet;
	while(*p)
	{
		if(_tcsicmp(p, ext) == 0)
		{
			return true;
		}
		
		p += _tcslen(p);
		p++;
	}

	return false;
}

LPCTSTR AlternateFiles::getMatchingSet(LPCTSTR pSet1, LPCTSTR pSet2, LPCTSTR ext) const
{
	if( extMatches(pSet1, ext) )
		return pSet2;
	else if( extMatches(pSet2, ext) )
		return pSet1;
	else
		return NULL;
}


#define MATCH(_state, _str) \
	(state == _state && (_tcscmp(_str, name) == 0))

/**
 * <AlternateFiles>
 *     <Set ext1=".cpp;.cxx" ext2=".h;.hpp" />
 * </AlternateFiles>
 */
void AlternateFiles::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if( MATCH(AF_START, _T("AlternateFiles")) )
	{
		state = AF_SETS;
	}
	else if( MATCH(AF_SETS, _T("Set")) )
	{
		LPCTSTR s1 = atts.getValue(_T("ext1"));
		LPCTSTR s2 = atts.getValue(_T("ext2"));
		if( s1 != NULL &&
			s2 != NULL &&
			(_tcslen(s1) > 0) &&
			(_tcslen(s2) > 0) )
		{
			AlternateFileSet* pSet = new AlternateFileSet(s1, s2);
			sets.push_back(pSet);
		}
	}
	else
		state = AF_UNKNOWN;
}