/**
 * @file filematcher.h
 * @brief Use regular expressions to match file names
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */


#ifndef filematcher_h__included
#define filematcher_h__included

//#ifndef pcreplus_h__included
//	#error filematcher.h requires pcreplus.h to be included first.
//#endif

#ifndef _STRING_
	#error filematcher.h requires <string> to be included first.
#endif

namespace PCRE
{

class RegExFileMatcher
{
public:
	RegExFileMatcher(LPCTSTR filterstr) : valid(false), sPatterns(_T(""))
	{
		build(filterstr);
	}

	bool Match(LPCTSTR filename) const
	{
		PNASSERT(Valid());
		return boost::xpressive::regex_match(tstring(filename), re);
	}

	bool Valid() const
	{
		return valid;
	}

	LPCTSTR Error() const
	{
		return error.c_str();
	}

private:
	tstring sPatterns;
	boost::xpressive::wsregex re;
	tstring error;
	bool valid;

	void build(LPCTSTR filterstr)
	{
		tstring s(filterstr);
		Trim(s);
		std::vector<tstring> toks;
		StringTokenise(s, toks, tstring(_T(";, ")));

		for(std::vector<tstring>::const_iterator i = toks.begin();
			i != toks.end();
			++i)
		{
			tstring query = (*i);
			Trim(query);

			tstring pattern = convertMask(query.c_str());
			if(sPatterns.length() > 0)
				sPatterns += _T('|');
			sPatterns += pattern;
		}

		if(sPatterns.length() > 0)
		{
			try
			{
				re = boost::xpressive::wsregex::compile(sPatterns, boost::xpressive::regex_constants::icase);
				valid = true;
			}
			catch(boost::xpressive::regex_error& ex)
			{
				CA2CT what(ex.what());
				error = what;
				valid = false;
			}
		}
	}

	tstring convertMask(LPCTSTR sIn)
	{
		tstring sOut = _T("(");

		size_t i = 0;
		while ( i < _tcslen(sIn) )
		{
			// Add most characters straight in...
			switch(sIn[i])
			{
				case _T('*'):
					sOut += _T(".*");
					break;
				
				case _T('?'):
					if(i == 0)
						sOut += _T("(^.)");
					else
						sOut += _T(".?");
					break;
				
				case _T('\\'):
					if(i == 0)
						sOut += _T("(^[\\\\])");
					else
						sOut += _T("\\\\");
					break;

				case _T('.'):
					sOut += _T("\\.");
					break;

				case _T(' '):
				case _T('\t'):
					break;
				
				default:
					if(i == 0)
					{
						sOut += _T("(^[");
						sOut += sIn[i];
						sOut += _T("])");
					}
					else
						sOut += sIn[i];
					break;
			}
			
			i++;
		}

		return sOut + _T("$)");
	}
};

} // namespace PCRE

#ifdef filefinder_h__included

// If we know about filefinder, then we can make a special one of those for
// matching...

template <class T, class TOwner>
class RegExFileFinderImpl : protected FileFinderImpl<T, TOwner, FileFinderData>
{
	typedef FileFinderImpl<T, TOwner, FileFinderData> baseClass;
	friend baseClass;

	public:
		RegExFileFinderImpl(TOwner* pOwner, OnFoundFunc func) : baseClass(pOwner, func)
		{
			matcher = NULL;
			blocker = NULL;
			fMatcher = NULL;
			fBlocker = NULL;
		}

		~RegExFileFinderImpl()
		{
			ClearFilters();
		}

		bool FindMatching(LPCTSTR path, bool bRecurse = false, bool bIncludeHidden = true)
		{			
			// Get the baseclass to find everything, and we filter in shouldMatch...
			return baseClass::Find(path, _T("*.*"), bRecurse, bIncludeHidden);
		}

#define SETFILTER(filter, theMatcher) \
	if(filter != NULL && _tcslen(filter) > 0) \
		{ \
		theMatcher = new PCRE::RegExFileMatcher(filter); \
		if(!theMatcher->Valid()) \
			return false; \
		}
#define CLEARFILTER(x) if(x != NULL){delete x; x = NULL;}

		bool SetFilters(LPCTSTR matchFiles, LPCTSTR excludeFiles, LPCTSTR matchFolders, LPCTSTR excludeFolders)
		{
			SETFILTER(matchFiles, matcher);
			SETFILTER(excludeFiles, blocker);
			SETFILTER(matchFolders, fMatcher);
			SETFILTER(excludeFolders, fBlocker);
			return true;
		}

		void ClearFilters()
		{
			CLEARFILTER(matcher);
			CLEARFILTER(blocker);
			CLEARFILTER(fMatcher);
			CLEARFILTER(fBlocker);
		}

#undef SETFILTER
#undef CLEARFILTER

	protected:
		bool shouldMatch(LPCTSTR file)
		{
			return (matcher == NULL || matcher->Match(file))
				&& (blocker == NULL || !blocker->Match(file));
		}

		bool shouldRecurse(LPCTSTR path, LPCTSTR subfolder)
		{
			return baseClass::shouldRecurse(path, subfolder) &&
				(fMatcher == NULL || fMatcher->Match(subfolder)) &&
				(fBlocker == NULL || !fBlocker->Match(subfolder));
		}

	protected:
		PCRE::RegExFileMatcher* matcher;
		PCRE::RegExFileMatcher* blocker;

		PCRE::RegExFileMatcher* fMatcher;
		PCRE::RegExFileMatcher* fBlocker;
};

template <class TOwner>
class RegExFileFinder : public RegExFileFinderImpl<RegExFileFinder<TOwner>, TOwner>
{
	typedef RegExFileFinderImpl<RegExFileFinder, TOwner> baseClass;
public:
	RegExFileFinder(TOwner* pOwner, OnFoundFunc func) : baseClass(pOwner, func){}
};

#endif

#endif // #ifndef filematcher_h__included