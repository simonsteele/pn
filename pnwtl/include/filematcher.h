#ifndef filematcher_h__included
#define filematcher_h__included

#ifndef pcreplus_h__included
	#error filematcher.h requires pcreplus.h to be included first.
#endif

#ifndef _STRING_
	#error filematcher.h requires <string> to be included first.
#endif

namespace PCRE
{

class RegExFileMatcher
{
public:
	RegExFileMatcher(LPCTSTR filterstr)
	{
		sPatterns = "";
		re = NULL;

		build(filterstr);
	}

	~RegExFileMatcher()
	{
		if(re != NULL)
			delete re;
	}

	bool Match(LPCTSTR filename)
	{
		PNASSERT(Valid());
		return re->Match(filename);
	}

	bool Valid()
	{
		return re != NULL;
	}

	LPCSTR Error()
	{
		return error.c_str();
	}

protected:
	string sPatterns;
	RegExp* re;
	string error;

	void build(LPCTSTR filterstr)
	{
		TCHAR* dup = new TCHAR[_tcslen(filterstr)+1];
		_tcscpy(dup, filterstr);

		TCHAR* p = dup;
		while(*p)
		{
			TCHAR* pSemi = strchr(p, _T(';'));
			if(pSemi)
				*pSemi = NULL;

			string pattern = convertMask(p);
			if(sPatterns.length() > 0)
				sPatterns += '|';
			sPatterns += pattern;

			if(pSemi)
				p = pSemi + 1;
			else
				p = p + _tcslen(p);
		}

		delete [] dup;

		if(sPatterns.length() > 0)
		{
			try
			{
				re = new RegExp(sPatterns.c_str(), RegExp::CaseInsensitive | RegExp::UTF8);
			}
			catch(REException& ex)
			{
				error = ex.GetMessage();
				delete re;
				re = NULL;
			}
		}
	}

	string convertMask(LPCTSTR sIn)
	{
		string sOut = "(";

		size_t i = 0;
		while ( i < _tcslen(sIn) )
		{
			// Add most characters straight in...
			switch(sIn[i])
			{
				case '*':
					sOut += ".*";
					break;
				
				case '?':
					if(i == 0)
						sOut += "(^.)";
					else
						sOut += ".?";
					break;
				
				case '\\':
					if(i == 0)
						sOut += "(^[\\\\])";
					else
						sOut += _T("\\\\");
					break;
				
				default:
					if(i == 0)
					{
						sOut += "(^[";
						sOut += sIn[i];
						sOut += "])";
					}
					else
						sOut += sIn[i];
					break;
			}
			
			i++;
		}

		return sOut + "$)";
	}
};

} // namespace PCRE

#ifdef filefinder_h__included

// If we know about filefinder, then we can make a special one of those for
// matching...

template <class TOwner>
class RegExFileFinder : protected FileFinderImpl<RegExFileFinder, TOwner>
{
	typedef FileFinderImpl<RegExFileFinder, TOwner> baseClass;
	friend baseClass;

	public:
		RegExFileFinder(TOwner* pOwner, OnFoundFunc func) : baseClass(pOwner, func)
		{
			matcher = NULL;
			blocker = NULL;
			fMatcher = NULL;
			fBlocker = NULL;
		}

		~RegExFileFinder()
		{
			ClearFilters();
		}

		bool FindMatching(LPCTSTR path, bool bRecurse = false)
		{			
			// Get the baseclass to find everything, and we filter in shouldMatch...
			return baseClass::Find(path, _T("*.*"), bRecurse);
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

#endif

#endif // #ifndef filematcher_h__included