/**
 * @file filefinder.h
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef filefinder_h__included
#define filefinder_h__included

/**
 * @brief Implementation class for a templated file finding class.
 */
template <class T, class TOwner>
class FileFinderImpl
{
public:
	typedef void (TOwner::*OnFoundFunc)(LPCTSTR path, LPCTSTR filename);
	FileFinderImpl(TOwner* pOwner, OnFoundFunc func)
	{
		owner = pOwner;
		f = func;
	}

	bool Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false)
	{
		HANDLE hFind;
		WIN32_FIND_DATA FindFileData;
		BOOL found = true;

		tstring sPattern(path);
		sPattern += filespec;

		hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
		if (hFind != INVALID_HANDLE_VALUE) 
		{
			while (found)
			{
				if( bRecurse && ((FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) )
				{
					T* pT = static_cast<T*>(this);
					if( pT->shouldRecurse(path, FindFileData.cFileName) )
					{
						tstring path2(path);
						path2 += FindFileData.cFileName;
						Find(path2.c_str(), filespec, bRecurse);
					}
				}

				// Call owner class with found data...
				(owner->*f)(path, FindFileData.cFileName);

				found = FindNextFile(hFind, &FindFileData);
			}

			FindClose(hFind);

			return true;
		}

		return false;
	}

protected:
	bool shouldRecurse(LPCTSTR /*path*/, LPCTSTR /*subfolder*/)
	{
		return true;
	}

protected:
	OnFoundFunc	f;
	TOwner*			owner;
};

/**
 * @brief Basic implementation of the FileFinderImpl class.
 */
template <class TOwner>
class FileFinder : public FileFinderImpl<FileFinder, TOwner>
{
	typedef FileFinderImpl<FileFinder, TOwner> baseClass;
public:
	FileFinder(TOwner* pOwner, OnFoundFunc func) : baseClass(pOwner, func){}
};

#endif //ifndef filefinder_h__included