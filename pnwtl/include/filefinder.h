/**
 * @file filefinder.h
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef filefinder_h__included
#define filefinder_h__included

class FileFinderData
{
public:
	LPCTSTR GetFilename() const
	{
		return m_findData.cFileName;
	}

	uint64_t GetLastWriteTime() const
	{
		return *((uint64_t*)&m_findData.ftLastWriteTime);
	}

	WIN32_FIND_DATA m_findData;
};

/**
 * @brief Implementation class for a templated file finding class.
 */
template <class T, class TOwner, class TFindData>
class FileFinderImpl
{
public:
	typedef void (TOwner::*OnFoundFunc)(LPCTSTR path, TFindData& details, bool& shouldContinue);
	typedef void (TOwner::*OnDirChange)(LPCTSTR path);
	FileFinderImpl(TOwner* pOwner, OnFoundFunc func)
	{
		owner = pOwner;
		f = func;

		fDirChange = NULL;
		fFinishDir = NULL;
	}

	void setDirChangeCallback(OnDirChange func)
	{
		fDirChange = func;
	}

	void setFinishedDirCallback(OnDirChange func)
	{
		fFinishDir = func;
	}

	bool Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false, bool bIncludeHidden = true)
	{
		HANDLE hFind;
		BOOL found = true;
		bool shouldContinue = true;

		WIN32_FIND_DATA& FindFileData =  findData.m_findData;

		tstring sPattern(path);
		sPattern += filespec;

		hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
		if (hFind != INVALID_HANDLE_VALUE) 
		{
			T* pT = static_cast<T*>(this);

			while (found && pT->shouldContinue())
			{	
				//if we are not including hidden files/directories and this one is hidden skip checking
				if( !( !bIncludeHidden && ((FindFileData.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN) != 0) ) )
				{
					if( bRecurse && ((FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) )
					{
						if( pT->shouldRecurse(path, FindFileData.cFileName) )
						{
							tstring path2(path);
							path2 += FindFileData.cFileName;
							path2 += _T('\\');
							if(fDirChange)
								(owner->*fDirChange)(path2.c_str());
							Find(path2.c_str(), filespec, bRecurse, bIncludeHidden);
							if(fFinishDir)
								(owner->*fFinishDir)(path2.c_str());
						}
					}
					else
					{
						// Call owner class with found data...
						if( pT->shouldMatch(FindFileData.cFileName) )
						{
							(owner->*f)(path, findData, shouldContinue);

							if (!shouldContinue)
							{
								break;
							}
						}
					}
				}

				found = FindNextFile(hFind, &FindFileData);
			}

			FindClose(hFind);

			return shouldContinue;
		}

		return false;
	}

	TFindData& GetFindData()
	{
		return findData;
	}

protected:
	bool shouldRecurse(LPCTSTR /*path*/, LPCTSTR subfolder)
	{
		if(_tcscmp(subfolder, _T("..")) != 0 && _tcscmp(subfolder, _T(".")) != 0)
			return true;
		return false;
	}

	bool shouldMatch(LPCTSTR folder)
	{
		return true;
	}

	bool shouldContinue()
	{
		return true;
	}

protected:
	OnFoundFunc	f;
	OnDirChange fDirChange;
	OnDirChange fFinishDir;
	TOwner*		owner;
	TFindData   findData;
};

/**
 * @brief Basic implementation of the FileFinderImpl class.
 */
template <class TOwner, class TFindData = FileFinderData>
class FileFinder : public FileFinderImpl<FileFinder<TOwner, TFindData>, TOwner, TFindData>
{
	typedef FileFinderImpl<FileFinder<TOwner, TFindData>, TOwner, TFindData> baseClass;
public:
	FileFinder(TOwner* pOwner, OnFoundFunc func) : baseClass(pOwner, func){}
};

typedef void (DefaultFoundFunc)(LPCTSTR path, FileFinderData& details, bool& shouldContinue);

/**
 * Find and let a functor handle this.
 */
template <typename TFunctor>
class FileFinderFunctor : public FileFinderImpl<FileFinderFunctor<TFunctor>, FileFinderFunctor<TFunctor>, FileFinderData>
{
	typedef FileFinderImpl<FileFinderFunctor<TFunctor>, FileFinderFunctor<TFunctor>, FileFinderData> baseClass;
public:
	explicit FileFinderFunctor(TFunctor functor) : m_f(functor), baseClass(this, &FileFinderFunctor<TFunctor>::OnFoundFunc) {}

private:
	void OnFoundFunc(LPCTSTR path, FileFinderData& details, bool& shouldContinue)
	{
		m_f(path, details, shouldContinue);
	}

	TFunctor m_f;
};

/**
 * Simple file finder that returns a list of matching files.
 */
class FileFinderList : public FileFinderImpl<FileFinderList, FileFinderList, FileFinderData>
{
	typedef FileFinderImpl<FileFinderList, FileFinderList, FileFinderData> baseClass;
public:
	FileFinderList() : baseClass(this, &FileFinderList::OnFoundFunc)
	{
	}

	/// Get a list of matching files from path
	const std::list<tstring>& GetFiles(LPCTSTR path, LPCTSTR filespec, bool recurse)
	{
		m_matches.clear();

		Find(path, filespec, recurse, true);

		return m_matches;
	}

private:
	void OnFoundFunc(LPCTSTR path, FileFinderData& details, bool& shouldContinue)
	{
		m_matches.push_back(details.GetFilename());
	}

	void OnDirChange(LPCTSTR path)
	{
	}

	std::list<tstring> m_matches;
};

#endif //ifndef filefinder_h__included