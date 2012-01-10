/**
 * @file filefinder.h
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef filefinder_h__included
#define filefinder_h__included

class IFileFinderData
{
public:
    ~IFileFinderData() {}
    virtual LPCTSTR GetFilename() const = 0;
    virtual uint64_t GetLastWriteTime() const = 0;
};

class IFileFinderResultHandler
{
public:
    ~IFileFinderResultHandler() {}
	virtual void OnFileFound(LPCTSTR path, IFileFinderData& details, bool& shouldContinue) = 0;
	virtual void OnDirChange(LPCTSTR path) = 0;
    virtual void OnDirFinished(LPCTSTR path) = 0;
    
    virtual bool shouldRecurse(LPCTSTR /*path*/, LPCTSTR subfolder) { return true; }
    virtual bool shouldMatch(LPCTSTR folder) { return true; }
    virtual bool shouldContinue() { return true; }
};

#if PLAT_WIN

class WinFileFinderData;

class WinFileFinderImpl
{
public:
    WinFileFinderImpl(IFileFinderResultHandler& resultHandler, WinFileFinderData& data);
    
    bool Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false, bool bIncludeHidden = true);
private:
    IFileFinderResultHandler& handler;
    WinFileFinderData& findData;
};

class WinFileFinderData : public IFileFinderData
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
    
    boost::shared_ptr<WinFileFinderImpl> GetImpl(IFileFinderResultHandler& handler)
    { 
        return boost::make_shared<WinFileFinderImpl>(handler, this);
    }
    
private:
    WinFileFinderImpl finderImpl;
};

typedef WinFileFinderData FileFinderData;

#else

class BoostFileFinderData;

class BoostFileFinderImpl
{
public:
    BoostFileFinderImpl(IFileFinderResultHandler& resultHandler, BoostFileFinderData& data);
    
    bool Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false, bool bIncludeHidden = true);
private:
    IFileFinderResultHandler& m_handler;
    BoostFileFinderData& m_data;
};

class BoostFileFinderData : public IFileFinderData
{
public:
    BoostFileFinderData() : m_lastWriteTime(0)
    {
    }
    
    virtual ~BoostFileFinderData() {}
    
    LPCTSTR GetFilename() const
    {
        return m_filename.c_str();
    }
    
    uint64_t GetLastWriteTime() const
    {
        return m_lastWriteTime;
    }
    
    boost::shared_ptr<BoostFileFinderImpl> GetImpl(IFileFinderResultHandler& handler)
    {
        return boost::shared_ptr<BoostFileFinderImpl>(new BoostFileFinderImpl(handler, *this));
    }
    
    void SetFilename(const std::string& filename)
    {
        m_filename = filename;
    }
    
    void SetLastWriteTime(uint64_t lastWriteTime)
    {
        m_lastWriteTime = lastWriteTime;
    }
    
private:
    std::string m_filename;
    uint64_t m_lastWriteTime;
};

// TODO: Work out why we can't use a typedef here with clang...
#define FileFinderData BoostFileFinderData

#endif

/**
 * @brief Implementation class for a templated file finding class.
 */
template <class T, typename TOwner, class TFindData>
class FileFinderImpl : public IFileFinderResultHandler
{
public:
	typedef void (TOwner::*OnFoundFunc)(LPCTSTR path, TFindData& details, bool& shouldContinue);
	typedef void (TOwner::*OnDirChangeFunc)(LPCTSTR path);
	FileFinderImpl(TOwner* pOwner, OnFoundFunc func)
	{
		owner = pOwner;
		f = func;

		fDirChange = NULL;
		fFinishDir = NULL;
	}

	void setDirChangeCallback(OnDirChangeFunc func)
	{
		fDirChange = func;
	}

	void setFinishedDirCallback(OnDirChangeFunc func)
	{
		fFinishDir = func;
	}
	
	TFindData& GetFindData()
	{
		return findData;
	}

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
    
    bool Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false, bool bIncludeHidden = true)
    {
        return findData.GetImpl(*this)->Find(path, filespec, bRecurse, bIncludeHidden);
    }
    
    virtual void OnFileFound(LPCTSTR path, IFileFinderData& details, bool& shouldContinue) { (owner->*f)(path, findData, shouldContinue); }
	virtual void OnDirChange(LPCTSTR path) { if (fDirChange) { (owner->*fDirChange)(path); }}
    virtual void OnDirFinished(LPCTSTR path) { if (fFinishDir) { (owner->*fFinishDir)(path); }}

protected:
	OnFoundFunc	f;
	OnDirChangeFunc fDirChange;
	OnDirChangeFunc fFinishDir;
	TOwner*		owner;
	TFindData   findData;
};

/**
 * @brief Basic implementation of the FileFinderImpl class.
 */
template <typename TOwner, typename TFindData = FileFinderData>
class FileFinder : public FileFinderImpl<FileFinder<TOwner, TFindData>, TOwner, TFindData>
{
	typedef FileFinderImpl<FileFinder<TOwner, TFindData>, TOwner, TFindData> baseClass;
public:
	FileFinder(TOwner* pOwner, typename baseClass::OnFoundFunc func) : baseClass(pOwner, func){}
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