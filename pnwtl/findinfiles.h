/**
 * @file findinfiles.h
 * @brief Find In Files Implementation
 * @author Simon Steele
 * @note Copyright (c) 2006-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef findinfiles_h__included_8313F7A1_10AC_44dc_A29B_97395C2F76E9
#define findinfiles_h__included_8313F7A1_10AC_44dc_A29B_97395C2F76E9

#include "include/ssthreads.h"

class FIFSink
{
	public:
		virtual void OnBeginSearch(LPCTSTR stringLookingFor, bool bIsRegex) = 0;
		virtual void OnFoundString(LPCTSTR stringFound, LPCTSTR szFilename, int line, LPCTSTR buf) = 0;
		virtual void OnEndSearch(int nFound, int nFiles) = 0;
};

class FileIterator
{
public:
	virtual ~FileIterator(){}
	virtual bool Next(tstring& file) = 0;
};

template <class TIter>
class IteratorWrapper : FileIterator
{
public:
	IteratorWrapper(TIter begin, TIter end) : m_i(iter), m_end(end)
	{}

	virtual ~IteratorWrapper() {}

	virtual bool Next(tstring& file)
	{
		if (m_i == m_end)
		{
			return false;
		}

		file = (*m_i);
		m_i++;
		return true;
	}

private:
	TIter m_i;
	TIter m_end;
};

class StringListIterator : public FileIterator
{
public:
	StringListIterator() : m_started(false){}
	virtual ~StringListIterator() {}

	virtual bool Next(tstring& file)
	{
		if (!m_started)
		{
			m_i = m_list.begin();
			m_started = true;
		}

		if (m_i == m_list.end())
		{
			return false;
		}

		file = (*m_i);
		m_i++;
		return true;
	}

	std::vector<tstring>& GetList()
	{
		return m_list;
	}

private:
	std::vector<tstring> m_list;
	std::vector<tstring>::const_iterator m_i;
	bool m_started;
};

typedef boost::shared_ptr<FileIterator> FileItPtr;

class BoyerMoore;
class FileFinderData;

class FIFThread : public CSSThread
{
	public:
		FIFThread();
		virtual ~FIFThread();

		void Find(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, bool bMatchWholeWord, bool bIncludeHidden, FIFSink* pSink);
		void Find(LPCTSTR findstr, FileItPtr& iterator, bool bCaseSensitive, bool bMatchWholeWord, FIFSink* pSink);

		void OnFoundFile(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/);

		void ManageStop();

	private:
		virtual void Run();
		virtual void OnException();

		void foundString(LPCTSTR szFilename, int line, LPCSTR buf);
		void searchFile(LPCTSTR filename);

		FileItPtr	m_it;
		BoyerMoore* m_pBM;
		FIFSink*	m_pSink;
		int			m_nFiles;
		int			m_nLines;
		tstring		m_path;
		tstring		m_fileExts;
		bool		m_bRecurse;
		bool		m_bIncludeHidden;
};

class FindInFiles : public Singleton<FindInFiles, SINGLETON_AUTO_DELETE>
{
	friend class Singleton<FindInFiles, SINGLETON_AUTO_DELETE>;

public:
	~FindInFiles();
	bool IsRunning();
	void Start(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, bool bMatchWholeWord, bool bIncludeHidden, FIFSink* pSink);
	void Start(LPCTSTR findstr, FileItPtr& iterator, bool bCaseSensitive, bool bMatchWholeWord, FIFSink* pSink);
	void Stop();

	template <class TIter>
	void Start(LPCTSTR findstr, TIter begin, TIter end, bool bCaseSensitive, FIFSink* pSink)
	{
		FileItPtr ptr(new IteratorWrapper(begin, end));
		Start(findstr, ptr, bCaseSensitive, pSink);
	}

protected:
	FIFThread	m_thread;
};

#endif // #ifndef findinfiles_h__included_8313F7A1_10AC_44dc_A29B_97395C2F76E9