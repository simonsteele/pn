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

class BoyerMoore;

class FIFThread : public CSSThread
{
	public:
		FIFThread();
		~FIFThread();

		void Find(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, FIFSink* pSink);

		void OnFoundFile(LPCTSTR path, LPCTSTR filename);

	protected:
		virtual void Run();
		virtual void OnException();

		void foundString(LPCTSTR szFilename, int line, LPCTSTR buf);

	protected:
		BoyerMoore* m_pBM;
		FIFSink*	m_pSink;
		int			m_nFiles;
		int			m_nLines;
		tstring		m_path;
		tstring		m_fileExts;
		bool		m_bRecurse;
};

class FindInFiles : public Singleton<FindInFiles, SINGLETON_AUTO_DELETE>
{
	friend class Singleton<FindInFiles, SINGLETON_AUTO_DELETE>;

public:
	~FindInFiles();
	bool IsRunning();
	void Start(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, FIFSink* pSink);
	void Stop();

protected:
	FIFThread	m_thread;
};

#endif // #ifndef findinfiles_h__included_8313F7A1_10AC_44dc_A29B_97395C2F76E9