#include "stdafx.h"
#include "include/boyermoore.h"
#include "include/filefinder.h"
#include "include/filematcher.h"

#include "findinfiles.h"

////////////////////////////////////////////////////////////////////////////////////
// FIFFinder
////////////////////////////////////////////////////////////////////////////////////

/**
 * This class imports all the good file finding and extension matching 
 * stuff in filematcher and filefinder and allows them to be cancelled
 * by the FIF thread.
 */
class FIFFinder : public RegExFileFinderImpl<FIFFinder, FIFThread>
{
public:
	typedef RegExFileFinderImpl<FIFFinder, FIFThread> baseClass;
	friend baseClass;

	FIFFinder(FIFThread* pOwner, OnFoundFunc func) : baseClass(pOwner, func){}

	bool shouldContinue()
	{
		PNASSERT(owner != NULL);
		/*LOG("CHECK\n");
		bool canRun = owner->GetCanRun();
		if(canRun)
			LOG("CAN\n");
		else
			LOG("EXIT EXIT EXIT EXIT EXIT EXIT EXIT EXIT\n");
		return canRun;*/
		return owner->GetCanRun();
	}
};

////////////////////////////////////////////////////////////////////////////////////
// FIFThread
////////////////////////////////////////////////////////////////////////////////////

FIFThread::FIFThread()
{
	m_pBM = new BoyerMoore();
}

void FIFThread::Find(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, bool bIncludeHidden, FIFSink* pSink)
{
	PNASSERT(findstr != NULL);
	PNASSERT(pSink != NULL);

	Stop();

	m_pBM->SetSearchString(findstr);
	m_pBM->SetCaseMode(bCaseSensitive);
	m_pBM->SetIncludeHidden(bIncludeHidden);
	m_pSink = pSink;
	m_fileExts = fileTypes;
	m_path = CPathName(path).c_str();
	m_bRecurse = bRecurse;
	m_bIncludeHidden = bIncludeHidden;
	Start();
}

FIFThread::~FIFThread()
{
	delete m_pBM;
}

void FIFThread::Run()
{
	FIFFinder finder(this, &FIFThread::OnFoundFile);
	finder.SetFilters(m_fileExts.c_str(), NULL, NULL, NULL);
	m_nFiles = 0;
	m_nLines = 0;
	
	// TODO: Change to true if using RegEx...
	if(m_pSink)
		m_pSink->OnBeginSearch(m_pBM->GetSearchString(), false);

	finder.FindMatching(m_path.c_str(), m_bRecurse, m_bIncludeHidden);

	m_pSink->OnEndSearch(m_nLines, m_nFiles);

	int a = 0;
}

void FIFThread::OnException()
{
	LOG(_T("PN2: Exception whilst doing a find in files.\n"));
}

#define FIFBUFFERSIZE 4096

void FIFThread::OnFoundFile(LPCTSTR path, FileFinderData& findData, bool& /*shouldContinue*/)
{
	CFileName fn(findData.GetFilename());
	fn.Root(path);

	FILE* file = _tfopen(fn.c_str(), _T("r"));
	if(file != NULL)
	{
		TCHAR szBuf[ FIFBUFFERSIZE ];

		int nLine = 0;
		m_nFiles++;

		while(_fgetts(szBuf, FIFBUFFERSIZE, file))
		{
			// Get the base pointer of the buffer.
			TCHAR *ptr = szBuf;
			int    nIdx;

			// Increase line number.
			// TODO: This is bad practice, as a line *could* be longer than 4096 chars.
			nLine++;

			// Find all occurences on this line.
			BOOL bAnyFound = FALSE;
			while (( _tcslen( ptr )) && (( nIdx = m_pBM->FindForward( ptr, ( int )_tcslen( ptr ))) >= 0 ))
			{
				// Are we at the first found entry?
				if ( bAnyFound == FALSE )
				{
					// Strip white spaces from the end of the input buffer.
					while ( _istspace( szBuf[ _tcslen( szBuf ) - 1 ] ))
						szBuf[ _tcslen( szBuf ) - 1 ] = 0;

					// Increase lines-found counter.
					m_nLines++;
				}

				// Found a least one.
				bAnyFound = TRUE;

				// Convert the line for print and post it to the
				// main thread for the GUI stuff.
				foundString(fn.c_str(), nLine, szBuf);
				
				// Increase search pointer so we can search the rest of the line.
				ptr += nIdx + 1;
			}
		}

		fclose(file);
		Sleep(0);
	}
}

void FIFThread::ManageStop()
{
	Stop();
}

/**
 * Called when an instance of the string being searched for is found.
 */
void FIFThread::foundString(LPCTSTR szFilename, int line, LPCTSTR buf)
{
	m_pSink->OnFoundString(m_pBM->GetSearchString(), szFilename, line, buf);
}

////////////////////////////////////////////////////////////////////////////////////
// FindInFiles
////////////////////////////////////////////////////////////////////////////////////

FindInFiles::~FindInFiles()
{
	m_thread.Stop();
}

bool FindInFiles::IsRunning()
{
	return !m_thread.GetStopped();
}

void FindInFiles::Start(
						LPCTSTR findstr, 
						LPCTSTR path, 
						LPCTSTR fileTypes, 
						bool bRecurse, 
						bool bCaseSensitive, 
						bool bIncludeHidden,
						FIFSink* pSink)
{
	m_thread.Find(findstr, path, fileTypes, bRecurse, bCaseSensitive, bIncludeHidden, pSink);
}

void FindInFiles::Stop()
{
	m_thread.ManageStop();
}