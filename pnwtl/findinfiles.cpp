/**
 * @file findinfiles.cpp
 * @brief Find In Files Implementation
 * @author Simon Steele
 * @note Copyright (c) 2006-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

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

FIFThread::FIFThread() : 
	m_pBM(new BoyerMoore())
{
}

void FIFThread::Find(LPCTSTR findstr, LPCTSTR path, LPCTSTR fileTypes, bool bRecurse, bool bCaseSensitive, bool bMatchWholeWord, bool bIncludeHidden, FIFSink* pSink)
{
	PNASSERT(findstr != NULL);
	PNASSERT(pSink != NULL);

	Stop();

	m_it.reset();

	CT2CA findstrconv(findstr);

	m_pBM->SetSearchString(findstrconv);
	m_pBM->SetCaseMode(bCaseSensitive);
	m_pBM->SetMatchWholeWord(bMatchWholeWord);
	m_pBM->SetIncludeHidden(bIncludeHidden);
	m_pSink = pSink;
	m_fileExts = fileTypes;
	m_path = CPathName(path).c_str();
	m_bRecurse = bRecurse;
	m_bIncludeHidden = bIncludeHidden;
	Start();
}

void FIFThread::Find(LPCTSTR findstr, FileItPtr& iterator, bool bCaseSensitive, bool bMatchWholeWord, FIFSink* pSink)
{
	PNASSERT(findstr != NULL);
	PNASSERT(pSink != NULL);

	Stop();

	m_it = iterator;

	CT2CA findstrconv(findstr);

	m_pBM->SetSearchString(findstrconv);
	m_pBM->SetCaseMode(bCaseSensitive);
	m_pBM->SetMatchWholeWord(bMatchWholeWord);
	m_pSink = pSink;
	Start();
}

FIFThread::~FIFThread()
{
	delete m_pBM;
}

void FIFThread::Run()
{
	// TODO: Change to true if using RegEx...
	if(m_pSink)
	{
		CA2CT findstr(m_pBM->GetSearchString());
		m_pSink->OnBeginSearch(findstr, false);
	}

	if (m_it.get())
	{
		m_nFiles = 0;
		m_nLines = 0;

		tstring file;
		while(m_it->Next(file) && GetCanRun())
		{
			searchFile(file.c_str());
		}
	}
	else
	{
		m_nFiles = 0;
		m_nLines = 0;
		
		FIFFinder finder(this, &FIFThread::OnFoundFile);
		finder.SetFilters(m_fileExts.c_str(), NULL, NULL, NULL);

		if (m_path.length() > 0)
		{
			finder.FindMatching(m_path.c_str(), m_bRecurse, m_bIncludeHidden);
		}
	}

	m_pSink->OnEndSearch(m_nLines, m_nFiles);
}

void FIFThread::OnException()
{
	LOG(_T("PN2: Exception whilst doing a find in files.\n"));
}

void FIFThread::OnFoundFile(LPCTSTR path, FileFinderData& findData, bool& /*shouldContinue*/)
{
	CFileName fn(findData.GetFilename());
	fn.Root(path);

	searchFile(fn.c_str());
}

void FIFThread::ManageStop()
{
	Stop();
}

#define FIFBUFFERSIZE 8192

/**
 * Do the actual searching in a file
 */
void FIFThread::searchFile(LPCTSTR filename)
{
	FILE* file = _tfopen(filename, _T("r"));
	if(file != NULL)
	{
		std::vector<char> buffer(FIFBUFFERSIZE);
		char* szBuf = &buffer[0];

		int nLine = 1;
		m_nFiles++;

		while (fgets(szBuf, FIFBUFFERSIZE, file))
		{
			int    nIdx;
			char *ptr(szBuf);
			size_t buflen(strlen(ptr));
			size_t remaininglen(buflen);
			bool haslineend(szBuf[buflen-1] == '\n' || szBuf[buflen-1] == '\r');
			bool bAnyFound(false);

			// Find all occurences in this buffer (usually a full line, but we will fail in extremely long lines
			// TODO: Deal with lines longer than 4096 bytes?
			while (remaininglen && ((nIdx = m_pBM->FindForward(ptr, static_cast<int>(remaininglen))) >= 0))
			{
				// Are we at the first found entry?
				if (!bAnyFound)
				{
					// Strip white spaces from the end of the input buffer.
					while (isspace(szBuf[buflen - 1]))
					{
						szBuf[buflen - 1] = NULL;
					}

					// Increase lines-found counter.
					m_nLines++;
				}

				// Found a least one.
				bAnyFound = true;

				// Convert the line for print and post it to the
				// main thread for the GUI stuff.
				foundString(filename, nLine, szBuf);
				
				// Increase search pointer so we can search the rest of the line.
				ptr += nIdx + 1;
				remaininglen = strlen(ptr);
			}

			// Increase line number.
			if (haslineend)
			{
				nLine++;
			}
		}

		fclose(file);
		Sleep(0);
	}
}

/**
 * Called when an instance of the string being searched for is found.
 */
void FIFThread::foundString(LPCTSTR szFilename, int line, LPCSTR buf)
{
	CA2CT searchconv(m_pBM->GetSearchString());
	CA2CT bufconv(buf);
	m_pSink->OnFoundString(searchconv, szFilename, line, bufconv);
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
						bool bMatchWholeWord,
						bool bIncludeHidden,
						FIFSink* pSink)
{
	m_thread.Find(findstr, path, fileTypes, bRecurse, bCaseSensitive, bMatchWholeWord, bIncludeHidden, pSink);
}

void FindInFiles::Start(LPCTSTR findstr, FileItPtr& iterator, bool bCaseSensitive, bool bMatchWholeWord, FIFSink* pSink)
{
	m_thread.Find(findstr, iterator, bCaseSensitive, bMatchWholeWord, pSink);
}

void FindInFiles::Stop()
{
	m_thread.ManageStop();
}