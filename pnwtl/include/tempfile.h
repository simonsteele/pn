/**
 * @file tempfile.h
 * @brief Temporary filenames.
 * @author Simon Steele
 * @note Copyright (c) 2004-2009 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef tempfile_h__included
#define tempfile_h__included

class TempFileName
{
	public:
		TempFileName(LPCTSTR existingFile = NULL, LPCTSTR extension = NULL, bool guid = false, bool useExistingPath = false)
		{
			if(guid)
			{
				guidFile(existingFile, extension, useExistingPath);
			}
			else
			{
				wintempFile(existingFile);
			}
		}

		void erase()
		{
			::DeleteFile(tempFile.c_str());
		}

		const char* c_str()
		{
#ifdef _UNICODE
			CT2CA conv(tempFile.c_str());
			tempFilea = conv;
			return tempFilea.c_str();
#else
			return tempFile;
#endif
		}

		const wchar_t* w_str()
		{
#ifdef _UNICODE
			return tempFile.c_str();
#else
			USES_CONVERSION;
			tempFilew = A2CW(tempFile.c_str());
			return tempFilew.c_str();
#endif
		}

		const LPCTSTR t_str()
		{
			return tempFile.c_str();
		}

	protected:

		void wintempFile(LPCTSTR existingFile)
		{
			/*
			UINT GetTempFileName(
				LPCTSTR lpPathName,
				LPCTSTR lpPrefixString,
				UINT uUnique,
				LPTSTR lpTempFileName
				);
			*/
			TCHAR fnBuf[MAX_PATH+1];
			
			if(existingFile != NULL)
			{
				CFileName fn(existingFile);
				tstring path = fn.GetPath();

				GetTempFileName(path.c_str(), _T("pn2"), 0, fnBuf);
				tempFile = fnBuf;
			}
			else
			{
				TCHAR tempPathBuf[MAX_PATH+1];
				::GetTempPath(MAX_PATH, tempPathBuf);

				GetTempFileName(tempPathBuf, _T("pn2"), 0, fnBuf);
				tempFile = fnBuf;
			}
		}

		void guidFile(LPCTSTR existingFile, LPCTSTR extension, bool useEPath)
		{
			TCHAR guidBuf[MAX_PATH+1];
			
			// Get a guid string.
			GUID guid;
			CoCreateGuid(&guid);
			GUIDToString(guid, guidBuf);

			if(existingFile != NULL)
			{
				// Get our path.
				CFileName fn(existingFile);
				
				if(useEPath)
					tempFile = fn.GetPath();
				else
				{
					TCHAR tempPathBuf[MAX_PATH+1];
					::GetTempPath(MAX_PATH, tempPathBuf);
					tempFile = tempPathBuf;
				}
				tempFile += guidBuf;
				tempFile += fn.GetExtension();
			}
			else
			{
				TCHAR tempPathBuf[MAX_PATH+1];
				::GetTempPath(MAX_PATH, tempPathBuf);
				
				tempFile = tempPathBuf;
				tempFile += guidBuf;
				tempFile += extension;
			}
		}

		void GUIDToString( GUID& guid, LPTSTR sResult )
		{
			wsprintf(sResult, _T("{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}"),
				guid.Data1, guid.Data2, guid.Data3,
				guid.Data4[0], guid.Data4[1], guid.Data4[2], guid.Data4[3],
				guid.Data4[4], guid.Data4[5], guid.Data4[6], guid.Data4[7]);
		}

	protected:
		tstring tempFile;

#ifdef _UNICODE
		std::string tempFilea;
#else
		std::wstring tempFilew;
#endif
};

#endif