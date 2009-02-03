/**
 * @file files.h
 * @brief File access wrappers
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef __filenames_h__
#define __filenames_h__

#include <string>

#ifndef tstring
	typedef std::basic_string<TCHAR> tstring;
#endif

#define CFILE_CouldNotSaveError _T("%s could not be saved in the specified location.\nThis could be due to an absent disk, a broken network connection, or a full disk.\nDo you want to save in another location?")

#define CFILE_CouldNotLoadError _T("%s could not be opened.\nThis could be because the file no longer exists, a disk is absent or due to a broken network connection.")

#define CFILE_SaveAccessDenied _T("%s could not be saved because access was denied.\nThis could be due to the file being marked read-only, or the disk being write-protected.\nDo you want to save in another location?")

#define CFILE_LoadAccessDenied _T("%s could not be opened because access was denied.\nThis could be because you do not have sufficient rights\nto open this file.")

#define CFILE_SaveDiskFullError _T("%s could not be saved because the disk is full.\nDo you want to save in another location?")

#define CFILE_SaveShareViolation _T("%s could not be saved because another program or user is using the same file.\nDo you want to save in another location?")

#define CFILE_LoadShareViolation _T("%s could not be opened because another program or user is using the same file.")

#define CFILE_NetSaveError _T("%s could not be saved because of a network error. This could be because of a broken network connection or because the network was too busy.\nDo you want to save in another location?")

#define CFILE_NetLoadError _T("%s could not be opened because of a network error. This could be because of a broken network connection or because the network was too busy.")

/**
 * @Author Simon Steele
 * @Date 25.03.2002
 * @brief CFile represents a file - basically it pretends to be MFCs CFile
 */
class CFile
{
	public:

		enum OpenFlags {
			modeRead			= 0x0000,
			modeBinary			= 0x0000,
			modeWrite			= 0x0001,
			modeReadWrite		= 0x0002,
			modeText			= 0x0008
		};

		enum EFrom {
			current		= SEEK_CUR, // Current position of file pointer. 
			end			= SEEK_END, // End of file. 
			begin		= SEEK_SET	// Beginning of file.
		};

		CFile();
		virtual ~CFile();
		
		bool Open(LPCTSTR filename, UINT flags = 0);
		int Read(void* lpBuf, UINT nCount);
		int Write(void* lpBuf, UINT nCount);
		void Close();
		void Seek(UINT offset, EFrom from = begin);

		int ShowError(LPCTSTR filename, LPCTSTR app, bool bOpen = true);

		long GetPosition() const;
		long GetLength();

	protected:
		FILE* m_file;
};

class CTextFile : public CFile
{
	public:
		CTextFile() : CFile() {}
		virtual ~CTextFile() {}

#ifndef PN_NO_CSTRING
		bool ReadLine(CString& line);
#endif
		bool WriteLine(LPCTSTR line);
};

uint64_t FileAge(LPCTSTR FileName);
bool DirExists(LPCTSTR szDir);
bool IsDirectory(LPCTSTR szDir);
bool FileExists(LPCTSTR FileName);
bool CreateDirectoryRecursive(LPCTSTR pszDirectory, LPSECURITY_ATTRIBUTES lpSA = NULL);
bool DeleteDirectory(LPCTSTR szDir, bool undoable = true);

#endif