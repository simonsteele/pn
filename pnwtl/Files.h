
#ifndef __filenames_h__
#define __filenames_h__

#include <string>

#ifndef tstring
	typedef std::basic_string<TCHAR> tstring;
#endif

#define CFILE_CouldNotSaveError _T("%s could not be saved in the specified location.\nThis could be due to an absent disk, a broken network connection, or a full disk.\nDo you want to save in another location?")

#define CFILE_CouldNotLoadError _T("%s could not be opened .\nThis could be because the file no longer exists, a disk is absent or due to a broken network connection.")

#define CFILE_SaveAccessDenied _T("%s could not be saved because access was denied.\nThis could be due to the file being marked read-only, or the disk being write-protected.\nDo you want to save in another location?")

#define CFILE_LoadAccessDenied _T("%s could not be opened because access was denied.\nThis could be because you do not have sufficient rights\nto open this file.")

#define CFILE_SaveDiskFullError _T("%s could not be saved because the disk is full.\nDo you want to save in another location?")

#define CFILE_SaveShareViolation _T("%s could not be saved because another program or user is\nusing the same file.\nDo you want to save in another location?")

#define CFILE_LoadShareViolation _T("%s could not be opened because another program or user is\nusing the same file.")

#define CFILE_NetSaveError _T("%s could not be saved because of a network error. This\ncould be because of a broken network\nconnection or because the network was too busy.\nDo you want to save in another location?")

#define CFILE_NetLoadError _T("%s could not be opened because of a network error. This\ncould be because of a broken network\nconnection or because the network was too busy.")

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

		int ShowError(LPCTSTR filename, bool bOpen = true);

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

/**
 * @Author Simon Steele
 * @Date 13.08.2001
 * @brief
 * CFileName is a class which represents a filename
 * and provides operations for working with one.
 *
 * This class will perform a number of operations on
 * a filename - it inherently understands the format
 * of a filename.
 */
class CFileName
{
public:
	CFileName() : m_FileName(_T("")){}
	CFileName(const CFileName& copy){*this = copy;}
	CFileName(LPCTSTR filename){m_FileName = filename;}
	CFileName(const tstring& copy){*this = copy;}
	
	// Operators
	
	CFileName& operator = (LPCTSTR filename);			///< Set the filename to "filename".
	CFileName& operator = (const tstring& filename);	///< Set the filename to "filename".
	CFileName& operator = (const CFileName& filename);	///< Change to filename.filename.
	
	// Operations
	void ChangeExtensionTo(LPCTSTR newext);				///< Change the extension of the filename.
	void ChangePathTo(LPCTSTR newpath);					///< Change the path of the filename to newpath.
	
	tstring GetExtension();
	tstring GetFileName();
	tstring GetFileName_NoExt();
	tstring GetPath();
	void GetPath(tstring& buf);							///< Return c:\temp\ of c:\temp\dat.dat
	void GetFileName_NoExt(tstring& buf);				///< Return the filename part of c:\temp\filename.dat
	void GetFileName(tstring& buf);						///< Return the filename.dat part of c:\temp\filename.dat
	tstring& Sanitise();								///< Fix up the filename to standard form.

	bool IsRelativePath();								///< Return true if it's a relative path.
	tstring GetRelativePath(LPCTSTR path);				///< Return the relative path string required to get to path.
	bool CanGetRelativePath(LPCTSTR path);				///< Return true if we can build a relative path to 'path'.
	void Root(LPCTSTR rootPath);						///< Root the relative filename in rootPath.

	bool IsSubElementOf(LPCTSTR path);					///< Return true if this file is below path in the file system.
	bool PathIsParentElementOf(LPCTSTR path);			///< Return true if the path is below us in the file system.
	

	/**
	 * GetFileAge returns the integer dos date of the
	 * file represented by this class. If the file does
	 * not exist, the function returns -1.
	 */
	int GetFileAge();									///< Get the age of the file.

	int	GetLength();
	LPCTSTR c_str();

	const tstring& ToLower();

	operator tstring() {return m_FileName;}

	static tstring GetCurrentDirectory();

protected:
	tstring	m_FileName;

	int GetLastSlashPos();
	int GetLastDotPos(tstring* str=NULL);
};

int FileAge(LPCTSTR FileName);
bool DirExists(LPCTSTR szDir);
bool FileExists(LPCTSTR FileName);
bool CreateDirectoryRecursive(LPCTSTR pszDirectory, LPSECURITY_ATTRIBUTES lpSA = NULL);

#endif