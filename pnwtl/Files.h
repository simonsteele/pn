
#ifndef __filenames_h__
#define __filenames_h__

#include <string>

#ifndef cfnString
	#ifdef ctcString
		#define cfnString ctcString
	#else
		#ifdef UNICODE
			#define cfnString std::wstring
		#else
			#define cfnString std::string
		#endif
	#endif
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

		CFile();
		virtual ~CFile();
		
		bool Open(LPCTSTR filename, UINT flags = 0);
		int Read(void* lpBuf, UINT nCount);
		int Write(void* lpBuf, UINT nCount);
		void Close();

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

		bool ReadLine(CString& line);
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
private:
	cfnString	m_FileName;

	int GetLastSlashPos();
	int GetLastDotPos(cfnString* str=NULL);

public:
	CFileName() : m_FileName(""){}
	CFileName(const CFileName& copy){*this = copy;}
	CFileName(LPCTSTR filename){m_FileName = filename;}
	
	// Operators
	
	CFileName& operator = (LPCTSTR filename);			///< Set the filename to "filename".
	CFileName& operator = (const CFileName& filename);	///< Change to filename.filename.
	
	// Operations
	void ChangeExtensionTo(LPCTSTR newext);				///< Change the extension of the filename.
	void ChangePathTo(LPCTSTR newpath);					///< Change the path of the filename to newpath.
	
	cfnString GetExtension();
	void GetPath(cfnString& buf);						///< Return c:\temp\ of c:\temp\dat.dat
	void GetFileName_NoExt(cfnString& buf);				///< Return the filename part of c:\temp\filename.dat
	void GetFileName(cfnString& buf);					///< Return the filename.dat part of c:\temp\filename.dat

	/**
	 * GetFileAge returns the integer dos date of the
	 * file represented by this class. If the file does
	 * not exist, the function returns -1.
	 */
	int GetFileAge();									///< Get the age of the file.

	int	GetLength();
	const TCHAR* c_str();

};

int FileAge(LPCTSTR FileName);
bool CreateDirectoryRecursive(LPCTSTR pszDirectory, LPSECURITY_ATTRIBUTES lpSA = NULL);

#endif