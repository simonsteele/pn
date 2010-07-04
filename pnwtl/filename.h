#ifndef filename_h__included
#define filename_h__included

/**
 * @brief represent a filename and provide operations for working with one.
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
	void AddExtension(LPCTSTR newext);					///< Add a new extension to the filename, even if there's one already.
	void ChangeExtensionTo(LPCTSTR newext);				///< Change the extension of the filename.
	void ChangePathTo(LPCTSTR newpath);					///< Change the path of the filename to newpath.
	
	tstring GetExtension();
	tstring GetFileName();
	tstring GetFileName_NoExt();
	tstring GetPath();
	tstring GetDirectoryName();							///< Return temp of c:\a\b\temp\d.e
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
	
	void SetForwardSlashes();							///< Switch to forward slashes...
	
	/**
	 * GetFileAge returns the FILETIME 64-bit date of the
	 * file represented by this class. If the file does
	 * not exist, the function returns ~0.
	 */
	uint64_t GetFileAge();									///< Get the age of the file.

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

class CPathName : public CFileName
{
public:
	CPathName(LPCTSTR path);
	CPathName(const tstring& copy){*this = copy;}

	CPathName& operator = (const tstring& filename);	///< Set the filename to "filename".

	void ChangeLastElement(LPCTSTR lastEl);
};

#endif // #ifndef filename_h__included