
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

/**
 * @Author Simon Steele
 * @Date 25.03.2002
 * @brief CFile represents a file - basically it pretends to be MFCs CFile
 */
class CFile
{
	public:
		CFile();
		~CFile();
		
		bool Open(LPCTSTR filename, UINT flags);
		int Read(void* lpBuf, UINT nCount);
		void Close();

		long GetPosition() const;
		long GetLength();

	protected:
		FILE* m_file;
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

#endif