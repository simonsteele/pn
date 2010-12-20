/**
 * @file ifilesource.h
 * @brief Interfaces for file handling.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Eventual intent is for this to allow files to be provided by file system,
 * ftp, sftp.
 */

#ifndef IFILESOURCE_H__INCLUDED
#define IFILESOURCE_H__INCLUDED

class IFile;
typedef boost::shared_ptr<IFile> IFilePtr;

/**
 * Exception type thrown when trying to open files for read/write.
 * Contains the last error value.
 */
class FileSourceException : public std::exception
{
public:
	FileSourceException(int error) : m_error(error)
	{
	}

	virtual ~FileSourceException() {}

	/**
	 * Get the error code indicating the error performing I/O.
	 */
	int GetError() const { return m_error; }

private:
	int m_error;
};

/**
 * Interface for file sources.
 */
class IFileSource
{
public:
	virtual ~IFileSource() {}

	virtual IFilePtr OpenWrite(const wchar_t* filename) = 0;
	virtual IFilePtr OpenRead(const wchar_t* filename) = 0;
};

/**
 * Interface for an existing file.
 */
class IFile
{
public:
	virtual ~IFile() {}
	virtual size_t Write(const void* buffer, int count) = 0;
	virtual size_t Read(void* buffer, int count) = 0;
	virtual void Close() = 0;

	virtual std::wstring GetFilename() const = 0;
};

#endif // #ifndef IFILESOURCE_H__INCLUDED