
#ifndef __inifile_h__
#define __inifile_h__

#define CIniFile_Revision 1.1 // Fixed ValueExists to be case insensitive.

#pragma warning( disable : 4786) //disable the 255 character STL warning.

#include <string>
#include <vector>

#ifndef iniString
	#ifdef ctcString
		#define iniString ctcString
	#else
		#ifdef UNICODE
			#define iniString std::wstring
		#else
			#define iniString std::string
		#endif
	#endif
#endif

#define iniStringList std::vector<iniString>
#define slit iniStringList::iterator

class CIniFileIF
{
	private:
		CIniFileIF(){m_FileName = NULL;}
	protected:
		TCHAR *m_FileName;
	public:
		CIniFileIF(LPCTSTR filename);
		~CIniFileIF();
		
		bool SectionExists(LPCTSTR Section);
		bool ValueExists(LPCTSTR Section, LPCTSTR Ident);
		virtual iniString ReadString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Default) = 0;
		virtual void WriteString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Value) = 0;
		
		virtual long ReadInteger(LPCTSTR Section, LPCTSTR Ident, long Default=0);
		virtual void WriteInteger(LPCTSTR Section, LPCTSTR Ident, long Value);

		virtual bool ReadBool(LPCTSTR Section, LPCTSTR Ident, bool Default=false);
		virtual void WriteBool(LPCTSTR Section, LPCTSTR Ident, bool Value);

		virtual double ReadDouble(LPCTSTR Section, LPCTSTR Ident, double Default=0);
		virtual void WriteDouble(LPCTSTR Section, LPCTSTR Ident, double Value);
  
		virtual void DeleteKey(LPCTSTR Section, LPCTSTR Ident) = 0;
		virtual void EraseSection(LPCTSTR Section) = 0;
		virtual void UpdateFile() = 0;
     
		virtual void ReadSection(LPCTSTR Section, iniStringList& Strings) = 0;
		virtual void ReadSections(iniStringList& Strings) = 0;
		virtual void ReadSectionValues(LPCTSTR Section, iniStringList& Strings) = 0;
};

/**
 * @todo implement ReadSectionValues
 */
class CIniFile : public CIniFileIF
{
	public:
		CIniFile(LPCTSTR filename) : CIniFileIF(filename){}
		virtual iniString ReadString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Default);
		virtual void WriteString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Value);

		virtual std::string ReadStringA(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Default);

		virtual void ReadSection(LPCTSTR Section, iniStringList& Strings);
		virtual void ReadSections(iniStringList& Strings);
		virtual void ReadSectionValues(LPCTSTR Section, iniStringList& Strings);

		virtual void DeleteKey(LPCTSTR Section, LPCTSTR Ident);
		virtual void EraseSection(LPCTSTR Section);
		virtual void UpdateFile();
};

#endif