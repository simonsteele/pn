#include "stdafx.h"
#include "inifile.h"

CIniFileIF::CIniFileIF(LPCTSTR filename)
{
	if(!filename)
		throw "Filename must be a real filename";

	m_FileName = new TCHAR[_tcslen(filename)+1];
	_tcscpy(m_FileName, filename);
}

CIniFileIF::~CIniFileIF()
{
	if(m_FileName != NULL)
		delete [] m_FileName;
}

bool CIniFileIF::SectionExists(LPCTSTR Section)
{
	iniStringList S;
	ReadSection(Section, S);
	return (S.size() > 0);
}

bool CIniFileIF::ValueExists(LPCTSTR Section, LPCTSTR Ident)
{
	iniStringList S;
	
	ReadSection(Section, S);
	for(slit i = S.begin(); i != S.end(); ++i)
	{
		// *i should be an iniString
		if (_tcsicmp(Ident, (*i).c_str()) == 0)
			return true;
	}
	return false;
}

/**
 * @todo Error Checking
 * @todo Check hex values (0x01)
 */
long CIniFileIF::ReadInteger(LPCTSTR Section, LPCTSTR Ident, long Default)
{
	iniString val;
	val = ReadString(Section, Ident, _T(""));
	if(val == "")
	{
		return Default;
	}
	
	return _ttol(val.c_str());
}

void CIniFileIF::WriteInteger(LPCTSTR Section, LPCTSTR Ident, long Value)
{
	TCHAR buf[34];
	_ltot(Value, &buf[0], 10);
	WriteString(Section, Ident, &buf[0]);
}

bool CIniFileIF::ReadBool(LPCTSTR Section, LPCTSTR Ident, bool Default)
{
	return ReadInteger(Section, Ident, (int)Default) != 0;
}

void CIniFileIF::WriteBool(LPCTSTR Section, LPCTSTR Ident, bool Value)
{
	TCHAR *boolstr[2] = {_T("0"), _T("1")};
	WriteString(Section, Ident, boolstr[(int)Value]);
}

/**
 * @todo Error Checking
 * @todo Check hex values (0x01)
 */
double CIniFileIF::ReadDouble(LPCTSTR Section, LPCTSTR Ident, double Default)
{
	iniString val;
	
	val = ReadString(Section, Ident, _T(""));
	if(val == "")
	{
		return Default;
	}

	TCHAR *endstr;
	
	return _tcstod(val.c_str(), &endstr);
}

void CIniFileIF::WriteDouble(LPCTSTR Section, LPCTSTR Ident, double Value)
{
	TCHAR buf[100];
	_stprintf(&buf[0], _T("%f"), Value);
	WriteString(Section, Ident, &buf[0]);
}

///////////////////////////////////////////////////////////////////////////////////////

iniString CIniFile::ReadString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Default)
{
	TCHAR buf[2049];
	GetPrivateProfileString(Section, Ident, Default, &buf[0], 2048, m_FileName);
	return (iniString(&buf[0]));
}

std::string CIniFile::ReadStringA(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Default)
{
	TCHAR buf[2049];
	GetPrivateProfileString(Section, Ident, Default, &buf[0], 2048, m_FileName);
#ifdef UNICODE
	char buf2[2049];
	wcstombs(&buf2[0], &buf[0], 2049);
	return std::string(&buf2[0]);
#else
	return (iniString(&buf[0]));
#endif
}

void CIniFile::WriteString(LPCTSTR Section, LPCTSTR Ident, LPCTSTR Value)
{
	WritePrivateProfileString(Section, Ident, Value, m_FileName);
}

void CIniFile::ReadSection(LPCTSTR Section, iniStringList& Strings)
{
	TCHAR *Buffer = new TCHAR[16384];
	TCHAR *P;
	try
	{
		Strings.clear();
		if (GetPrivateProfileString(Section, NULL, NULL, Buffer, 16384, m_FileName) != 0)
		{
			P = Buffer;
			while (*P != 0)
			{
				Strings.insert(Strings.end(), P);
				P += (_tcslen(P) + 1);
			}
		}
	}
	catch (...)
	{

	}

	delete [] Buffer;
}

void CIniFile::ReadSections(iniStringList& Strings)
{
	TCHAR *Buffer = new TCHAR[16384];
	TCHAR *P;
	try
	{
		Strings.clear();
		if (GetPrivateProfileString(NULL, NULL, NULL, Buffer, 16384, m_FileName) != 0)
		{
			P = Buffer;
			while (*P != 0)
			{
				Strings.insert(Strings.end(), P);
				P += (_tcslen(P) + 1);
			}
		}
	}
	catch (...)
	{

	}

	delete [] Buffer;
}

void CIniFile::ReadSectionValues(LPCTSTR Section, iniStringList& Strings)
{
	iniStringList isl;
	iniString buf;
	iniString buf2;
	ReadSection(Section, isl);
	Strings.clear();
	for(slit i = isl.begin(); i != isl.end(); ++i)
	{
		buf = ReadString(Section, (*i).c_str(), _T(""));
		buf2 = (*i);
		buf2 += _T("=");
		buf2 += buf;
		Strings.insert(Strings.end(), buf2);
	}

}

void CIniFile::DeleteKey(LPCTSTR Section, LPCTSTR Ident)
{
	WritePrivateProfileString(Section, Ident, NULL, m_FileName);	
}

void CIniFile::EraseSection(LPCTSTR Section)
{
	if (! WritePrivateProfileString(Section, NULL, NULL, m_FileName) )
	{
		throw "Error writing to .ini file.";
	}
}

void CIniFile::UpdateFile()
{
	WritePrivateProfileString(NULL, NULL, NULL, m_FileName);
}