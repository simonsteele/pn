
#include "stdafx.h"
#include "FileAssoc.h"
#include "resource.h"
#include "FileAssocInfo.h"

// ProgID used to associate a file extension with this application
// Format: Application.Component.Version
// Note: component will be the extension to register here
#define PROGID_TEMPLATE _T("PN2.%s.1")

#define INVALID_CHARACTERS _T("\\/:?\"<>|.*")

/**
Helper function to get the current module file name.
*/
DWORD AtlGetModuleFileName(_CSTRING_NS::CString& strPath, bool fQuoteSpaces = false)
{
	DWORD nRes = 0;
	for(DWORD nLen = 256; ; nLen *= 2)
	{
		LPTSTR pszPath = strPath.GetBufferSetLength(nLen);
		if(pszPath == NULL)
		{
			nRes = 0;
			break;
		}
		nRes = GetModuleFileName(NULL, pszPath, nLen);
		if(nRes < nLen - 1)
		{
			if(fQuoteSpaces == true)
			{
				if(nRes > nLen - 3)
					pszPath = strPath.GetBufferSetLength(nLen + 3);

				::PathQuoteSpaces(pszPath);
			}
			break;
		}
	}

	strPath.ReleaseBuffer();
	return nRes;
}

//////////////////////////////////////////////////////////////////////////////
// FileAssoc
//////////////////////////////////////////////////////////////////////////////

FileAssoc::FileAssoc(LPCTSTR ext, LPCTSTR verb)
	: m_extension(ext)
	, m_verb(VerbNone)
	, m_isAssociated(false)
	, m_fConflict(false)
	, m_verbCmd()
	, m_verbAppName()
	, m_previousCmd()
	, m_typeName()
	, m_appPath()
{
	DWORD res = AtlGetModuleFileName(m_appPath, true);
	ATLASSERT(res != 0);

	if(verb != NULL)
	{
		SetExtensionAndVerb(ext, verb);
	}
}
FileAssoc::FileAssoc(const FileAssoc& fileAssoc)
	: m_extension(fileAssoc.m_extension)
	, m_verb(fileAssoc.m_verb)
	, m_isAssociated(fileAssoc.m_isAssociated)
	, m_fConflict(fileAssoc.m_fConflict)
	, m_verbCmd(fileAssoc.m_verbCmd)
	, m_verbAppName(fileAssoc.m_verbAppName)
	, m_previousCmd(fileAssoc.m_previousCmd)
	, m_typeName(fileAssoc.m_typeName)
	, m_appPath(fileAssoc.m_appPath)
{
	if(m_appPath.GetLength() == 0)
	{
		DWORD res = AtlGetModuleFileName(m_appPath, true);
		ATLASSERT(res != 0);
	}
}

bool FileAssoc::operator ==(const FileAssoc& other) const
{
	return m_extension == other.m_extension;
}

bool FileAssoc::IsValid() const
{
	return m_extension.GetLength() > 0 && m_verb != VerbNone;
}

bool FileAssoc::HasConflict() const
{
	return m_fConflict;
}

LPCTSTR FileAssoc::GetInvalidChars()
{
	return INVALID_CHARACTERS;
}

const CString& FileAssoc::GetCurrentAppName() const
{
	return m_verbAppName;
}

const CString& FileAssoc::GetCurrentTypeName() const
{
	return m_typeName;
}

const CString& FileAssoc::GetExtension() const
{
	return m_extension;
}

FileAssoc::Verb FileAssoc::GetVerb() const
{
	return m_verb;
}

LPCTSTR FileAssoc::GetVerbName() const
{
	return VerbToString(m_verb);
}

void FileAssoc::SetExtensionAndVerb(const CString& ext, const CString& verb)
{
	Reset();

	m_extension = ext;
	// Remove all leading/ending spaces and invalid characters from the
	// extension (an extension is the string after the last point in a
	// filename and the code assumes extensions without point throughout)
	// Lower-case is used for simplicity only
	m_extension.Trim().MakeLower();
	LPCTSTR invalidChars = INVALID_CHARACTERS;
	for(int i = 0; invalidChars[i] != 0; i++)
	{
		m_extension.Remove(invalidChars[i]);
	}

	m_verb = StringToVerb(verb);
	m_fConflict = false;

	CheckExtension();
}

void FileAssoc::SetVerb(const Verb& verb)
{
	m_verb = verb;
}

void FileAssoc::Associate()
{
	if(m_fConflict && IsValid())//m_verbCmd.Find(m_appPath) == -1)
	{
		AssociateExtention();
	}
}

void FileAssoc::Disassociate()
{
	// If we have a previous command, all we need to do is to restore it.
	// If we do not have a previous command, we need to check if ou verb is the
	// only registered verb in the ProgID. If that is the case, we remove the
	// extension and ProgID keys. In the case of other registered verbs, we
	// simply delete the verb key.

	CRegKey regClasses;
	LONG res = regClasses.Open(HKEY_CURRENT_USER, _T("Software\\Classes"));

	// Get the ProgID
	CString progId;
	CString keyName;
	keyName.Format(_T(".%s"), m_extension);
	CRegKey reg;
	res = reg.Open(regClasses, keyName);
	if(res == ERROR_SUCCESS)
	{
		ULONG size = 0;
		reg.QueryStringValue(NULL, NULL, &size);
		reg.QueryStringValue(NULL, progId.GetBuffer(size), &size);
		progId.ReleaseBuffer();
		reg.Close();
	}

	if(m_previousCmd.GetLength() > 0)
	{
		keyName.Format(_T("%s\\shell\\%s\\command"), progId, VerbToString(m_verb));
		res = reg.Open(regClasses, keyName);
		if(res == ERROR_SUCCESS)
		{
			reg.SetStringValue(NULL, m_previousCmd);
			reg.Close();
		}
	}
	else
	{
		keyName.Format(_T("%s\\shell"), progId);
		TCHAR name[256];
		res = reg.Open(regClasses, keyName);
		for(int i = 0; res == ERROR_SUCCESS; i++)
		{
			DWORD size = 256;
			res = reg.EnumKey(i, name, &size);
		}

		if(i == 2)
		{
			regClasses.RecurseDeleteKey(progId);
			keyName.Format(_T(".%s"), m_extension);
			regClasses.RecurseDeleteKey(keyName);
		}
		else
		{
			keyName.Format(_T("%s\\shell\\%s"), progId);
			regClasses.RecurseDeleteKey(keyName);
		}
	}
}

void FileAssoc::Reset()
{
	m_extension.Empty();
	m_verb = VerbNone;
	m_isAssociated = false;
	m_fConflict = false;
	m_verbCmd.Empty();
	m_verbAppName.Empty();
	m_previousCmd.Empty();
	m_typeName.Empty();
}

void FileAssoc::CheckExtension()
{
	// Check if the extension is registered and if the verb is already in use

	CString ext(_T('.'));
	ext += m_extension;

	CRegKey reg;
	LONG res = reg.Open(HKEY_CLASSES_ROOT, ext, KEY_READ);
	m_isAssociated = (res == ERROR_SUCCESS);
	if(m_isAssociated)
	{
		TCHAR progId[256];
		ULONG len = 256;
		res = reg.QueryStringValue(NULL, progId, &len);
		if(res == ERROR_SUCCESS)
		{
			GetVerbCommand(ext, m_verb);
		}
		reg.Close();
	}

	GetTypeName();
}

FileAssoc::Verb FileAssoc::StringToVerb(const CString& verbName) const
{
	Verb verb;
	if(verbName == _T("Open"))
		verb = VerbOpen;
	else
		verb = VerbEdit;
	return verb;
}

LPCTSTR FileAssoc::VerbToString(Verb verb) const
{
	LPCTSTR verbName;
	if(verb == VerbOpen)
		verbName = _T("Open");
	else
		verbName = _T("Edit");
	return verbName;
}

void FileAssoc::GetVerbCommand(const CString& assoc, Verb verb)
{
	CString verbString(VerbToString(verb));

	CString assocString;
	DWORD size = MAX_PATH;
	HRESULT hr = AssocQueryString(ASSOCF_NOFIXUPS, ASSOCSTR_FRIENDLYAPPNAME, assoc,
		verbString, assocString.GetBuffer(MAX_PATH), &size);
	assocString.ReleaseBuffer();
	if(SUCCEEDED(hr))
	{
		m_verbAppName = assocString;

		size = MAX_PATH;
		hr = AssocQueryString(ASSOCF_NOFIXUPS, ASSOCSTR_COMMAND, assoc,
			verbString, assocString.GetBuffer(MAX_PATH), &size);
		assocString.ReleaseBuffer();
		if(SUCCEEDED(hr))
			m_verbCmd = assocString;
	}

	// Convert both paths to lowercase for comparison purposes...
	CString lcRegCmd(m_verbCmd);
	lcRegCmd.MakeLower();
	CString lcApp(m_appPath);
	lcApp.MakeLower();

	m_fConflict = (m_verbCmd.GetLength() > 0 && lcRegCmd.Find(lcApp) == -1);
}

void FileAssoc::GetTypeName()
{
	CString tmpFileName(_T("a."));
	tmpFileName += m_extension;
	SHFILEINFO sfi = { 0 };
	DWORD_PTR res = ::SHGetFileInfo(tmpFileName, FILE_ATTRIBUTE_NORMAL, &sfi,
		sizeof(sfi), SHGFI_TYPENAME | SHGFI_USEFILEATTRIBUTES);
	if(res != 0)
		m_typeName = sfi.szTypeName;
}

/**
*/
bool FileAssoc::AssociateExtention(LPCTSTR pDescription /*= NULL*/, LPCTSTR pIconFile /*= NULL*/)
{
	// Get the ProgID
	CString progId;
	CString keyName;
	keyName.Format(_T(".%s"), m_extension);
	CRegKey reg;
	LONG res = reg.Open(HKEY_CLASSES_ROOT, keyName, KEY_READ);
	if(res == ERROR_SUCCESS)
	{
		ULONG size = 0;
		reg.QueryStringValue(NULL, NULL, &size);
		reg.QueryStringValue(NULL, progId.GetBuffer(size), &size);
		progId.ReleaseBuffer();
		reg.Close();
	}
	else
	{
		progId.Format(PROGID_TEMPLATE, m_extension);
	}

	CRegKey regClasses;
	res = regClasses.Open(HKEY_CURRENT_USER, _T("Software\\Classes"));

	res = reg.Create(regClasses, keyName);
	if(res == ERROR_SUCCESS)
	{
		reg.SetStringValue(NULL, progId);
		reg.Close();
	}

	if(pDescription != NULL)
	{
		res = reg.Create(regClasses, progId);
		if(res == ERROR_SUCCESS)
		{
			reg.SetStringValue(NULL, pDescription);
			reg.Close();
		}
	}

	keyName.Format(_T("%s\\shell\\%s\\command"), progId, VerbToString(m_verb));
	res = reg.Open(regClasses, keyName);
	if(res == ERROR_SUCCESS)
	{
		// Save previous verb command only if we override it and if it is not
		// our own
		if(m_verbCmd.GetLength() > 0 && m_verbCmd.Find(m_appPath) == -1)
		{
			m_previousCmd = m_verbCmd;
		}
	}
	else
	{
		// Either this extension has no association or it is defined in HCR.
		// In both cases, we assign our verb in the users classes key as the
		// user might not have write rights to HCR.
		res = reg.Create(regClasses, keyName);
	}

	if(res == ERROR_SUCCESS)
	{
		ATLASSERT(m_appPath.GetLength() > 0);

		CString cmd;
		cmd.Format(_T("%s \"%%1\""), m_appPath);

		reg.SetStringValue(NULL, cmd);
		reg.Close();
	}

	if(pIconFile != NULL)
	{
		keyName.Format(_T("%s\\DefaultIcon"), progId);
		res = reg.Create(regClasses, keyName);
		if(res == ERROR_SUCCESS)
		{
			reg.SetStringValue(NULL, pIconFile);
			reg.Close();
		}
	}

	m_fConflict = false;

	return res == ERROR_SUCCESS;
}

/**
*/
bool FileAssoc::AssociateExtention(LPCTSTR pDescription, int iIconIndex)
{
	ATLASSERT(m_appPath.GetLength() > 0);

	CString iconFile;
	iconFile.Format(_T("%s,%d"), m_appPath, iIconIndex);

	return AssociateExtention(pDescription, iconFile);
}


//////////////////////////////////////////////////////////////////////////////
// FileAssocManager::FileAssocHelper
//////////////////////////////////////////////////////////////////////////////

bool AtlGetPrivateProfileString(LPCTSTR section, LPCTSTR key, LPCTSTR file, CString& val)
{
	DWORD size = 0;
	DWORD read;
	do
	{
		size += 256;
		read = ::GetPrivateProfileString(section, key, _T(""), val.GetBufferSetLength(size), size, file);
	} while(read == size - 1);
	val.ReleaseBuffer();
	return read > 0;
}

FileAssocManager::FileAssocHelper::FileAssocHelper()
	: FileAssoc()
{}
FileAssocManager::FileAssocHelper::FileAssocHelper(const FileAssoc& fa)
	: FileAssoc(fa)
{}

bool FileAssocManager::FileAssocHelper::Load(const CString& section, const CString& file)
{
	Reset();

	bool res = AtlGetPrivateProfileString(section, _T("Extension"), file, m_extension);
	if(res)
	{
		m_extension.Trim().MakeLower().Remove(_T('.'));
		m_extension.Remove(_T('*'));

		CString verb;
		if(AtlGetPrivateProfileString(section, _T("Verb"), file, verb))
			m_verb = StringToVerb(verb);

		AtlGetPrivateProfileString(section, _T("PreviousCommand"), file, m_previousCmd);

		CString assoc(_T("."));
		assoc += m_extension;
		GetVerbCommand(assoc, m_verb);
		GetTypeName();
		res = true;
	}
	return res;
}

void FileAssocManager::FileAssocHelper::Save(const CString& section, const CString& file)
{
	// Clear this section
	::WritePrivateProfileSection(section, _T(""), file);

	::WritePrivateProfileString(section, _T("Extension"), m_extension, file);
	::WritePrivateProfileString(section, _T("Verb"), VerbToString(m_verb), file);
	if(m_previousCmd.GetLength() > 0)
		::WritePrivateProfileString(section, _T("PreviousCommand"), m_previousCmd, file);
}

//////////////////////////////////////////////////////////////////////////////
// FileAssocManager
//////////////////////////////////////////////////////////////////////////////

FileAssocManager::FileAssocManager()
	: m_associate()
	, m_disassociate()
{
	Load();
}

bool FileAssocManager::CheckAssociations()
{
	bool res = false;

	FileAssocInfo fai;

	CString msg;
	for(int i = 0; i < m_associate.GetSize(); i++)
	{
		if(m_associate[i].HasConflict())
		{
			fai.m_unassociated.Add(m_associate[i]);
		}
	}

	if(fai.m_unassociated.GetSize() > 0)
	{
		if(OPTIONS->Get(PNSK_INTERFACE, _T("ShowAssocQuestion"), true))
			res = fai.DoModal() == IDYES;
		else
			res = true;
	}

	return res;
}

void FileAssocManager::UpdateAssociations()
{
	int i;
	for(i = 0; i < m_associate.GetSize(); i++)
	{
		m_associate[i].Associate();
	}
	for(i = 0; i < m_disassociate.GetSize(); i++)
	{
		m_disassociate[i].Disassociate();
	}

	// Notify the system about the changes
	::SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NULL, NULL);

	Save();
}

void FileAssocManager::Load()
{
	/*
	[Association1]
	Extension=ext
	Verb=Open|Edit
	PreviousCommand=... (optional)

	[AssociationN]
	...
	*/
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);

	CString filename(path.c_str());
	filename += _T("FileAssociations.ini");

	CString section;
	int i = 1;
	FileAssocHelper fa;
	for(;;)
	{
		section.Format(_T("Association%d"), i++);

		if(fa.Load(section, filename) == false)
		{
			break;
		}

		if(fa.IsValid())
		{
			m_associate.Add(fa);
			FileAssocHelper fa2(m_associate[m_associate.GetSize() - 1]);
		}
	}
}

void FileAssocManager::Save()
{
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);

	CString filename(path.c_str());
	filename += _T("FileAssociations.ini");

	//HACK: Clear the file to ensure we do not keep references to disassociated extensions.
	HANDLE hFile;
	hFile = ::CreateFile(filename, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	::CloseHandle(hFile);

	CString section;
	for(int i = 0; i < m_associate.GetSize(); i++)
	{
		section.Format(_T("Association%d"), i + 1);
		FileAssocHelper fa(m_associate[i]);
		fa.Save(section, filename);
	}
}

void FileAssocManager::SetAssociation(const FileAssoc& fileAssoc)
{
	int i = m_associate.Find(fileAssoc);
	if(i == -1)
	{
		m_associate.Add(fileAssoc);
	}
	else
	{
		// Only the verb can be modified and this preserves the prevous
		// command member.
		m_associate[i].SetVerb(fileAssoc.GetVerb());//m_associate[i].SetAtIndex(i, fileAssoc);
	}
	m_disassociate.Remove(fileAssoc);
}

void FileAssocManager::UnsetAssociation(const FileAssoc& fileAssoc)
{
	BOOL removed = m_associate.Remove(fileAssoc);
	if(removed)
	{
		m_disassociate.Add(fileAssoc);
	}
}

const FileAssocManager::FileAssocs& FileAssocManager::GetAssociations() const
{
	return m_associate;
}

/**
*/
bool FileAssocManager::RegisterOpenWith()
{
	// HKEY_CURRENT_USER\Software\Classes\Applications\pn.exe\shell\open\command
	//TODO: Set the above registry key def value to "path\to\pn.exe" "%1"
	return false;
}
