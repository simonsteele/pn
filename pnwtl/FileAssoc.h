/**
 * @file FileAssoc.h
 * @brief File Associations Management
 * @author Bjoern Graf
 * @note Copyright (c) 2005-2006 Bjoern Graf and Simon Steele
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#pragma once

class FileAssoc
{
protected:
	//TODO: support "Edit/Open with PN2"?
	// see: ms-help://MS.VSCC.2003/MS.MSDNQTR.2003FEB.1033/shellcc/platform/shell/programmersguide/shell_basics/shell_basics_extending/fileassociations/fa_verbs.htm#supplemental
	enum Verb
	{
		VerbNone = -1,
		VerbOpen,
		VerbEdit,
		VerbEditWith,
		VerbMax
	};

public:
	FileAssoc(LPCTSTR ext = NULL, LPCTSTR verb = NULL);
	FileAssoc(const FileAssoc& fileAssoc);

	bool operator ==(const FileAssoc& other) const;

	bool IsValid() const;
	bool HasConflict() const;
	bool IsAssociated() const;

	static LPCTSTR GetInvalidChars();

	const CString& GetCurrentAppName() const;
	const CString& GetCurrentTypeName() const;
	const CString& GetExtension() const;
	Verb GetVerb() const;
	CString GetVerbName(bool forDisplay = false) const;

	void SetExtensionAndVerb(const CString& ext, const CString& verb);
	void SetVerb(const Verb& verb);
	void Associate();
	void Disassociate();

protected:

	void Reset();

	void CheckExtension();

	Verb StringToVerb(const CString& verbName) const;
	CString VerbToString(Verb verb, bool forDisplay = false) const;

	void GetVerbCommand(const CString& assoc, Verb verb);
	void GetTypeName();

	bool AssociateExtention(LPCTSTR pDescription = NULL, LPCTSTR pIconFile = NULL);
	bool AssociateExtention(LPCTSTR pDescription, int iIconIndex);

protected:
	CString m_extension;
	Verb m_verb;
	bool m_isAssociated;
	bool m_fConflict;

	CString m_verbCmd;
	CString m_verbAppName;

	CString m_previousCmd;

	CString m_typeName;
	CString m_appPath;
};

class FileAssocManager
{
	class FileAssocHelper : public FileAssoc
	{
	public:
		FileAssocHelper();
		FileAssocHelper(const FileAssoc& fa);

		bool Load(const CString& section, const CString& file);
		void Save(const CString& section, const CString& file);
	};

public:
	typedef CSimpleArray<FileAssoc> FileAssocs;

	FileAssocManager();

	bool CheckAssociations();
	void UpdateAssociations();

	void Load();
	void Save();

	void SetAssociation(const FileAssoc& fileAssoc);
	void UnsetAssociation(const FileAssoc& fileAssoc);

	const FileAssocs& GetAssociations() const;

	void CheckOpenWith();

private:
	bool RegisterOpenWith();
	bool RegisterOpenWithForPerceivedType();

private:
	FileAssocs m_associate;
	FileAssocs m_disassociate;
};
