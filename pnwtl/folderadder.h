/**
 * @file folderadder.h
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef folderadder_h__included
#define folderadder_h__included

#ifndef filematcher_h__included
	#error folderadder.h requires filematcher.h to be included first.
#endif

//////////////////////////////////////////////////////////////////////////////
// FolderAdder
//////////////////////////////////////////////////////////////////////////////

namespace Projects
{

class FolderAdder : public RegExFileFinder<FolderAdder>//FileFinderImpl<FolderAdder, FolderAdder>
{
	typedef RegExFileFinder<FolderAdder> base;
	friend base;

public:
	FolderAdder() : base(this, &FolderAdder::onFind)
	{
		pFolder = NULL;
		pCursor = NULL;
		setDirChangeCallback(&FolderAdder::onEnterDir);
		setFinishedDirCallback(&FolderAdder::onLeaveDir);
	}

	Folder* GetFolder(LPCTSTR path, LPCTSTR filter, LPCTSTR basePath, bool recurse)
	{
		lpszBasePath = basePath;

		pFolder = newFolder(path);
		pCursor = pFolder;

		// make sure the path has a trailing slash, CPathName will do that.
		CPathName pn(path);

		ClearFilters();
		SetFilters(filter, NULL, NULL, _T("CVS;.svn"));
		FindMatching(pn.c_str(), recurse);

		return pFolder;
	}

protected:
	Folder* pFolder;
	Folder* pCursor;
	LPCTSTR lpszBasePath;

	void onFind(LPCTSTR path, LPCTSTR filename)
	{
		tstring fullpath(path);
		fullpath += filename;
		pCursor->AddFile(fullpath.c_str());
	}

	void onEnterDir(LPCTSTR path)
	{
		Folder* pFolder = newFolder(path);
		pCursor->AddChild(pFolder);
		pCursor = pFolder;
	}

	void onLeaveDir(LPCTSTR path)
	{
		pCursor = pCursor->GetParent();
	}

	virtual Folder* newFolder(LPCTSTR path)
	{
		CPathName fn(path);
		tstring dirName = fn.GetDirectoryName();
		return new Folder(dirName.c_str(), lpszBasePath);
	}
};

//////////////////////////////////////////////////////////////////////////////
// MagicFolderAdder
//////////////////////////////////////////////////////////////////////////////

class MagicFolderAdder : public FolderAdder
{
public:
	void BuildFolder(MagicFolder* folder, LPCTSTR path, LPCTSTR filter, LPCTSTR basePath, bool recurse)
	{
		lpszBasePath = basePath;

		pFolder = folder;
		pCursor = pFolder;

		// make sure the path has a trailing slash, CPathName will do that.
		CPathName pn(path);

		ClearFilters();
		SetFilters(filter, NULL, NULL, _T("CVS;.svn"));
		FindMatching(pn.c_str(), recurse);
	}

protected:
	virtual Folder* newFolder(LPCTSTR path)
	{
		CPathName fn(path);
		tstring dirName = fn.GetDirectoryName();
		MagicFolder* mf = new MagicFolder(dirName.c_str(), path, lpszBasePath);
		mf->SetGotContents(true);
		return mf;
	}
};

} // namespace Projects

#endif // #ifndef folderadder_h__included