/**
 * @file projectwriter.h
 * @brief Write Project XML
 * @author Simon Steele
 * @note Copyright (c) 2009-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef projectwriter_h__included
#define projectwriter_h__included

#include "third_party/genx/genx.h"
#include "include/pngenx.h"

namespace Projects
{

#define u(x) (constUtf8)x

/**
 * Write project XML files based on the object model.
 */
class ProjectWriter : public GenxXMLWriter
{
public:
	/**
	 * Get the writer instance.
	 */
	genxWriter& GetWriter()
	{
		return m_writer;
	}

	/**
	 * Write a whole project definition.
	 */
	void WriteProject(Project* project)
	{
		genxStartElementLiteral(m_writer, NULL, u("Project"));

		addAttributeConvertUTF8(m_aName, project->GetName());

		if (project->typeID.length() != 0)
		{
			addAttributeConvertUTF8(m_aTypeId, project->typeID.c_str());
		}

		writeFolderContents(project);

		genxEndElement(m_writer);
	}

protected:
	virtual void initXmlBits()
	{
		genxStatus s;

		m_eFile = genxDeclareElement(m_writer, NULL, u("File"), &s);
		m_eFolder = genxDeclareElement(m_writer, NULL, u("Folder"), &s);
		m_eMagicFolder = genxDeclareElement(m_writer, NULL, u("MagicFolder"), &s);
		
		PREDECLARE_ATTRIBUTES()
			ATT("path", m_aPath);
			ATT("name", m_aName);
			ATT("typeId", m_aTypeId);
			ATT("filter", m_aFilter);
			ATT("excludeFiles", m_aExcludedFiles);
			ATT("excludeFolders", m_aExcludedFolders);
		END_ATTRIBUTES();
	}

private:
	void writeFile(File* file)
	{
		genxStartElement(m_eFile);
		
		addAttributeConvertUTF8(m_aPath, file->GetRelativePath());

		// Save user data...
		file->GetUserData().Write(this);

		genxEndElement(m_writer);
	}

	void writeFolderContents(Folder* folder)
	{
		folder->GetUserData().Write(this);

		// Save children folders
		
		for(FOLDER_LIST::const_iterator i = folder->GetFolders().begin();
			i != folder->GetFolders().end();
			++i)
		{
			if ((*i)->GetType() == ptMagicFolder)
			{
				writeMagicFolder(static_cast<MagicFolder*>(*i));
			}
			else
			{
				writeFolder((*i));
			}
		}

		// Save files
		for(FILE_LIST::const_iterator j = folder->GetFiles().begin(); 
			j != folder->GetFiles().end(); 
			++j)
		{
			writeFile((*j));
		}
	}

	void writeMagicFolder(MagicFolder* folder)
	{
		genxStartElement(m_eMagicFolder);
		addAttributeConvertUTF8(m_aName, folder->GetName());
		
		CFileName cfn( folder->GetBasePath() );
		tstring relPath = cfn.GetRelativePath( folder->GetParent()->GetBasePath() );
		addAttributeConvertUTF8(m_aPath, relPath.c_str());
		addAttributeConvertUTF8(m_aFilter, folder->GetFilter());
		addAttributeConvertUTF8(m_aExcludedFiles, folder->GetExcludedFileFilter());
		addAttributeConvertUTF8(m_aExcludedFolders, folder->GetFolderFilter());

		writeFolderContents(folder);

		genxEndElement(m_writer);
	}

	void writeFolder(Folder* folder)
	{
		genxStartElement(m_eFolder);
		addAttributeConvertUTF8(m_aName, folder->GetName());

		writeFolderContents(folder);

		genxEndElement(m_writer);
	}

	genxElement m_eFolder;
	genxElement m_eMagicFolder;
	genxElement m_eFile;
	genxAttribute m_aPath;
	genxAttribute m_aName;
	genxAttribute m_aTypeId;
	genxAttribute m_aFilter;
	genxAttribute m_aExcludedFiles;
	genxAttribute m_aExcludedFolders;
};


}

#endif //#ifndef projectwriter_h__included