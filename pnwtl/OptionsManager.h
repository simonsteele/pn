/**
 * @file optionsmanager.h
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionsmanager_h__included
#define optionsmanager_h__included

#include "ssreg.h"

static const TCHAR* pnregroot = _T("Software\\Echo Software\\PN2\\");

typedef enum {leCRLF, leCR, leLF} ELineEndings;

/**
 * This class represents all of the major
 * options that each editor may wish to check at any
 * given time. To avoid an enormous virtual function
 * table, this class basically contains public
 * members for frequently used options, and the rest
 * are accessed through functions.
 */
class COptionsManager
{
	// Class Functionality
	public:
		COptionsManager();
		~COptionsManager();

		void Load();
		void Save();

		static COptionsManager*	GetInstance();
		static COptionsManager&	GetInstanceRef();
		static void DeleteInstance();

	// Class Members
	public:
		int TabWidth;
		bool ShowIndentGuides;
		ELineEndings LineEndings;

		void SetInterface(LPCTSTR key, bool val);		
		bool GetInterface(LPCTSTR key, bool defval);
		
		void GetSchemesPaths(ctcString& path, ctcString& compiledPath);
		
		SFindOptions*		GetFindOptions(){return &m_FindOptions;}
		SReplaceOptions*	GetReplaceOptions(){return &m_ReplaceOptions;}

	protected:
		static COptionsManager* s_pInstance;
		SFindOptions			m_FindOptions;
		SReplaceOptions			m_ReplaceOptions;
};

#endif