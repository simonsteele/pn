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
		virtual ~COptionsManager();

		void Load();
		void Save();

	// Class Members
	public:
		int TabWidth;
		bool ShowIndentGuides;
		ELineEndings LineEndings;

		void SetInterface(LPCTSTR key, bool val);		
		bool GetInterface(LPCTSTR key, bool defval);
};

#endif