/**
 * @file ctagsnavigator.h
 * @brief CTAGS output parser for jump to function implementation.
 * @author Simon Steele
 * @note Copyright (c) 2004-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the CPPNAVIGATOR_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// CPPNAVIGATOR_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef CPPNAVIGATOR_EXPORTS
#define CPPNAVIGATOR_API __declspec(dllexport) __stdcall 
#else
#define CPPNAVIGATOR_API __declspec(dllimport) __stdcall
#endif

#include "../../jumptointerface.h"

#include <string>

#define PARSE_BUFFER_SIZE	16384

typedef struct tagParseState
{
	char		buffer[PARSE_BUFFER_SIZE*2];
	int			extra;
	extensions::ITagSink*	sink;
} PARSESTATE, * LPPARSESTATE;

class CTagsTagSource : public extensions::ITagSource
{
public:
	CTagsTagSource();

	/**
	 * Get a list of schemes supported
	 */
	virtual const char* GetSchemesSupported();
	
	/**
	 * Enumerate tags
	 */
	virtual bool FindTags(extensions::ITagSink* sink, const wchar_t* filename, void* userData, MASKSTRUCT mask, const char* scheme);

	/**
	 * Load externally defined ctags languages.
	 */
	void LoadAdditionalLanguages();

	/**
	 * Set a path to look in for external ctags definitions
	 */
	void SetTaggersPath(const wchar_t* path);

private:
	bool canParse(char* buffer, DWORD dwLength);
	bool parseData(LPPARSESTATE state, DWORD dwBytesRead, MASKSTRUCT mask, void* userData, int* ltypes, int* utypes);
	bool processLine(char* p, const char* pLineEnd, const int* ltypes, const int* utypes, extensions::METHODINFO& mi);
	
	/**
	 * Path to taggers directory. Used for external ctags language definitions.
	 */
	std::wstring m_taggersPath;
	std::string m_schemes;
	std::wstring m_optionsParam;
};