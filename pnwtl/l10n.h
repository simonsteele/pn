/**
 * @file l10n.h
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2005-2012 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef l10n_h__included_49BE3F95_5C3B_433b_B37F_417D489B9587
#define l10n_h__included_49BE3F95_5C3B_433b_B37F_417D489B9587

namespace L10N
{
	/**
	 * Base class for string loading
	 */
	class StringLoader : DelObject
	{
	public:
		virtual ~StringLoader(){}

		static tstring Get(UINT dwStringID);
#if PLAT_WIN
		static std::wstring GetW(UINT dwStringID);
#endif
		static std::string GetA(UINT dwStringID);

#if PLAT_WIN
		static void InitResourceLoader();
#endif
        /** 
         * Take ownership of parameter loader, and use for all string lookups
         */
        static void SetLoader(StringLoader* loader);
        
	protected:
		virtual tstring load(UINT dwStringID) = 0;

		StringLoader(){}
		static void release();
		
	private:
		static StringLoader* s_pTheInstance;
	};
}

#define LSS(stringId) L10N::StringLoader::Get(stringId)
#define LSWS(stringId) L10N::StringLoader::GetW(stringId)
#define LSAS(stringId) L10N::StringLoader::GetA(stringId)
#define LS(stringId) L10N::StringLoader::Get(stringId).c_str()
#define LSW(stringId) L10N::StringLoader::GetW(stringId).c_str()
#define LSA(stringId) L10N::StringLoader::GetA(stringId).c_str()
#define MAKE_OPTIONSTREEPATH(groupId, nodeId) L10N::StringLoader::Get(groupId) + _T("\\") + L10N::StringLoader::Get(nodeId)

#endif //#ifndef l10n_h__included_49BE3F95_5C3B_433b_B37F_417D489B9587