/**
 * @file l10n.h
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2005-2011 Simon Steele - http://untidy.net/
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
		static std::wstring GetW(UINT dwStringID);
		static std::string GetA(UINT dwStringID);

		static void InitResourceLoader();

	protected:
		virtual tstring load(UINT dwStringID) = 0;

	protected:
		StringLoader(){}
		static void release();
		
	protected:
		static StringLoader* s_pTheInstance;
	};

	namespace Impl
	{
		/**
		 * Implement resource script string loading
		 */
		class ResourceStringLoader : public StringLoader
		{
		public:
			ResourceStringLoader(HINSTANCE hInst) : m_hInstance(hInst){}
			virtual ~ResourceStringLoader(){}

		// Implement StringLoader
		protected:
			virtual tstring load(UINT dwStringID);
		
		protected:
			HINSTANCE m_hInstance;
		};
	}
}

#define LSS(stringId) L10N::StringLoader::Get(stringId)
#define LSWS(stringId) L10N::StringLoader::GetW(stringId)
#define LSAS(stringId) L10N::StringLoader::GetA(stringId)
#define LS(stringId) L10N::StringLoader::Get(stringId).c_str()
#define LSW(stringId) L10N::StringLoader::GetW(stringId).c_str()
#define LSA(stringId) L10N::StringLoader::GetA(stringId).c_str()
#define MAKE_OPTIONSTREEPATH(groupId, nodeId) L10N::StringLoader::Get(groupId) + _T("\\") + L10N::StringLoader::Get(nodeId)

#endif //#ifndef l10n_h__included_49BE3F95_5C3B_433b_B37F_417D489B9587