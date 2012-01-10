#ifndef plugin_h__included
#define plugin_h__included

class Plugin
{
	public:
        Plugin(LPCTSTR path) : m_hModule(NULL)
		{
#if PLAT_WIN
			m_hModule = ::LoadLibrary(path);
#endif
		}

		~Plugin()
		{
			Unload();
		}

		FARPROC FindFunction(LPCSTR fnName)
		{
#if PLAT_WIN
			if(m_hModule)
				return ::GetProcAddress(m_hModule, fnName);
			else
#endif
				return NULL;
		}

		virtual bool Valid()
		{
			return m_hModule != NULL;
		}

		virtual void Unload()
		{
			unload();
		}

	protected:
		HMODULE m_hModule;

		void unload()
		{
#if PLAT_WIN
			if(m_hModule)
				::FreeLibrary(m_hModule);
#endif
		}
};

#endif // #ifndef plugin_h__included