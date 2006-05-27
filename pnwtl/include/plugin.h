#ifndef plugin_h__included
#define plugin_h__included

class Plugin
{
	public:
		Plugin(LPCTSTR path)
		{
			hModule = ::LoadLibrary(path);
		}

		~Plugin()
		{
			Unload();
		}

		FARPROC FindFunction(LPCSTR fnName)
		{
			if(hModule)
				return ::GetProcAddress(hModule, fnName);
			else
				return NULL;
		}

		virtual bool Valid()
		{
			return hModule != NULL;
		}

		virtual void Unload()
		{
			unload();
		}

	protected:
		HMODULE hModule;

		void unload()
		{
			if(hModule)
				::FreeLibrary(hModule);
		}
};

#endif // #ifndef plugin_h__included