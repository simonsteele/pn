/**
 * @file l10n.cpp
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2005-2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "l10n.h"

using namespace L10N;

#if PLAT_WIN
namespace
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
#endif


StringLoader* StringLoader::s_pTheInstance = NULL;

tstring StringLoader::Get(UINT dwStringID)
{
	PNASSERT(s_pTheInstance != NULL);
	return s_pTheInstance->load(dwStringID);
}

std::string StringLoader::GetA(UINT dwStringID)
{
	tstring s = Get(dwStringID);
	CT2CA res(s.c_str());
	return std::string(res);
}

#if PLAT_WIN
std::wstring StringLoader::GetW(UINT dwStringID)
{
	tstring s = Get(dwStringID);
	CT2CW res(s.c_str());
	return std::wstring(res);
}
#endif

#if PLAT_WIN
void StringLoader::InitResourceLoader()
{
	SetLoader(new ResourceStringLoader(_Module.m_hInstResource));
}
#endif

void StringLoader::SetLoader(StringLoader* loader)
{
	release();
    
	s_pTheInstance = loader;
	DeletionManager::Register(s_pTheInstance);
}

void StringLoader::release()
{
	if(s_pTheInstance != NULL)
	{
		DeletionManager::UnRegister(s_pTheInstance);
		delete s_pTheInstance;
		s_pTheInstance = NULL;
	}
}

