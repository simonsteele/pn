/**
 * @file l10n.cpp
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2005-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "l10n.h"

using namespace L10N;
using namespace L10N::Impl;

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

std::wstring StringLoader::GetW(UINT dwStringID)
{
	tstring s = Get(dwStringID);
	CT2CW res(s.c_str());
	return std::wstring(res);
}

void StringLoader::InitResourceLoader()
{
	release();

	s_pTheInstance = new ResourceStringLoader(_Module.m_hInstResource);
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

tstring ResourceStringLoader::load(UINT dwStringID)
{
	CString s;
	s.LoadString(m_hInstance, dwStringID);
	return (LPCTSTR)s;
}