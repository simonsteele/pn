/**
 * @file l10n.cpp
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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

std::wstring StringLoader::GetW(UINT dwStringID)
{
	USES_CONVERSION;
	tstring s = Get(dwStringID);
	return T2W(s.c_str());
}

void StringLoader::InitResourceLoader()
{
	release();

	s_pTheInstance = new ResourceStringLoader(_Module.m_hInst);
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