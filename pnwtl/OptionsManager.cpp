/**
 * @file optionsmanager.cpp
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"

COptionsManager::COptionsManager()
{
	Load();
}

COptionsManager::~COptionsManager()
{
	Save();
}

void COptionsManager::Load()
{
	ssreg::CSRegistry reg;
	ctcString root(pnregroot);
	ctcString cs(root);
	
	// Editor Settings ----------------------
	cs += _T("Editor Settings");
	reg.OpenKey(cs.c_str(), true);
	ShowIndentGuides = reg.ReadBool(_T("IndentationMarkers"), true);
	LineEndings = (ELineEndings)reg.ReadInt(_T("DefaultLineEndings"), leCRLF);
	TabWidth = reg.ReadInt(_T("TabWidth"), 4);

	//cs = root + _T("Interface Settings");
	//reg.OpenKey(cs, true);
	
}

void COptionsManager::Save()
{
	ssreg::CSRegistry reg;
	ctcString root(pnregroot);
	ctcString cs;

	// Editor Settings ----------------------
	cs = root + _T("Editor Settings");
	reg.OpenKey(cs.c_str(), true);
	reg.WriteBool(_T("IndentationMarkers"), ShowIndentGuides);
	reg.WriteInt(_T("DefaultLineEndings"), LineEndings);
	reg.WriteInt(_T("TabWidth"), TabWidth);
	
	//cs = root + _T("Interface Settings");
	//reg.OpenKey(cs, true);
}

void COptionsManager::SetInterface(LPCTSTR key, bool val)
{
	ssreg::CSRegistry reg;
	ctcString root(pnregroot);
	root += _T("Interface Settings");

	reg.OpenKey(root.c_str());
	reg.WriteBool(key, val);
}

bool COptionsManager::GetInterface(LPCTSTR key, bool defval)
{
	ssreg::CSRegistry reg;
	ctcString root(pnregroot);
	root += _T("Interface Settings");

	reg.OpenKey(root.c_str());
	return reg.ReadBool(key, defval);
}