/**
 * @file searchoptions.cpp
 * @brief Implement SearchOptions
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"

const char* SearchOptions::GetFindText() const
{
	return (const char*)FindText;
}

void SearchOptions::SetFindText(const char* findText)
{	
	FindText = findText;
}

bool SearchOptions::GetMatchWholeWord() const
{
	return MatchWholeWord;
}

void SearchOptions::SetMatchWholeWord(bool matchWholeWord)
{
	MatchWholeWord = matchWholeWord;
}

bool SearchOptions::GetMatchCase() const
{
	return MatchCase;
}

void SearchOptions::SetMatchCase(bool matchCase)
{
	MatchCase = matchCase;
}

bool SearchOptions::GetUseRegExp() const
{
	return UseRegExp;
}

void SearchOptions::SetUseRegExp(bool useRegExp)
{
	UseRegExp = useRegExp;
}

bool SearchOptions::GetSearchBackwards() const
{
	return !Direction;
}

void SearchOptions::SetSearchBackwards(bool backwards)
{
	Direction = !backwards;
}

bool SearchOptions::GetLoopOK() const
{
	return Loop;
}

void SearchOptions::SetLoopOK(bool loop)
{
	Loop = loop;
}

bool SearchOptions::GetUseSlashes() const
{
	return UseSlashes;
}

void SearchOptions::SetUseSlashes(bool slashes)
{
	UseSlashes = slashes;
}

const char* SearchOptions::GetReplaceText() const
{
	return (const char*)ReplaceText;
}

void SearchOptions::SetReplaceText(const char* text)
{
	ReplaceText = text;
}

bool SearchOptions::GetReplaceInSelection() const
{
	return InSelection;
}

void SearchOptions::SetReplaceInSelection(bool inSelection)
{
	InSelection = inSelection;
}

const char* SearchOptions::GetFileExts() const
{
	return (const char*)FileExts;
}

void SearchOptions::SetFileExts(const char* extensions)
{
	FileExts = extensions;
}


const char* SearchOptions::GetSearchPath() const
{
	return (const char*)Path;
}

void SearchOptions::SetSearchPath(const char* path)
{
	Path = path;
}


bool SearchOptions::GetRecurse() const
{
	return Recurse;
}

void SearchOptions::SetRecurse(bool recurse)
{
	Recurse = recurse;
}


bool SearchOptions::GetIncludeHidden() const
{
	return IncludeHidden;
}

void SearchOptions::SetIncludeHidden(bool hidden)
{
	IncludeHidden = hidden;
}

bool SearchOptions::GetFound() const
{	
	return Found;
}
