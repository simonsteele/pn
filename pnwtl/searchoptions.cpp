/**
 * @file searchoptions.cpp
 * @brief Implement SearchOptions
 * @author Simon Steele
 * @note Copyright (c) 2006-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"

SearchOptions::SearchOptions() :
	Found(false),
	NoCursorMove(false)
{
}

const char* SearchOptions::GetFindText() const
{
	return FindText.c_str();
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

extensions::EFindWhere SearchOptions::GetFindTarget() const
{
	return m_findWhere;
}

void SearchOptions::SetFindTarget(extensions::EFindWhere target)
{
	m_findWhere = target;
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

bool SearchOptions::GetNoCursorMove() const
{
	return NoCursorMove;
}

void SearchOptions::SetNoCursorMove(bool reposition)
{
	NoCursorMove = reposition;
}

const char* SearchOptions::GetReplaceText() const
{
	return ReplaceText.c_str();
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
	return FileExts.c_str();
}

void SearchOptions::SetFileExts(const char* extensions)
{
	FileExts = extensions;
}


const char* SearchOptions::GetSearchPath() const
{
	return Path.c_str();
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

extensions::EFIFFileSet SearchOptions::GetFileSet() const
{
	return m_fileSet;
}

void SearchOptions::SetFileSet(extensions::EFIFFileSet fileSet)
{
	m_fileSet = fileSet;
}

bool SearchOptions::GetFound() const
{	
	return Found;
}

void SearchOptions::SetFound(bool found)
{
	Found = found;
}