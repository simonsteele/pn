// Scintilla source code edit control
/** @file AutoComplete.cxx
 ** Defines the auto completion list box.
 **/
// Copyright 1998-2003 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "Platform.h"

#include "CharacterSet.h"
#include "AutoComplete.h"

#ifdef SCI_NAMESPACE
using namespace Scintilla;
#endif

AutoComplete::AutoComplete() :
	active(false),
	separator(' '),
	typesep('?'),
	ignoreCase(false),
	chooseSingle(false),
	lb(0),
	posStart(0),
	startLen(0),
	cancelAtStartPos(true),
	autoHide(true),
	dropRestOfWord(false)	{
	lb = ListBox::Allocate();
	stopChars[0] = '\0';
	fillUpChars[0] = '\0';
}

AutoComplete::~AutoComplete() {
	if (lb) {
		lb->Destroy();
		delete lb;
		lb = 0;
	}
}

bool AutoComplete::Active() const {
	return active;
}

void AutoComplete::Start(Window &parent, int ctrlID,
	int position, Point location, int startLen_,
	int lineHeight, bool unicodeMode) {
	if (active) {
		Cancel();
	}
	lb->Create(parent, ctrlID, location, lineHeight, unicodeMode);
	lb->Clear();
	active = true;
	startLen = startLen_;
	posStart = position;
}

void AutoComplete::SetStopChars(const char *stopChars_) {
	strncpy(stopChars, stopChars_, sizeof(stopChars));
	stopChars[sizeof(stopChars) - 1] = '\0';
}

bool AutoComplete::IsStopChar(char ch) {
	return ch && strchr(stopChars, ch);
}

void AutoComplete::SetFillUpChars(const char *fillUpChars_) {
	strncpy(fillUpChars, fillUpChars_, sizeof(fillUpChars));
	fillUpChars[sizeof(fillUpChars) - 1] = '\0';
}

bool AutoComplete::IsFillUpChar(char ch) {
	return ch && strchr(fillUpChars, ch);
}

void AutoComplete::SetSeparator(char separator_) {
	separator = separator_;
}

char AutoComplete::GetSeparator() const {
	return separator;
}

void AutoComplete::SetTypesep(char separator_) {
	typesep = separator_;
}

char AutoComplete::GetTypesep() const {
	return typesep;
}

void AutoComplete::SetList(const char *list) {
	lb->SetList(list, separator, typesep);
}

void AutoComplete::Show(bool show) {
	lb->Show(show);
	if (show)
		lb->Select(0);
}

void AutoComplete::Cancel() {
	if (lb->Created()) {
		lb->Clear();
		lb->Destroy();
		active = false;
	}
}


void AutoComplete::Move(int delta) {
	int count = lb->Length();
	int current = lb->GetSelection();
	current += delta;
	if (current >= count)
		current = count - 1;
	if (current < 0)
		current = 0;
	lb->Select(current);
}

void AutoComplete::Select(const char *word, bool bIgnoreCase) {
	int location = -1;

	char *tmpWord = const_cast<char*>(word);
	tmpWord = (bIgnoreCase) ? _strlwr(tmpWord) : tmpWord;
	size_t tmpWordLength = strlen(tmpWord);

	const int maxItemLen=1000;
	char item[maxItemLen];

	int length = lb->Length() - 1;

	for(int i = 0; i <= length; i++)
	{
		lb->GetValue(i, item, maxItemLen);

		char *tmpItem = const_cast<char*>(item);
		tmpItem = (bIgnoreCase) ? _strlwr(tmpItem) : tmpItem;

		if(strncmp(tmpWord, tmpItem, tmpWordLength) == 0)
		{
			location = i;
			break;
		}
	}

	if (location == -1 && autoHide)
		Cancel();
	else
		lb->Select(location);
}

