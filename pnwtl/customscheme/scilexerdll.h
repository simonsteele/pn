#ifndef scilexerdll_h__included
#define scilexerdll_h__included

// The order of these includes is important.
#include <stdlib.h>
#include "scintilla\platform.h"
#include "scintilla\propset.h"
//#include "scintilla\accessor.h"
//#include "scintilla\windowaccessor.h"

// Create WordLists...

static WordList **StringToWordLists(char *val[]) {
    int dim = 0;

    while (val[dim])
        dim++;

	WordList **wla = new WordList*[dim+1];
    
	for (int i = 0; i < dim; i++) 
	{
        wla[i] = new WordList;
        wla[i]->Set(val[i]);
    }
    
	wla[dim] = 0;
    
	return wla;
}

static void FreeWordLists(WordList *wla[])
{
	int dim = 0;
	while (wla[dim])
	{
		delete wla[dim];
		dim++;
	}

	delete [] wla;
	wla = NULL;
}

// These functions are exported:
int EXPORT __stdcall GetLexerCount();

void EXPORT __stdcall GetLexerName(unsigned int lexer, char *name, int buf);

void EXPORT __stdcall Lex(unsigned int lexer, unsigned int startPos, int length, 
						 int initStyle, char *words[], WindowID window, char *props);

void EXPORT __stdcall Fold(unsigned int lexer, unsigned int startPos, int length, 
						 int initStyle, char *words[], WindowID window, char *props);

#endif