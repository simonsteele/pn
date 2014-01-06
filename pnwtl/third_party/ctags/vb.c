/*********************************************************************
   Copyright (C) 2006 DeepSoft - Manuel Sandoval
 
   This software is provided 'as-is', without any express or implied
   warranty.  In no event will the authors be held liable for any damages
   arising from the use of this software.
 
   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:
 
   1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
 
   2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
 
   3. This notice may not be removed or altered from any source distribution.
 
   http://www.anyedit.org
  Manuel Sandoval webmailusr-msn@yahoo.com
  //M.Sandoval: New Module
**********************************************************************/
/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "entry.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_CLASS,  K_VARIABLE, K_FUNCTION,K_CONST,K_MODULE, K_SUB
} phpKind;

static kindOption VBKinds [] = {
    { TRUE, 'c', "class",       "classes" },   
	{ TRUE, 'd', "dim",         "variables" },
    { TRUE, 'f', "function",    "functions" },
	{ TRUE, 'k', "const",       "constants" },
	{ TRUE, 'm', "module",		"modules" },
	{ TRUE, 's', "sub",         "procedures" },
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isLetter(const int c){    return (boolean)(isalpha(c) || (c >= 127  &&  c <= 255));}
static boolean isVarChar1(const int c){    return (boolean)(isLetter (c)  ||  c == '_');}
static boolean isVarChar(const int c)
{    return (boolean)(isVarChar1 (c) || isdigit (c));
}

 void MyMakeTag( vString*   name, kindOption*   kinds,   int kind , char*sgn, char*s0, char*s1)
{
	tagEntryInfo e;
    initTagEntry (&e, vStringValue (name));	
	e.language="VB.Net";
	e.extensionFields.signature=sgn;
	e.extensionFields.scope[0]=s0;
	e.extensionFields.scope[1]=s1;
	e.kindName = kinds [kind].name;
    e.kind     = kinds [kind].letter;
    makeTagEntry (&e);
}
static void findVBTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line1;
	char * cc=0; 
	char * cm=0;
	char scope=1;
	char sgn[100];
	int iSgn=0;
	char * line=0;
	const char *cp=0;
	char * start_pos;
	while ((line1 = fileReadLine ()) != NULL)
	{
		if(strlen((char*)line1)==0)continue;
		if(line)free(line);
		line=_strdup((char*)line1);strlwr((char*)line); //case insensitive
		cp = line;
		while (isspace (*cp))cp++;

		start_pos=strstr((char*)cp,"dim");
		if(!start_pos)start_pos=strstr((char*)cp,"const");
		if(!start_pos)start_pos=strstr((char*)cp,"withevents");//ManuelSandoval2 new keyword				

		//ManuelSandoval2 handle end keyword. strstr can get confused with friEND or other
		//words containing "end".
		//Must go here, before all the possible subsequent keywords (function/sub/class)
		if(!start_pos)
		{
			char* tmp=strstr((char*)cp,"end ");
			if(cp==tmp)start_pos=tmp;
		}

		if(!start_pos)start_pos=strstr((char*)cp,"function");
		if(!start_pos)start_pos=strstr((char*)cp,"sub");
		if(!start_pos)start_pos=strstr((char*)cp,"class");
		if(start_pos)cp=start_pos;

		//ManuelSandoval2 new keyword: withevents
		if (strncmp ((const char*) cp, "withevents", (size_t) 10) == 0  && isspace ((int) cp [10]))
		{
			cp += 10;
			while (isspace ((int) *cp))	++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);	++cp;	}
			vStringTerminate (name);
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			MyMakeTag (name, VBKinds, K_VARIABLE,0, cc?"class":0, cc);
			vStringClear (name);
		} 
		else if (strncmp ((const char*) cp, "dim", (size_t) 3) == 0  && isspace ((int) cp [3]))
		{
			cp += 3;
			while (isspace ((int) *cp))	++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);	++cp;	}
			vStringTerminate (name);
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			if(scope)MyMakeTag (name, VBKinds, K_VARIABLE,0, cc?"class":0, cc);
			vStringClear (name);
		} 
		else if (strncmp ((const char*) cp, "const", (size_t) 5) == 0 && ! isalnum ((int) cp [5]))
		{
			cp += 5;
			while (isspace ((int) *cp))
			++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);++cp;	}
			vStringTerminate (name);
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			if(scope)MyMakeTag (name, VBKinds, K_CONST,0,cc?"class":0, cc);
			vStringClear (name);
		}
		else if (strncmp ((const char*) cp, "end", (size_t) 3) == 0  && isspace ((int) cp [3]))
		{			
			cp+=3;			
			while (isspace ((int) *cp))	++cp;
			if (strncmp ((const char*) cp, "sub", (size_t) 3) == 0  && (isspace ((int) cp [3])||!cp[3]))	scope=1;
			if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  && (isspace ((int) cp [8])||!cp[8]))scope=1;
			continue;
		}
		else if (strncmp ((const char*) cp, "sub", (size_t) 3) == 0  && isspace ((int) cp [3]))
		{
			scope=0;			
			cp += 3;
			while (isspace ((int) *cp))	++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);	++cp;	}
			vStringTerminate (name);
			while (isspace ((int) *cp)) ++cp;
			memset(sgn,0,sizeof(sgn)); iSgn=0;	while((*cp!=0) && (iSgn<sizeof(sgn)-1)){sgn[iSgn]=*cp;cp++;iSgn++;}
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			MyMakeTag (name, VBKinds, K_SUB, strdup(sgn),cc?"class":0, cc);
			vStringClear (name);
		} 
		else if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&isspace ((int) cp [8]))
		{
			scope=0;
			cp += 8;
			while (isspace ((int) *cp))
			++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_'){vStringPut (name, (int) *cp);++cp;	}
			vStringTerminate (name);
			while (isspace ((int) *cp))++cp; 
			memset(sgn,0,sizeof(sgn)); iSgn=0;	while(*cp && iSgn<sizeof(sgn)-1){sgn[iSgn]=*cp;cp++;iSgn++;}
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			MyMakeTag (name, VBKinds, K_FUNCTION, strdup(sgn),cc?"class":0, cc);
			vStringClear (name);
		} 
		else if (strncmp ((const char*) cp, "module", (size_t) 6) == 0 && isspace ((int) cp [6]))
		{
			cp += 6;
			while (isspace ((int) *cp))
			++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);++cp;	}
			vStringTerminate (name);
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			cm=strdup(name->buffer );
			MyMakeTag (name, VBKinds, K_MODULE,0,0,0);
			vStringClear (name);
		}
		else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0 && isspace ((int) cp [5]))
		{
			cp += 5;
			while (isspace ((int) *cp))
			++cp;
			vStringClear (name);
			while (isalnum ((int) *cp)  ||  *cp == '_')	{vStringPut (name, (int) *cp);++cp;	}
			vStringTerminate (name);
			if(name->buffer[0]==0){vStringPut (name,'?');vStringTerminate(name);}
			cc=strdup(name->buffer );
			if(cm)
			{
				char * tmp=strdup(cm);
				tmp=strcat(tmp,".");cc=strcat(tmp,cc);
			}
			MyMakeTag (name, VBKinds, K_CLASS,0,cm?"module":0,cm);
			vStringClear (name);
		}			
	}
	if(line)free(line);
    vStringDelete (name);
}

extern parserDefinition* VBParser (void)
{
    static const char *const extensions [] = { "vb", "bas", NULL };
    parserDefinition* def = parserNew ("VB.Net");
    def->kinds      = VBKinds;
    def->kindCount  = KIND_COUNT (VBKinds);
    def->extensions = extensions;
    def->parser     = findVBTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
