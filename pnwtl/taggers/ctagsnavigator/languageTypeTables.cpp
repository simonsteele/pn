/**
 * @file languageTypeTables.cpp
 * @brief Type mappings.
 * @author Simon Steele, Ryan Mulder
 * @note Copyright (c) 2004-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
 
#include "stdafx.h"
#include "languageTypeTables.h"
#include "../tagtypes.h"

#include <map>
#include <string>
#include <vector>

// Map for tables loaded from an external file
typedef std::map< std::string, std::pair< int*, int* > > ExternalTableT;
static ExternalTableT s_externalTables;

/*
static kindOption CKinds [] = {
    { TRUE,  'c', "class",      "classes"},
    { TRUE,  'd', "macro",      "macro definitions"},
    { TRUE,  'e', "enumerator", "enumerators (values inside an enumeration)"},
    { TRUE,  'f', "function",   "function definitions"},
    { TRUE,  'g', "enum",       "enumeration names"},
    { FALSE, 'l', "local",      "local variables"},
    { TRUE,  'm', "member",     "class, struct, and union members"},
    { TRUE,  'n', "namespace",  "namespaces"},
    { FALSE, 'p', "prototype",  "function prototypes"},
    { TRUE,  's', "struct",     "structure names"},
    { TRUE,  't', "typedef",    "typedefs"},
    { TRUE,  'u', "union",      "union names"},
    { TRUE,  'v', "variable",   "variable definitions"},
    { FALSE, 'x', "externvar",  "external variable declarations"},
};
 */

int lCTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ TAG_MACRO,
	/*e*/ TAG_ENUM,		/*f*/ TAG_FUNCTION,
	/*g*/ TAG_ENUMNAME,	/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_VARIABLE, /*local*/
	/*m*/ TAG_MEMBER,	/*n*/ TAG_NAMESPACE,
	/*o*/ 0,			/*p*/ TAG_PROTOTYPE,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_STRUCTURE,/*t*/ TAG_TYPEDEF,
	/*u*/ TAG_UNION,	/*v*/ TAG_VARIABLE,
	/*w*/ 0,			/*x*/ TAG_VARIABLE, /*extern*/
	/*y*/ 0,			/*z*/ 0
};

int uCTypes[26] = {
	/*A*/ 0,			/*B*/ 0,
	/*C*/ 0,			/*D*/ 0,
	/*E*/ 0,			/*F*/ TAG_FILENAME,
	/*G*/ 0,			/*H*/ 0,
	/*I*/ 0,			/*J*/ 0,
	/*K*/ 0,			/*L*/ 0,
	/*M*/ 0,			/*N*/ 0,
	/*O*/ 0,			/*P*/ 0,
	/*Q*/ 0,			/*R*/ 0,
	/*S*/ 0,			/*T*/ 0,
	/*U*/ 0,			/*V*/ 0,
	/*W*/ 0,			/*X*/ 0,
	/*Y*/ 0,			/*Z*/ 0
};

/*
static kindOption CsharpKinds [] = {
    { TRUE,  'c', "class",      "classes"},
    { TRUE,  'd', "macro",      "macro definitions"},
    { TRUE,  'e', "enumerator", "enumerators (values inside an enumeration)"},
    { TRUE,  'E', "event",      "events"},
    { TRUE,  'f', "field",      "fields"},
    { TRUE,  'g', "enum",       "enumeration names"},
    { TRUE,  'i', "interface",  "interfaces"},
    { FALSE, 'l', "local",      "local variables"},
    { TRUE,  'm', "method",     "methods"},
    { TRUE,  'n', "namespace",  "namespaces"},
    { TRUE,  'p', "property",   "properties"},
    { TRUE,  's', "struct",     "structure names"},
    { TRUE,  't', "typedef",    "typedefs"},
};
*/

int lCSTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ TAG_MACRO,
	/*e*/ TAG_ENUM,		/*f*/ TAG_MEMBER,
	/*g*/ TAG_ENUMNAME,	/*h*/ 0,
	/*i*/ TAG_INTERFACE,/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_VARIABLE, /*local*/
	/*m*/ TAG_METHOD,	/*n*/ TAG_NAMESPACE,
	/*o*/ 0,			/*p*/ TAG_PROPERTY,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_STRUCTURE,/*t*/ TAG_TYPEDEF,
	/*u*/ 0,	/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

int uCSTypes[26] = {
	/*A*/ 0,			/*B*/ 0,
	/*C*/ 0,			/*D*/ 0,
	/*E*/ TAG_EVENT,	/*F*/ TAG_FILENAME,
	/*G*/ 0,			/*H*/ 0,
	/*I*/ 0,			/*J*/ 0,
	/*K*/ 0,			/*L*/ 0,
	/*M*/ 0,			/*N*/ 0,
	/*O*/ 0,			/*P*/ 0,
	/*Q*/ 0,			/*R*/ 0,
	/*S*/ 0,			/*T*/ 0,
	/*U*/ 0,			/*V*/ 0,
	/*W*/ 0,			/*X*/ 0,
	/*Y*/ 0,			/*Z*/ 0
};

/*static kindOption JavaKinds [] = {
    { TRUE,  'c', "class",     "classes"},
    { TRUE,  'f', "field",     "fields"},
    { TRUE,  'i', "interface", "interfaces"},
    { FALSE, 'l', "local",     "local variables"},
    { TRUE,  'm', "method",    "methods"},
    { TRUE,  'p', "package",   "packages"},
};*/

int lJTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_MEMBER,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ TAG_INTERFACE,/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_VARIABLE, /*local*/
	/*m*/ TAG_METHOD,	/*n*/ 0,
	/*o*/ 0,			/*p*/ TAG_NAMESPACE, /*actually a package...*/
	/*q*/ 0,			/*r*/ 0,
	/*s*/ 0,			/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption PerlKinds [] = {
    { TRUE, 'c', "constant",   "constants" },
    { TRUE, 'l', "label",      "labels" },
    { TRUE, 's', "subroutine", "subroutines" }
};*/

int lPerlTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CONSTANT,	/*d*/ 0,
	/*e*/ 0,			/*f*/ 0,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_LABEL,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ 0,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_FUNCTION,	/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption PascalKinds [] = {
    { TRUE, 'f', "function",  "functions"},
    { TRUE, 'p', "procedure", "procedures"}
};*/

int lPascalTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ 0,			/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_FUNCTION,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ TAG_PROCEDURE,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ 0,			/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption RubyKinds [] = {
    { TRUE, 'c', "class",  "classes" },
    { TRUE, 'f', "method", "methods" },
    { TRUE, 'F', "singleton method", "singleton methods" },
    { TRUE, 'm', "mixin",  "mixins" }
};*/

int lRubyTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_METHOD,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ TAG_MIXIN,	/*n*/ 0,
	/*o*/ 0,			/*p*/ 0,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ 0,			/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

int uRubyTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ 0,			/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_SINGLETON,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ 0,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ 0,			/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption SchemeKinds [] = {
    { TRUE, 'f', "function", "functions" },
    { TRUE, 's', "set",      "sets" }
};*/

int lSchemeTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ 0,			/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_FUNCTION,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ 0,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_SET,		/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption SqlKinds [] = {
    { TRUE,  'c', "cursor",    "cursors"	},
    { FALSE, 'd', "prototype", "prototypes"	},
    { TRUE,  'f', "function",  "functions"	},
    { TRUE,  'F', "field",     "record fields"	},
    { FALSE, 'l', "local",     "local variables"},
    { TRUE,  'L', "label",     "block label"    },
    { TRUE,  'P', "package",   "packages"	},
    { TRUE,  'p', "procedure", "procedures"	},
    { TRUE,  'r', "record",    "records"	},
    { TRUE,  's', "subtype",   "subtypes"	},
    { TRUE,  't', "table",     "tables"		},
    { TRUE,  'T', "trigger",   "triggers"	},
    { TRUE,  'v', "variable",  "variables"	},
};*/

int lSQLTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CURSOR,	/*d*/ TAG_PROTOTYPE,
	/*e*/ 0,			/*f*/ TAG_FUNCTION,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_VARIABLE,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ TAG_PROCEDURE,
	/*q*/ 0,			/*r*/ TAG_RECORD,
	/*s*/ TAG_SUBTYPE,	/*t*/ TAG_TABLE,
	/*u*/ 0,			/*v*/ TAG_VARIABLE,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

int uSQLTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ 0,			/*d*/ 0,
	/*e*/ 0,			/*f*/ TAG_FIELD,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ TAG_LABEL,
	/*m*/ 0,			/*n*/ 0,
	/*o*/ 0,			/*p*/ TAG_MODULE, /* actually package */
	/*q*/ 0,			/*r*/ 0,
	/*s*/ 0,			/*t*/ TAG_TRIGGER,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption VerilogKinds [] = {
 { TRUE, 'c', "constant",  "constants (define, parameter, specparam)" },
 { TRUE, 'e', "event",     "events" },
 { TRUE, 'f', "function",  "functions" },
 { TRUE, 'm', "module",    "modules" },
 { TRUE, 'n', "net",       "net data types" },
 { TRUE, 'p', "port",      "ports" },
 { TRUE, 'r', "register",  "register data types" },
 { TRUE, 't', "task",      "tasks" }
};*/

int lVerilogTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CONSTANT,	/*d*/ 0,
	/*e*/ TAG_EVENT,	/*f*/ TAG_FUNCTION,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ TAG_MODULE,	/*n*/ TAG_NET,
	/*o*/ 0,			/*p*/ TAG_PORT,
	/*q*/ 0,			/*r*/ TAG_REGISTER,
	/*s*/ 0,			/*t*/ TAG_TASK,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*
static kindOption VBKinds [] = {
{ TRUE, 'c', "class", "classes" },
{ TRUE, 'd', "dim", "variables" },
{ TRUE, 'f', "function", "functions" },
{ TRUE, 'k', "const", "constants" },
{ TRUE, 'm', "module", "modules" },
{ TRUE, 's', "sub", "procedures" },
};*/

int lVBTypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ TAG_FIELD,
	/*e*/ 0,			/*f*/ TAG_FUNCTION,
	/*g*/ 0,			/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ TAG_CONSTANT,	/*l*/ 0,
	/*m*/ TAG_MODULE,	/*n*/ 0,
	/*o*/ 0,			/*p*/ 0,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_PROCEDURE,/*t*/ 0,
	/*u*/ 0,			/*v*/ 0,
	/*w*/ 0,			/*x*/ 0, 
	/*y*/ 0,			/*z*/ 0
};

/*static kindOption VhdlKinds [] = {
 { TRUE, 'c', "constant",     "constants" },
 { TRUE, 't', "type",         "types" },
 { TRUE, 'v', "variable",     "variables" },
 { TRUE, 'a', "attribute",    "attributes" },
 { TRUE, 's', "signal",       "signals" },
 { TRUE, 'f', "function",     "functions" },
 { TRUE, 'p', "procedure",    "procedure" },
 { TRUE, 'k', "component",    "components" },
 { TRUE, 'l', "package",      "packages" },
 { TRUE, 'm', "process",      "process" },
 { TRUE, 'n', "entity",       "entity" },
 { TRUE, 'o', "architecture", "architecture" },
 { TRUE, 'u', "port",         "ports" }
};*/

int lVhdlTypes[26] = {
	/*a*/ TAG_ATTRIBUTE,	/*b*/ 0,
	/*c*/ TAG_CONSTANT,		/*d*/ 0,
	/*e*/ 0,				/*f*/ TAG_FUNCTION,
	/*g*/ 0,				/*h*/ 0,
	/*i*/ 0,				/*j*/ 0,
	/*k*/ TAG_COMPONENT,	/*l*/ TAG_PACKAGE,
	/*m*/ TAG_PROCESS,		/*n*/ TAG_ENTITY,
	/*o*/ TAG_ARCHITECTURE, /*p*/ TAG_PROCEDURE,
	/*q*/ 0,				/*r*/ 0,
	/*s*/ TAG_NET,			/*t*/ TAG_TYPEDEF,
	/*u*/ TAG_PORT,			/*v*/ TAG_VARIABLE,
	/*w*/ 0,				/*x*/ 0, 
	/*y*/ 0,				/*z*/ 0
};

int lLuaTypes[26] = {
	/*a*/ 0,				/*b*/ 0,
	/*c*/ 0,				/*d*/ 0,
	/*e*/ 0,				/*f*/ TAG_FUNCTION,
	/*g*/ 0,				/*h*/ 0,
	/*i*/ 0,				/*j*/ 0,
	/*k*/ 0,				/*l*/ 0,
	/*m*/ 0,				/*n*/ 0,
	/*o*/ 0,				/*p*/ 0,
	/*q*/ 0,				/*r*/ 0,
	/*s*/ 0,				/*t*/ 0,
	/*u*/ 0,				/*v*/ 0,
	/*w*/ 0,				/*x*/ 0, 
	/*y*/ 0,				/*z*/ 0
};

/*
the following are satisfied by the c defaults:
php, python, tcl
*/

void getTables(const char* schemeName, int** lcTypes, int** ucTypes)
{
	// default uppercase - not many use this.
	*ucTypes = uCTypes;

	if(strcmp(schemeName, "csharp") == 0)
	{
		*lcTypes = lCSTypes;
		*ucTypes = uCSTypes;
	}
	else if(strcmp(schemeName, "java") == 0)
	{
		*lcTypes = lJTypes;
	}
	else if(strcmp(schemeName, "pascal") == 0)
	{
		*lcTypes = lPascalTypes;
	}
	else if(strcmp(schemeName, "perl") == 0)
	{
		*lcTypes = lPerlTypes;
	}
	else if(strcmp(schemeName, "ruby") == 0)
	{
		*lcTypes = lRubyTypes;
		*ucTypes = uRubyTypes;
	}
	else if(strcmp(schemeName, "sql") == 0)
	{
		*lcTypes = lSQLTypes;
		*ucTypes = uSQLTypes;
	}
	else if(strcmp(schemeName, "vb") == 0)
	{
		*lcTypes = lVBTypes;
	}
	else if(strcmp(schemeName, "verilog") == 0)
	{
		*lcTypes = lVerilogTypes;
	}
	else if(strcmp(schemeName, "vhdl") == 0)
	{
		*lcTypes = lVhdlTypes;
	}
	else if(strcmp(schemeName, "lua") == 0)
	{
		*lcTypes = lLuaTypes;
	}
	else if(s_externalTables.count( schemeName ) != 0)
	{
		ExternalTableT::mapped_type* table = &s_externalTables[schemeName];
		*lcTypes = table->first;
		*ucTypes = table->second;
	}
	else
	{
		*lcTypes = lCTypes;
	}
}

// This class is used to automatically clean up dynamically allocated tables
class TablePtrHolder
{
public:
	std::vector< int* > m_ptrs;

	~TablePtrHolder()
	{
		for ( size_t i = 0; i < m_ptrs.size(); ++i )
		{
			delete [] m_ptrs[i];
		}
	}
};

static TablePtrHolder s_tablePtrHolder;

/** 
 * Determines the address of the first character of each string and stores that address into the vector "pointers".
 * @param	buffer		[IN]	Contains multiple strings separated by NULL, with an extra NULL at the end
 *  							Example: "Tom\0Bill\0Bob\0Sue\0Jenny\0John\0\0"
 * @param	pointers	[OUT]	Vector to hold pointers to the individual strings
 */
void ParseNullSeparatedStringBuffer( const char* buffer, std::vector< const char* >* pointers)
{	
	// Used to store the pointer to the next string
	const char* nextString = buffer;

	// Used to store the last valid pointer
	const char* lastString;

	//Fill the array with string pointers
	while( *nextString != '\0' )
	{
		//Store the pointer to the first string
		(*pointers).push_back( nextString );
		
		// Use the starting point and length of the last string 
		// to determine the start of the next string
		lastString = (*pointers)[ (*pointers).size() - 1 ];
		nextString = lastString + strlen( lastString ) + 1;
	}

	// No More Strings
	return;
}

std::string WcsTo1252(const wchar_t* str)
{
	int len = WideCharToMultiByte(CP_ACP, 0, str, -1, NULL, 0, NULL, NULL);
	std::string ret;
	ret.resize(len);
	len = WideCharToMultiByte(CP_ACP, 0, str, -1, &ret[0], len, NULL, NULL);
	ret.resize(len);
	return ret;
}

void loadExternalTables(const wchar_t* fileName, std::string* moreSchemes)
{
	// Get all sections from the .ini file
	char allSections[2000];
	std::string fileNameA(WcsTo1252(fileName));
	::GetPrivateProfileSectionNamesA(allSections, sizeof( allSections ), fileNameA.c_str());

	std::vector< const char* > sections;
	ParseNullSeparatedStringBuffer( allSections, &sections );

	// used for section data
	char data[2000];
	std::vector< const char* > lines;
	std::vector< const char* >::const_iterator line;

	std::vector< const char* >::const_iterator section;
	for ( section = sections.begin(); section != sections.end(); ++section )
	{
		// Get all key/value pairs from section
		::GetPrivateProfileSectionA( *section, data, sizeof( data ), fileNameA.c_str() );
		ParseNullSeparatedStringBuffer( data, &lines );

		// Tables for language type data
		int* lTable = new int[26];
		memset( lTable, 0, 26 * sizeof(int) );
		
		int* uTable = new int[26];
		memset( uTable, 0, 26 * sizeof(int) );
		
		for ( line = lines.begin(); line != lines.end(); ++line )
		{
			const char* entry = *line;

			if ( strlen( entry ) < 3 )
			{
				continue;
			}

			// Only single letter keys are accepted
			if ( entry[1] != '=' )
			{
				continue;
			}

			// The key must be in a-z or A-Z
			char key = entry[0];
			if ( key >= 'a' && key <= 'z' )
			{
				lTable[key - 'a'] = ::atoi( entry + 2 );
			}

			if ( key >= 'A' && key <= 'Z' )
			{
				uTable[key - 'A'] = ::atoi( entry + 2 );
			}		
		}
		
		lines.clear();

		// Remember pointers to delete later
		s_tablePtrHolder.m_ptrs.push_back( lTable );
		s_tablePtrHolder.m_ptrs.push_back( uTable );

		s_externalTables[ *section ] = std::make_pair( lTable, uTable );
		*moreSchemes += ";";
		*moreSchemes += *section;
	}
}
