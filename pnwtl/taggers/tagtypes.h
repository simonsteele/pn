#ifndef tagtypes_h__included
#define tagtypes_h__included

/*[[[cog
tagnames = [
	'Unknown', 
	'Function', 
	'Procedure',
	'Class',
	'Macro',
	'Enum',
	'Filename',
	'EnumName',
	'Member',
	'Prototype',
	'Structure',
	'Typedef',
	'Union',
	'Variable',
	'Namespace',
	'Method',
	'Event',
	'Interface',
	'Property',
	'Program',
	'Constant',
	'Label',
	'Singleton',
	'Mixin',
	'Module',
	'Net',
	'Port',
	'Register',
	'Task',
	'Cursor',
	'Record',
	'SubType',
	'Trigger',
	'Set',
	'Field',
	'Table',
]
import tagsdefines
tagsdefines.showDefines(tagnames)
]]]*/
// TAG type defines
#define TAG_UNKNOWN     0
#define TAG_FUNCTION    1
#define TAG_PROCEDURE   2
#define TAG_CLASS       3
#define TAG_MACRO       4
#define TAG_ENUM        5
#define TAG_FILENAME    6
#define TAG_ENUMNAME    7
#define TAG_MEMBER      8
#define TAG_PROTOTYPE   9
#define TAG_STRUCTURE   10
#define TAG_TYPEDEF     11
#define TAG_UNION       12
#define TAG_VARIABLE    13
#define TAG_NAMESPACE   14
#define TAG_METHOD      15
#define TAG_EVENT       16
#define TAG_INTERFACE   17
#define TAG_PROPERTY    18
#define TAG_PROGRAM     19
#define TAG_CONSTANT    20
#define TAG_LABEL       21
#define TAG_SINGLETON   22
#define TAG_MIXIN       23
#define TAG_MODULE      24
#define TAG_NET         25
#define TAG_PORT        26
#define TAG_REGISTER    27
#define TAG_TASK        28
#define TAG_CURSOR      29
#define TAG_RECORD      30
#define TAG_SUBTYPE     31
#define TAG_TRIGGER     32
#define TAG_SET         33
#define TAG_FIELD       34
#define TAG_TABLE       35

#define TAG_MAX         35

// TAG type mask definitions
#define TAGM_UNKNOWN    1
#define TAGM_FUNCTION   1 << 1
#define TAGM_PROCEDURE  1 << 2
#define TAGM_CLASS      1 << 3
#define TAGM_MACRO      1 << 4
#define TAGM_ENUM       1 << 5
#define TAGM_FILENAME   1 << 6
#define TAGM_ENUMNAME   1 << 7
#define TAGM_MEMBER     1 << 8
#define TAGM_PROTOTYPE  1 << 9
#define TAGM_STRUCTURE  1 << 10
#define TAGM_TYPEDEF    1 << 11
#define TAGM_UNION      1 << 12
#define TAGM_VARIABLE   1 << 13
#define TAGM_NAMESPACE  1 << 14
#define TAGM_METHOD     1 << 15
#define TAGM_EVENT      1 << 16
#define TAGM_INTERFACE  1 << 17
#define TAGM_PROPERTY   1 << 18
#define TAGM_PROGRAM    1 << 19
#define TAGM_CONSTANT   1 << 20
#define TAGM_LABEL      1 << 21
#define TAGM_SINGLETON  1 << 22
#define TAGM_MIXIN      1 << 23
#define TAGM_MODULE     1 << 24
#define TAGM_NET        1 << 25
#define TAGM_PORT       1 << 26
#define TAGM_REGISTER   1 << 27
#define TAGM_TASK       1 << 28
#define TAGM_CURSOR     1 << 29
#define TAGM_RECORD     1 << 30
#define TAGM_SUBTYPE    1 << 31
#define TAGM_TRIGGER    1 << 32
#define TAGM_SET        1 << 33
#define TAGM_FIELD      1 << 34
#define TAGM_TABLE      1 << 35

// TAG type mask table
static int maskVals[36] = {
	TAGM_UNKNOWN,
	TAGM_FUNCTION,
	TAGM_PROCEDURE,
	TAGM_CLASS,
	TAGM_MACRO,
	TAGM_ENUM,
	TAGM_FILENAME,
	TAGM_ENUMNAME,
	TAGM_MEMBER,
	TAGM_PROTOTYPE,
	TAGM_STRUCTURE,
	TAGM_TYPEDEF,
	TAGM_UNION,
	TAGM_VARIABLE,
	TAGM_NAMESPACE,
	TAGM_METHOD,
	TAGM_EVENT,
	TAGM_INTERFACE,
	TAGM_PROPERTY,
	TAGM_PROGRAM,
	TAGM_CONSTANT,
	TAGM_LABEL,
	TAGM_SINGLETON,
	TAGM_MIXIN,
	TAGM_MODULE,
	TAGM_NET,
	TAGM_PORT,
	TAGM_REGISTER,
	TAGM_TASK,
	TAGM_CURSOR,
	TAGM_RECORD,
	TAGM_SUBTYPE,
	TAGM_TRIGGER,
	TAGM_SET,
	TAGM_FIELD,
	TAGM_TABLE,
};
//[[[end]]]

#endif //#ifndef tagtypes_h__included