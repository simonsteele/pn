#ifndef tagtypes_h__included
#define tagtypes_h__included

typedef struct maskType
{
	int mask1;		// Maskbit 0..31
	int mask2;		// Maskbit 32..63
} MASKSTRUCT;

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
#define TAG_ATTRIBUTE   36
#define TAG_COMPONENT   37
#define TAG_PACKAGE     38
#define TAG_ENTITY      39
#define TAG_ARCHITECTURE 40
#define TAG_PROCESS		41

#define TAG_MAX         41

// TAG type mask table
static MASKSTRUCT maskVals[TAG_MAX+1] = {
	{1<<0,	0},		//TAG_UNKNOWN     0
	{1<<1,	0},		//TAG_FUNCTION    1
	{1<<2,	0},		//TAG_PROCEDURE   2
	{1<<3,	0},		//TAG_CLASS       3
	{1<<4,	0},		//TAG_MACRO       4
	{1<<5,	0},		//TAG_ENUM        5
	{1<<6,	0},		//TAG_FILENAME    6
	{1<<7,	0},		//TAG_ENUMNAME    7
	{1<<8,	0},		//TAG_MEMBER      8
	{1<<9,	0},		//TAG_PROTOTYPE   9
	{1<<10,	0},		//TAG_STRUCTURE   10
	{1<<11,	0},		//TAG_TYPEDEF     11
	{1<<12,	0},		//TAG_UNION       12
	{1<<13,	0},		//TAG_VARIABLE    13
	{1<<14,	0},		//TAG_NAMESPACE   14
	{1<<15,	0},		//TAG_METHOD      15
	{1<<16,	0},		//TAG_EVENT       16
	{1<<17,	0},		//TAG_INTERFACE   17
	{1<<18,	0},		//TAG_PROPERTY    18
	{1<<19,	0},		//TAG_PROGRAM     19
	{1<<20,	0},		//TAG_CONSTANT    20
	{1<<21,	0},		//TAG_LABEL       21
	{1<<22,	0},		//TAG_SINGLETON   22
	{1<<23,	0},		//TAG_MIXIN       23
	{1<<24,	0},		//TAG_MODULE      24
	{1<<25,	0},		//TAG_NET         25
	{1<<26,	0},		//TAG_PORT        26
	{1<<27,	0},		//TAG_REGISTER    27
	{1<<28,	0},		//TAG_TASK        28
	{1<<29,	0},		//TAG_CURSOR      29
	{1<<30,	0},		//TAG_RECORD      30
	{1<<31,	0},		//TAG_SUBTYPE     31
	{0,		1<<0},	//TAG_TRIGGER     32
	{0,		1<<1},	//TAG_SET         33
	{0,		1<<2},	//TAG_FIELD       34
	{0,		1<<3},	//TAG_TABLE       35
	{0,		1<<4},	//TAG_ATTRIBUTE   36
	{0,		1<<5},	//TAG_COMPONENT   37
	{0,		1<<6},	//TAG_PACKAGE     38
	{0,		1<<7},	//TAG_ENTITY      39
	{0,		1<<8},	//TAG_ARCHITECTURE 40
	{0,		1<<9},	//TAG_PROCESS		41
};
//[[[end]]]

#endif //#ifndef tagtypes_h__included