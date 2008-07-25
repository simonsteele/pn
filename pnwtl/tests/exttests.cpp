#include "stdafx.h"

#include <assert.h>
#define PNASSERT assert

#include "../allocator.h"
#include "../pnextstring.h"

using namespace PN;

/**
 * Test the extensions string class
 */
void testString()
{
	AsciiString< BasicAllocator<char> > theString("blah");
	AString theStringCake("cheesecake");

	// Get
	TESTASSERT( ((const char*)theString) != NULL );
	TESTASSERT( strlen((const char*)theString) == 4 );

	// GetLength
	TESTASSERT( theString.GetLength() == 4 );

	// == 
	TESTASSERT( theString == "blah" );
	TESTASSERT( theString == theString );

	// !=
	TESTASSERT( theString != "RRRRRRR" );
	TESTASSERT( theString != theStringCake );
	TESTASSERT( !(theString != theString) );

	// Set
	theString.Set( "Monkey" );
	TESTASSERT( theString == "Monkey" );

	// Add
	theString.Add( "Fish" );
	TESTASSERT( theString == "MonkeyFish" );

	// Reset
	theString.Reset();
	TESTASSERT( theString == "" );
	TESTASSERT( theString.GetLength() == 0 );

	// Empty
	TESTASSERT( theString.Empty() );
	theString = "test";
	TESTASSERT( !theString.Empty() );
	
	// = char *
	TESTASSERT( (theString = "bananananana") == "bananananana" );
	
	// = AsciiString
	TESTASSERT( (theString = theStringCake) == "cheesecake" );

	// += char *
	TESTASSERT( (theString += "y") == "cheesecakey" );

	// += AsciiString
	TESTASSERT( (theString += theStringCake) == "cheesecakeycheesecake" );
	
	// Reset()
	theString.Reset();
	TESTASSERT( theString == "" );
	TESTASSERT( theString.Empty() );

	// Lock/UnlockBuffer
	char* buf = theString.LockBuffer(20);
	strcpy(buf, "shorter than 20");
	theString.UnlockBuffer();
	TESTASSERT( theString.GetLength() == 15 );
	TESTASSERT( theString == "shorter than 20" );
	
	// Truncate
	theString = "shorter";
	theString.Truncate(5);
	TESTASSERT( theString.GetLength() == 5 );
	TESTASSERT( theString == "short" );

	// LocalAlloc Allocator - cross allocator conversions
	AsciiString< LocalAllocAllocator<char> > string2;
	string2 = theString;
	TESTASSERT( theString == string2 );
	TESTASSERT( string2 == theString );
	TESTASSERT( string2 == "short" );

	// Long String
	TESTASSERT( (theString = "blahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblah").GetLength() == 96 );
}