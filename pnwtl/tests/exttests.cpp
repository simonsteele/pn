#include "stdafx.h"

#include <boost/test/unit_test.hpp>

#include "../allocator.h"
#include "../pnextstring.h"

using namespace PN;

typedef AsciiString< BasicAllocator<char> > astring;

BOOST_AUTO_TEST_SUITE( extension_tests );

/**
 * Test the extensions string class
 */
BOOST_AUTO_TEST_CASE( test_string_charstar_cast )
{
	AsciiString< BasicAllocator<char> > theString("blah");
	AString theStringCake("cheesecake");

	// Get
	BOOST_CHECK( ((const char*)theString) != NULL );
	BOOST_CHECK( strlen((const char*)theString) == 4 );
}

BOOST_AUTO_TEST_CASE( test_string_get_length )
{
	astring theString("test");

	// GetLength
	BOOST_CHECK( theString.GetLength() == 4 );
}

BOOST_AUTO_TEST_CASE( test_string_isequalto )
{
	astring theString("blah");

	// == 
	BOOST_CHECK( theString == "blah" );
	BOOST_CHECK( theString == theString );
}

BOOST_AUTO_TEST_CASE( test_string_notequalto )
{
	astring theString("test_string_notequalto");
	astring theStringCake("cheesecake");

	// !=
	BOOST_CHECK( theString != "RRRRRRR" );
}

BOOST_AUTO_TEST_CASE( test_string_notequalto_otherstring )
{
	astring theString("test_string_notequalto");
	astring theStringCake("cheesecake");

	// !=
	BOOST_CHECK( theString != theStringCake );
}

BOOST_AUTO_TEST_CASE( test_string_set )
{
	astring theString;

	// Set
	theString.Set( "Monkey" );
	BOOST_CHECK( theString == "Monkey" );
}

BOOST_AUTO_TEST_CASE( test_string_add )
{
	astring theString("Monkey");

	// Add
	theString.Add( "Fish" );
	BOOST_CHECK( theString == "MonkeyFish" );
}

BOOST_AUTO_TEST_CASE( test_string_reset )
{
	astring theString("Monkey");

	// Reset
	theString.Reset();
	BOOST_CHECK( theString == "" );
	BOOST_CHECK( theString.GetLength() == 0 );
}

BOOST_AUTO_TEST_CASE( test_string_empty )
{
	astring theString;

	// Empty
	BOOST_CHECK( theString.Empty() );
	theString = "test";
	BOOST_CHECK( !theString.Empty() );
}

BOOST_AUTO_TEST_CASE( test_string_equals_charstar )
{
	astring theString;

	// = char *
	BOOST_CHECK( (theString = "bananananana") == "bananananana" );
}

BOOST_AUTO_TEST_CASE( test_string_equals_otherstring )
{
	astring theString;
	astring theStringCake("cheesecake");

	// = AsciiString
	BOOST_CHECK( (theString = theStringCake) == "cheesecake" );
}

BOOST_AUTO_TEST_CASE( test_string_plusequals )
{
	astring theString("cheesecake");
	
	// += char *
	BOOST_CHECK( (theString += "y") == "cheesecakey" );
}

BOOST_AUTO_TEST_CASE( test_string_plusequals_otherstring )
{
	astring theString("cheesecakey");
	astring theStringCake("cheesecake");

	// += AsciiString
	BOOST_CHECK( (theString += theStringCake) == "cheesecakeycheesecake" );
}

BOOST_AUTO_TEST_CASE( test_string_lock_buffer )
{
	astring theString;

	// Lock/UnlockBuffer
	char* buf = theString.LockBuffer(20);
	strcpy(buf, "shorter than 20");
	theString.UnlockBuffer();
	BOOST_CHECK( theString.GetLength() == 15 );
	BOOST_CHECK( theString == "shorter than 20" );
}

BOOST_AUTO_TEST_CASE( test_string_truncate )
{
	// Truncate
	astring theString = "shorter";
	theString.Truncate(5);
	BOOST_CHECK( theString.GetLength() == 5 );
	BOOST_CHECK( theString == "short" );
}

BOOST_AUTO_TEST_CASE( test_string_localalloc )
{
	astring theString("short");

	// LocalAlloc Allocator - cross allocator conversions
	AsciiString< LocalAllocAllocator<char> > string2;
	string2 = theString;
	BOOST_CHECK( theString == string2 );
	BOOST_CHECK( string2 == theString );
	BOOST_CHECK( string2 == "short" );
}

BOOST_AUTO_TEST_CASE( test_string_with_longtext )
{
	astring theString;

	// Long String
	BOOST_CHECK( (theString = "blahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblahblah").GetLength() == 96 );
}

BOOST_AUTO_TEST_SUITE_END();