#include "stdafx.h"

#include <boost/test/unit_test.hpp>

#include "../include/Utf8_16.h"

BOOST_AUTO_TEST_SUITE( io_tests );

BOOST_AUTO_TEST_CASE( opening_empty_utf16_file_with_bom_has_no_data )
{
	char data[2] = { Utf8_16::k_Boms[Utf8_16::eUtf16LittleEndian][0], Utf8_16::k_Boms[Utf8_16::eUtf16LittleEndian][1] };

	Utf8_16::encodingType encodingType(Utf8_16::eUtf16LittleEndian);
	int lenFile(2);
	int nBomSkipBytes(2);
	Utf8_16_Read converter;
	
	lenFile = converter.convert(&data[0], lenFile, encodingType, nBomSkipBytes);
	
	BOOST_CHECK_EQUAL(lenFile, 0);
}

BOOST_AUTO_TEST_CASE( opening_empty_utf8_file_with_bom_has_no_data )
{
	char data[3] = { Utf8_16::k_Boms[Utf8_16::eUtf8][0], Utf8_16::k_Boms[Utf8_16::eUtf8][1], Utf8_16::k_Boms[Utf8_16::eUtf8][2] };

	Utf8_16::encodingType encodingType(Utf8_16::eUtf8);
	int lenFile(3);
	int nBomSkipBytes(3);
	Utf8_16_Read converter;
	
	lenFile = converter.convert(&data[0], lenFile, encodingType, nBomSkipBytes);
	
	BOOST_CHECK_EQUAL(lenFile, 0);
}

// Bug: http://code.google.com/p/pnotepad/issues/detail?id=592&start=200
BOOST_AUTO_TEST_CASE( converting_threebyte_utf8_char_works )
{
	unsigned char data[3] = { 0xe2, 0x9f, 0xa0 };

	Utf8_16::encodingType encodingType(Utf8_16::eUtf16LittleEndian);
	Utf8_Iter iter;
	iter.reset();
	iter.set(&data[0], sizeof(data), encodingType);
	++iter;
	++iter;
	DWORD dwUtf16 = iter.get();
	
	BOOST_CHECK_EQUAL(0x27e0, dwUtf16);
}

BOOST_AUTO_TEST_SUITE_END();