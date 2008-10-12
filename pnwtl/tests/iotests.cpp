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

BOOST_AUTO_TEST_SUITE_END();