#include "stdafx.h"

#include <boost/test/unit_test.hpp>

#include "../filename.h"
#include "../projectmeta.h"

BOOST_AUTO_TEST_SUITE( userdata_tests );

BOOST_AUTO_TEST_CASE( namespace_parsed_in_constructor )
{
	Projects::XmlNode node(_T("urn:test:test"));

	BOOST_REQUIRE(node.Matches(_T("urn:test"), _T("test")));
	BOOST_REQUIRE(node.Matches(_T("urn:test"), _T("monkey")) == false);
	BOOST_REQUIRE(node.Matches(_T("urn:test2"), _T("test")) == false);
}

BOOST_AUTO_TEST_SUITE_END();