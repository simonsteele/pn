#include "stdafx.h"
#include <boost/xpressive/xpressive.hpp>
using namespace boost::xpressive;

void testRegex()
{
	// At the time of writing this causes an exception due to stepping over the end of the string:
	std::string s("> \"\" ");
	std::string expression("(?P<f>.+):(?P<l>[0-9]+):((?P<c>[0-9]+):)?.*");

	sregex re = sregex::compile(expression);

	regex_search(s, re);
}