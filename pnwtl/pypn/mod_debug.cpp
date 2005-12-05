#include "stdafx.h"

void pyOutputDebugString(const char* message)
{
	OutputDebugString(message);
}

using namespace boost::python;

BOOST_PYTHON_MODULE(debug)
{
	def("OutputDebugString", pyOutputDebugString);
}