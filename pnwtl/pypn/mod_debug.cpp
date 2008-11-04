#include "stdafx.h"

void pyOutputDebugString(const char* message)
{
	OutputDebugString(message);
}

using namespace boost::python;

BOOST_PYTHON_MODULE(debug)
{
	boost::python::docstring_options docstring_options(true);

	def("OutputDebugString", pyOutputDebugString);
}