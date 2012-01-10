#include <stdafx.h>
#include "dos_wildcard.h"

// TODO: Replace with xpressive
#include <boost/regex.hpp>

namespace PN { namespace IO {

class dos_wildcard_implementation
{
public:
   boost::regex m_expression;
};

dos_wildcard::dos_wildcard()
{
}

dos_wildcard::dos_wildcard(const char* wild, bool ignore_case)
{
   assign(wild, ignore_case);
}

dos_wildcard::dos_wildcard(const std::string& wild, bool ignore_case)
{
   assign(wild, ignore_case);
}

dos_wildcard::dos_wildcard(const dos_wildcard& wild)
: m_pimpl(wild.m_pimpl)
{
}

dos_wildcard::~dos_wildcard()
{
}

// assign:
dos_wildcard& dos_wildcard::operator=(const char* wild)
{
   assign(wild);
   return *this;
}

dos_wildcard& dos_wildcard::operator=(const std::string& wild)
{
   assign(wild);
   return *this;
}

dos_wildcard& dos_wildcard::operator=(const dos_wildcard& wild)
{
   m_pimpl = wild.m_pimpl;
   return *this;
}

dos_wildcard& dos_wildcard::assign(const char* wild, bool ignore_case)
{
   do_assign(wild, wild + std::strlen(wild), ignore_case);
   return *this;
}

dos_wildcard& dos_wildcard::assign(const std::string& wild, bool ignore_case)
{
   do_assign(wild.data(), wild.data() + wild.size(), ignore_case);
   return *this;
}

// match:
bool dos_wildcard::match(const char* text)
{
   return do_match(text, text + std::strlen(text));
}

bool dos_wildcard::match(const std::string& text)
{
   return do_match(text.data(), text.data() + text.size());
}

bool dos_wildcard::match(const boost::filesystem::path& path)
{
   return match(path.string());
}

// predicates:
bool dos_wildcard::operator()(const char* text)
{
   return match(text);
}

bool dos_wildcard::operator()(const std::string& text)
{
   return match(text);
}

bool dos_wildcard::operator()(const boost::filesystem::path& text)
{
   return match(text);
}

//implementation:
void dos_wildcard::do_assign(const char* p1, const char* p2, bool ignore_case)
{
   static const boost::regex transformer("([+{}()\\[\\]$\\^|])|(\\*)|(\\?)|(\\.)|([\\\\/:])");
   static const char* replace_string = "(?1\\\\$1)(?2[^\\\\\\\\/\\:]*)(?3[^\\\\\\\\/\\:])(?4\\(\\?\\:\\\\.|$\\))(?5[\\\\\\\\\\\\/\\:])";
   cow();
   boost::regex::flag_type flags = (ignore_case == 0 ? boost::regex::perl : boost::regex::perl | boost::regex::icase);
   m_pimpl->m_expression.assign(regex_replace(std::string(p1, p2), transformer, replace_string, boost::format_all), flags);
}

bool dos_wildcard::do_match(const char* p1, const char*p2)
{
   return regex_match(p1, p2, m_pimpl->m_expression);
}

void dos_wildcard::cow()
{
   // copy-on-write
   if(!m_pimpl.get())
   {
      m_pimpl.reset(new dos_wildcard_implementation());
   }
   else if(!m_pimpl.unique())
   {
      m_pimpl.reset(new dos_wildcard_implementation(*(m_pimpl.get())));
   }
}

} } // namespace PN::IO

