#ifndef BOOST_WILDCARD_HPP
#define BOOST_WILDCARD_HPP

namespace PN { namespace IO {

class dos_wildcard_implementation;

class dos_wildcard
{
public:
   // construct:
   dos_wildcard();
   dos_wildcard(const char* wild, bool ignore_case = true);
   dos_wildcard(const std::string& wild, bool ignore_case = true);
   dos_wildcard(const dos_wildcard& wild);
   ~dos_wildcard();

   // assign:
   dos_wildcard& operator=(const char* wild);
   dos_wildcard& operator=(const std::string& wild);
   dos_wildcard& operator=(const dos_wildcard& wild);
   dos_wildcard& assign(const char* wild, bool ignore_case = true);
   dos_wildcard& assign(const std::string& wild, bool ignore_case = true);

   // match:
   bool match(const char* text);
   bool match(const std::string& text);
   bool match(const boost::filesystem::path& path);

   // predicates:
   bool operator()(const char* text);
   bool operator()(const std::string& text);
   bool operator()(const boost::filesystem::path& text);

private:
   void do_assign(const char* p1, const char* p2, bool ignore_case);
   bool do_match(const char* p1, const char*p2);
   void cow();

   boost::shared_ptr<dos_wildcard_implementation> m_pimpl;

};
    
}} // namespace PN::IO

#endif

