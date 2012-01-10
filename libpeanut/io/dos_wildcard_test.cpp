

#include <boost/test/test_tools.hpp>
#include <boost/wildcard.hpp>

int test_main( int, char* [] )
{
   boost::dos_wildcard wild;
   BOOST_TEST(wild.assign("*.bmp").match("m.bmp"));
   BOOST_TEST(wild.assign("*.*").match("m.bmp"));
   BOOST_TEST(wild.assign("*.*").match("m."));
   BOOST_TEST(wild.assign("*.*").match("m"));
   BOOST_TEST(wild.assign("?.bmp").match("m.bmp"));
   BOOST_TEST(wild.assign("?.bmp").match("ma.bmp") == 0);
   BOOST_TEST(wild.assign("?.bmp").match(".bmp") == 0);
   // case sensitivity:
   BOOST_TEST(wild.assign("*.bmp").match("m.BMP"));
   BOOST_TEST(wild.assign("*.bmp").match("m.Bmp"));
   BOOST_TEST(wild.assign("*.bmp", false).match("m.BMP") == 0);
   BOOST_TEST(wild.assign("*.bmp", false).match("m.Bmp") == 0);
   BOOST_TEST(wild.assign("*.bmp", false).match("m.bmp"));
   // special chars:
   BOOST_TEST(wild.assign("`!\"£$%^&(){}[]|;'@#~<>,.bmp", false).match("`!\"£$%^&(){}[]|;'@#~<>,.bmp"));
   // path separators:
   BOOST_TEST(wild.assign("*.bmp").match("abc/m.bmp") == 0);
   BOOST_TEST(wild.assign("*.bmp").match("abc\\m.bmp") == 0);
   BOOST_TEST(wild.assign("*.bmp").match("abc:m.bmp") == 0);
   BOOST_TEST(wild.assign("*/*.bmp").match("abc/m.bmp"));
   BOOST_TEST(wild.assign("*\\*.bmp").match("abc/m.bmp"));
   BOOST_TEST(wild.assign("*:*.bmp").match("abc/m.bmp"));
   BOOST_TEST(wild.assign("*/*.bmp").match("abc\\m.bmp"));
   BOOST_TEST(wild.assign("*\\*.bmp").match("abc\\m.bmp"));
   BOOST_TEST(wild.assign("*:*.bmp").match("abc\\m.bmp"));
   BOOST_TEST(wild.assign("*/*.bmp").match("abc:m.bmp"));
   BOOST_TEST(wild.assign("*\\*.bmp").match("abc:m.bmp"));
   BOOST_TEST(wild.assign("*:*.bmp").match("abc:m.bmp"));
   return 0;
}
