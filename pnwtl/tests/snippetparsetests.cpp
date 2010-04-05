#include "stdafx.h"

#include <boost/test/unit_test.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>
#include "../textclips.h"

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phx = boost::phoenix;

//using namespace qi;
//using namespace boost::spirit::ascii;

using qi::int_;
using qi::double_;
using qi::char_;
using qi::parse;
using qi::lit;
using qi::lexeme;
using ascii::space;
using ascii::space_type;
using qi::_1;

using TextClips::Chunk;

typedef std::vector<Chunk>::const_iterator ChunkItC;

BOOST_AUTO_TEST_SUITE( snippet_parse_tests );

template <typename Iterator>
class snippet_parser
{
public:
	explicit snippet_parser() : m_grammar(*this)
	{
	}

	struct snippet : qi::grammar<Iterator, boost::spirit::ascii::space_type>
	{
		explicit snippet(snippet_parser& owner) : snippet::base_type(start), m_parser(owner)
		{
			// bring in _val?
			using qi::labels::_val;

			// text, match anything but $ and collect it up:
			text_rule = lexeme[+(char_ - '$')[_val += qi::_1]];
		
			// Empty tabstop, e.g. $(0)
			empty_tabstop_rule = lit("$(") >> int_[_val = _1] >> char_(')');
			
			// TODO: We need to find a way to not set the text when seeing it, but after the full expression:
			tabstop_text_rule = lit("$(") >> int_[qi::_a = qi::_1] >> char_(',') >> tabstop_inner_text_rule[phx::bind(&snippet::text_tabstop, this, qi::_1, qi::_a)] >> char_(')');
			
			// Rule to match the text in a tabstop:
			tabstop_inner_text_rule = lexeme[+(char_ - ')')[_val += qi::_1]];

			// Master expression:
			start = +(
				text_rule[phx::bind(&snippet::text, this, _1)] | 
				empty_tabstop_rule[phx::bind(&snippet::empty_tabstop, this, _1)] | 
				tabstop_text_rule);
		}

		// RULES:
		qi::rule<Iterator, std::string()> text_rule;
		qi::rule<Iterator, int(), boost::spirit::ascii::space_type> empty_tabstop_rule;
		qi::rule<Iterator, std::string()> tabstop_inner_text_rule;
		qi::rule<Iterator, qi::locals<int>, boost::spirit::ascii::space_type> tabstop_text_rule;
		qi::rule<Iterator, boost::spirit::ascii::space_type> start;
		

		snippet_parser& m_parser;
		
		// HANDLERS:
		void empty_tabstop(int const& i)
		{
			m_tabstops.push_back(Chunk(TextClips::ctField, i));
		}

		void text(std::string const& str)
		{
			m_tabstops.push_back(Chunk(TextClips::ctNone, str));
		}

		void text_tabstop(std::string const& str, int const& i)
		{
			m_tabstops.push_back(Chunk(TextClips::ctField, i, str));
		}

		std::vector<Chunk> m_tabstops;
	};

	bool parse_snippet(Iterator first, Iterator last)
	{
		bool r = phrase_parse(
			first,                          
			last,                           
			m_grammar,
			boost::spirit::ascii::space
		);
	    
		if (first != last) // fail if we did not get a full match
			return false;
	    
		return r;
	}

	const std::vector<Chunk>& get_tabstops() const
	{
		return m_grammar.m_tabstops;
	}

private:
	snippet m_grammar;
};

BOOST_AUTO_TEST_CASE( parse_empty_tabstop )
{
	snippet_parser<std::string::iterator> p;
	std::string input("$(0)");
	std::string input2("$(0");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(false, p.parse_snippet(input2.begin(), input2.end()));
}

BOOST_AUTO_TEST_CASE( collect_text )
{
	snippet_parser<std::string::iterator> p;
	std::string input("blah$(0)");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(2, p.get_tabstops().size());
	BOOST_REQUIRE_EQUAL(0, p.get_tabstops().front().Id);
	
	// Field 1:
	ChunkItC c = p.get_tabstops().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("blah", (*c).GetText());

	// Field 2:
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL(0, (*c).Id);
}

BOOST_AUTO_TEST_CASE( collect_whitespace )
{
	snippet_parser<std::string::iterator> p;
	std::string input(" $(0)");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(2, p.get_tabstops().size());
	BOOST_REQUIRE_EQUAL(0, p.get_tabstops().front().Id);
	
	// Field 1:
	ChunkItC c = p.get_tabstops().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL(" ", (*c).GetText());

	// Field 2:
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL(0, (*c).Id);
}

BOOST_AUTO_TEST_CASE( tabstop_with_initial_text )
{
	snippet_parser<std::string::iterator> p;
	std::string input("$(1, Hello World!)");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	
	BOOST_REQUIRE_EQUAL(1, p.get_tabstops().size());
	
	// Field 1:
	ChunkItC c = p.get_tabstops().begin();
	BOOST_REQUIRE_EQUAL(1, (*c).Id);
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL("Hello World!", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( empty_tabstop_in_list )
{
	snippet_parser<std::string::iterator> p;
	std::string input("$(0)");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(1, p.get_tabstops().size());
	BOOST_REQUIRE_EQUAL(0, p.get_tabstops().front().Id);
}

BOOST_AUTO_TEST_SUITE_END();