#include "stdafx.h"

#include <utility>
#include <boost/test/unit_test.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>
#include <boost/fusion/include/std_pair.hpp>
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
		explicit snippet(snippet_parser& owner) : snippet::base_type(start), m_parser(owner), m_vars(NULL)
		{
			// bring in _val?
			using qi::labels::_val;

			// text, match anything but $ and collect it up:
			text_rule = lexeme[+(char_ - '$' - '\\')[_val += qi::_1]];

			// Handle escapes
			escape_rule = lit("\\") >> char_;

			// Empty placeholder, e.g. ${0)
			empty_placeholder_rule = lit("${") >> int_[_val = _1] >> char_('}');

			// Numbered placeholder with initial text:
			placeholder_text_rule = lit("${") >> int_ >> lit(":") >> placeholder_inner_text_rule >> lit("}");

			// Rule to match the text in a placeholder:
			placeholder_inner_text_rule = lexeme[+(char_ - '}')[_val += qi::_1]];

			// Rule to match variables: ${blah}
			variable_rule = lit("${") >> +char_("-_a-zA-Z0-9") >> lit("}");

			// Master expression:
			start = +(
				text_rule				[phx::bind(&snippet::text, this, _1)] | 
				escape_rule				[phx::bind(&snippet::escape_char, this, _1)] |
				empty_placeholder_rule	[phx::bind(&snippet::empty_placeholder, this, _1)] | 
				placeholder_text_rule	[phx::bind(&snippet::text_placeholder, this, _1)] | 
				variable_rule			[phx::bind(&snippet::variable, this, _1)]
			);
		}

		// RULES:
		qi::rule<Iterator, std::string()> text_rule;
		qi::rule<Iterator, char()> escape_rule;
		qi::rule<Iterator, int(), boost::spirit::ascii::space_type> empty_placeholder_rule;
		qi::rule<Iterator, std::pair<int, std::string>(), boost::spirit::ascii::space_type> placeholder_text_rule;
		qi::rule<Iterator, std::string()> placeholder_inner_text_rule;
		qi::rule<Iterator, std::string()> variable_rule;
		qi::rule<Iterator, boost::spirit::ascii::space_type> start;

		snippet_parser& m_parser;
		
		// HANDLERS:
		void empty_placeholder(int const& i)
		{
			m_placeholders.push_back(Chunk(TextClips::ctField, i));
		}

		void text(std::string const& str)
		{
			append_text(str.c_str());
		}

		void escape_char(char c)
		{
			char buf[2] = {c, 0};
			append_text(buf);
		}

		void append_text(const char* buf)
		{
			if (m_placeholders.size() == 0)
			{
				m_placeholders.push_back(Chunk(TextClips::ctNone, buf));
			}
			else
			{
				Chunk& chunk = m_placeholders.back();
				if (!chunk.IsField())
				{
					std::string text(chunk.GetText());
					text += buf;
					chunk.SetText(text.c_str());
				}
				else
				{
					m_placeholders.push_back(Chunk(TextClips::ctNone, buf));
				}
			}
		}

		void text_placeholder(std::pair<int, std::string> const& args)//std::string const& str, int const& i)
		{
			m_placeholders.push_back(Chunk(TextClips::ctField, args.first, args.second));
		}

		void variable(std::string const& name)
		{
			variable(name, std::string(""));
		}

		void variable(std::string const& name, std::string const& value)
		{
			std::string realText;
			if (m_vars == NULL || !m_vars->GetVariable(name.c_str(), realText))
			{
				realText = value;
			}

			m_placeholders.push_back(Chunk(TextClips::ctNone, realText));
		}

		std::vector<Chunk> m_placeholders;
		TextClips::IVariableProvider* m_vars;
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

	const std::vector<Chunk>& get_placeholders() const
	{
		return m_grammar.m_placeholders;
	}

	void set_variables(TextClips::IVariableProvider* vars)
	{
		m_grammar.m_vars = vars;
	}

private:
	snippet m_grammar;
};

class FakeVariableProvider : public TextClips::IVariableProvider
{
	virtual bool GetVariable(const char* name, std::string& value)
	{
		if (strcmp(name, "TEST") == 0)
		{
			value = "Test Value";
			return true;
		}
		else if (strcmp(name, "Test2") == 0)
		{
			value = "Test Value 2";
			return true;
		}

		return false;
	}
};

BOOST_AUTO_TEST_CASE( parse_empty_placeholder )
{
	snippet_parser<std::string::iterator> p;
	std::string input("${0}");
	std::string input2("${0");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(false, p.parse_snippet(input2.begin(), input2.end()));
}

BOOST_AUTO_TEST_CASE( collect_text )
{
	snippet_parser<std::string::iterator> p;
	std::string input("blah${0}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(2, p.get_placeholders().size());
	BOOST_REQUIRE_EQUAL(0, p.get_placeholders().front().Id);
	
	// Field 1:
	ChunkItC c = p.get_placeholders().begin();
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
	std::string input(" ${0}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(2, p.get_placeholders().size());
	BOOST_REQUIRE_EQUAL(0, p.get_placeholders().front().Id);
	
	// Field 1:
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL(" ", (*c).GetText());

	// Field 2:
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL(0, (*c).Id);
}

BOOST_AUTO_TEST_CASE( placeholder_with_initial_text )
{
	snippet_parser<std::string::iterator> p;
	std::string input("${1:Hello World!}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	
	BOOST_REQUIRE_EQUAL(1, p.get_placeholders().size());
	
	// Field 1:
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(1, (*c).Id);
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL("Hello World!", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( empty_placeholder_in_list )
{
	snippet_parser<std::string::iterator> p;
	std::string input("${0}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(1, p.get_placeholders().size());
	BOOST_REQUIRE_EQUAL(0, p.get_placeholders().front().Id);
}

BOOST_AUTO_TEST_CASE( escape_placeholder_works )
{
	snippet_parser<std::string::iterator> p;
	std::string input("\\${0}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(1, p.get_placeholders().size());
	
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("${0}", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( escape_slash_works )
{
	snippet_parser<std::string::iterator> p;
	std::string input("\\\\");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(1, p.get_placeholders().size());
	
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("\\", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( escape_slash_doesnot_effect_placeholder )
{
	snippet_parser<std::string::iterator> p;
	std::string input("\\\\${0}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(2, p.get_placeholders().size());
	
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("\\", (*c).GetText());

	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
}

BOOST_AUTO_TEST_CASE( basic_variable )
{
	snippet_parser<std::string::iterator> p;
	FakeVariableProvider vars;
	p.set_variables(&vars);

	std::string input("${TEST}");
	BOOST_REQUIRE_EQUAL(true, p.parse_snippet(input.begin(), input.end()));
	BOOST_REQUIRE_EQUAL(1, p.get_placeholders().size());
	
	ChunkItC c = p.get_placeholders().begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("Test Value", (*c).GetText());
}

BOOST_AUTO_TEST_SUITE_END();