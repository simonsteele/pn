/**
 * @file chunkparser.cpp
 * @brief Text Clips Chunk Parsing.
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
 
#include "stdafx.h"
#include "chunkparser.h"

#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>
#include <boost/fusion/include/std_pair.hpp>

using namespace TextClips;
using namespace extensions;

namespace TextClips { namespace Internal {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phx = boost::phoenix;

using qi::int_;
using qi::double_;
using qi::char_;
using qi::parse;
using qi::lit;
using qi::lexeme;
using qi::no_skip;
using ascii::space;
using ascii::space_type;
using qi::_1;

/**
 * Grammar for parsing PN Clips, inspired by TextMate snippet syntax.
 */
template <typename Iterator>
struct snippet : qi::grammar<Iterator, boost::spirit::ascii::space_type>
{
	explicit snippet() : snippet::base_type(start), m_vars(NULL), m_runner(NULL)
	{
		// bring in _val?
		using qi::labels::_val;

		// text, match anything but $ and collect it up:
		text_rule = no_skip[+(char_ - '$' - '\\' - '`')[_val += qi::_1]];

		// Handle escapes
		escape_rule = lit("\\") >> char_;

		// Empty placeholder, e.g. ${0)
		empty_placeholder_rule = lit("${") >> int_[_val = _1] >> char_('}');

		// Short form placeholders
		short_form_placeholder_rule = lit("$") >> int_[_val = _1];

		// Numbered placeholder with initial text:
		placeholder_text_rule = lit("${") >> int_ >> lit(":") >> placeholder_inner_text_rule >> lit("}");

		// Rule to match the text in a placeholder:
		placeholder_inner_text_rule = no_skip[+(char_ - '}')[_val += qi::_1]];

		// Rule to match variables: ${blah}
		variable_rule = lit("${") >> +char_("-_a-zA-Z0-9") >> lit("}");

		// Rule to match variables: ${blah:default value}
		variable_with_default_rule = lit("${") >> +char_("-_a-zA-Z0-9") >> lit(":") >> placeholder_inner_text_rule >> lit("}");

		// Rule to match backtick expressions: `code here`
		backtick_expression_rule = lit('`') >> backtick_inner_text_rule >> lit('`');

		// Rule to match the text in a placeholder
		backtick_inner_text_rule = no_skip[+(char_ - '`')[_val += qi::_1]];

		// Master expression:
		start = +(
			no_skip[text_rule			[phx::bind(&snippet::text, this, _1)]] | 
			escape_rule					[phx::bind(&snippet::escape_char, this, _1)] |
			empty_placeholder_rule		[phx::bind(&snippet::empty_placeholder, this, _1)] | 
			placeholder_text_rule		[phx::bind(&snippet::text_placeholder, this, _1)] | 
			variable_rule				[phx::bind(&snippet::variable, this, _1)] |
			variable_with_default_rule	[phx::bind(&snippet::variable_default, this, _1)] |
			short_form_placeholder_rule [phx::bind(&snippet::empty_placeholder, this, _1)] |
			backtick_expression_rule    [phx::bind(&snippet::backtick, this, _1)]
		);
	}

	// RULES:
	qi::rule<Iterator, std::string()> text_rule;
	qi::rule<Iterator, char()> escape_rule;
	qi::rule<Iterator, int(), boost::spirit::ascii::space_type> empty_placeholder_rule;
	qi::rule<Iterator, int()> short_form_placeholder_rule;
	qi::rule<Iterator, std::pair<int, std::string>(), boost::spirit::ascii::space_type> placeholder_text_rule;
	qi::rule<Iterator, std::string()> placeholder_inner_text_rule;
	qi::rule<Iterator, std::string()> variable_rule;
	qi::rule<Iterator, std::pair<std::string, std::string>()> variable_with_default_rule;
	qi::rule<Iterator, std::string()> backtick_expression_rule;
	qi::rule<Iterator, std::string()> backtick_inner_text_rule;
	qi::rule<Iterator, boost::spirit::ascii::space_type> start;

	// HANDLERS:
	void empty_placeholder(int const& stop)
	{
		int chunkType = m_seenStops.insert(stop).second ? ctMasterField : ctField;
		if (stop == 0)
		{
			chunkType = chunkType | ctFinalCaretPos;
		}

		m_placeholders.push_back(Chunk(chunkType, stop));
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
		int chunkType = m_seenStops.insert(args.first).second ? ctMasterField : ctField;
		if (args.first == 0)
		{
			chunkType = chunkType | ctFinalCaretPos;
		}

		m_placeholders.push_back(Chunk(chunkType, args.first, args.second));
	}

	void variable(std::string const& name)
	{
		variable(name, std::string(""));
	}

	void variable_default(std::pair<std::string, std::string> const& details)
	{
		variable(details.first, details.second);
	}

	void variable(std::string const& name, std::string const& value)
	{
		std::string realText;
		if (m_vars == NULL || !m_vars->GetVariable(name.c_str(), realText))
		{
			realText = value;
		}

		append_text(realText.c_str());
	}

	void backtick(std::string const& text)
	{
		if (m_runner != NULL)
		{
			PN::AString as;
			
			m_runner->Exec("evalScript", text.c_str(), extensions::efCaptureOutput | extensions::efBuiltIn, as);

			append_text(as.Get());
		}
		else
		{
			append_text("");
		}
	}

	std::vector<Chunk> m_placeholders;
	std::set<int> m_seenStops;
	TextClips::IVariableProvider* m_vars;
	extensions::IScriptRunner* m_runner;
};

}} // namespace TextClips::Internal

ChunkParser::ChunkParser()
{
	m_grammar = new Internal::snippet<std::string::const_iterator>();
}

ChunkParser::~ChunkParser()
{
	delete m_grammar;
}

bool ChunkParser::Parse(const std::string& clip, std::vector<Chunk>& chunks)
{
	std::string::const_iterator first = clip.begin();
	std::string::const_iterator last = clip.end();
	bool r = phrase_parse(
		first,
		last,
		*m_grammar,
		boost::spirit::ascii::space
	);
    
	if (first != last)
	{
		// if we did not get a full match, we copy in the remaining text:
		std::string buf(first, last);
		m_grammar->m_placeholders.push_back(Chunk(TextClips::ctNone, buf));
		r = false;
	}
    
	std::swap(m_grammar->m_placeholders, chunks);

	return r;
}

void ChunkParser::SetVariableProvider(IVariableProvider* variables)
{
	m_grammar->m_vars = variables;
}

void ChunkParser::SetScriptRunner(IScriptRunner* runner)
{
	m_grammar->m_runner = runner;
}