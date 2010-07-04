#include "stdafx.h"

#include <utility>
#include <boost/test/unit_test.hpp>
#include "../textclips.h"
#include "../textclips/chunkparser.h"
#include "mocks/mockscriptrunner.h"

BOOST_AUTO_TEST_SUITE( snippet_parse_tests );

using TextClips::Chunk;

typedef std::vector<Chunk>::const_iterator ChunkItC;

class FakeVariableProvider : public TextClips::IVariableProvider
{
	virtual bool GetVariable(const char* name, std::string& value)
	{
		if (stricmp(name, "TEST") == 0)
		{
			value = "Test Value";
			return true;
		}
		else if (stricmp(name, "Test2") == 0)
		{
			value = "Test Value 2";
			return true;
		}

		return false;
	}
};

BOOST_AUTO_TEST_CASE( parse_empty_placeholder )
{
	TextClips::ChunkParser p;
	std::string input("${0}");
	std::string input2("${0");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(false, p.Parse(input2, chunks));
}

BOOST_AUTO_TEST_CASE( collect_text )
{
	TextClips::ChunkParser p;
	std::string input("blah${0}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(2, chunks.size());
	BOOST_REQUIRE_EQUAL(0, chunks.front().Id);
	
	// Field 1:
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("blah", (*c).GetText());

	// Field 2:
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL(0, (*c).Id);
}

BOOST_AUTO_TEST_CASE( collect_whitespace )
{
	TextClips::ChunkParser p;
	std::string input(" ${0}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(2, chunks.size());
	BOOST_REQUIRE_EQUAL(0, chunks.front().Id);
	
	// Field 1:
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL(" ", (*c).GetText());

	// Field 2:
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL(0, (*c).Id);
}

BOOST_AUTO_TEST_CASE( placeholder_with_initial_text )
{
	TextClips::ChunkParser p;
	std::string input("${1:Hello World!}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	// Field 1:
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(1, (*c).Id);
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL("Hello World!", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( placeholder_short_form )
{
	TextClips::ChunkParser p;
	std::string input("${1:Hello World!} and again: $1");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	
	BOOST_REQUIRE_EQUAL(3, chunks.size());
	
	// Field 1:
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(1, (*c).Id);
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
	BOOST_REQUIRE_EQUAL("Hello World!", (*c).GetText());
	c++;
	c++;
	BOOST_REQUIRE_EQUAL(1, (*c).Id);
}

BOOST_AUTO_TEST_CASE( empty_placeholder_in_list )
{
	TextClips::ChunkParser p;
	std::string input("${0}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	BOOST_REQUIRE_EQUAL(0, chunks.front().Id);
}

BOOST_AUTO_TEST_CASE( escape_placeholder_works )
{
	TextClips::ChunkParser p;
	std::string input("\\${0}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("${0}", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( escape_slash_works )
{
	TextClips::ChunkParser p;
	std::string input("\\\\");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("\\", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( escape_slash_doesnot_effect_placeholder )
{
	TextClips::ChunkParser p;
	std::string input("\\\\${0}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(2, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(false, (*c).IsField());
	BOOST_REQUIRE_EQUAL("\\", (*c).GetText());

	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsField());
}

BOOST_AUTO_TEST_CASE( basic_variable )
{
	TextClips::ChunkParser p;
	FakeVariableProvider vars;
	p.SetVariableProvider(&vars);

	std::string input("${TEST}");
	std::vector<Chunk> chunks;

	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("Test Value", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( basic_variable_in_text )
{
	TextClips::ChunkParser p;
	FakeVariableProvider vars;
	p.SetVariableProvider(&vars);

	std::string input("here is some${test2}text");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("here is someTest Value 2text", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( basic_variable_with_ignored_default )
{
	TextClips::ChunkParser p;
	FakeVariableProvider vars;
	p.SetVariableProvider(&vars);

	std::string input("${test:blah}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("Test Value", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( basic_variable_with_default )
{
	TextClips::ChunkParser p;
	FakeVariableProvider vars;
	p.SetVariableProvider(&vars);

	std::string input("${test3:blah blah}");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("blah blah", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( parse_failure_copies_text )
{
	TextClips::ChunkParser p;
	FakeVariableProvider vars;
	p.SetVariableProvider(&vars);

	std::string input("${1:Test} $(2) test");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(false, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(3, chunks.size());
	
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsMasterField());
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL(" ", (*c).GetText());
	c++;
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("$(2) test", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( backticks_evaluate_content )
{
	TextClips::ChunkParser p;
	FakeScriptRunner runner;
	p.SetScriptRunner(&runner);
	
	std::string input("`print \"Hello World\"`");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());

	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("Hello World", (*c).GetText());
}

BOOST_AUTO_TEST_CASE( backticks_default_blank )
{
	TextClips::ChunkParser p;
	
	std::string input("`print \"Hello World\"`");
	std::vector<Chunk> chunks;
	BOOST_REQUIRE_EQUAL(true, p.Parse(input, chunks));
	BOOST_REQUIRE_EQUAL(1, chunks.size());

	// With no script runner, we should just get blank:
	ChunkItC c = chunks.begin();
	BOOST_REQUIRE_EQUAL(true, (*c).IsText());
	BOOST_REQUIRE_EQUAL("", (*c).GetText());
}

BOOST_AUTO_TEST_SUITE_END();