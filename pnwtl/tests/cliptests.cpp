/**
 * Unit tests for Template Clips Behaviour
 */

#include "stdafx.h"

#include <boost/test/unit_test.hpp>

#include "../textclips.h"
#include "../textclips/clipmanager.h"
#include "../scintillaif.h"
#include "mocks/mockoptions.h"

static MockOptions options;

struct clips_fixture
{
	clips_fixture()
	{
		g_Options = &options;
	}
};

BOOST_FIXTURE_TEST_SUITE( clips_tests, clips_fixture );

using namespace TextClips;

/**
 * Check simple text results in a single chunk to be inserted.
 */
BOOST_AUTO_TEST_CASE( plain_text_should_become_single_chunk )
{
	Clip clip(tstring(L""), std::string(""), std::string("Clip\nText\nThree Lines"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(1, chunks.size());
	BOOST_REQUIRE_EQUAL("Clip\nText\nThree Lines", (*chunks.begin()).GetText());
}

/**
 * Check single simple field means three chunks.
 */
BOOST_AUTO_TEST_CASE( field_in_middle_makes_three_chunks )
{
	Clip clip(tstring(L""), std::string(""), std::string("Clip ${1} Text"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(3, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	BOOST_REQUIRE_EQUAL("Clip ", (*chunk).GetText());
	chunk++;
	BOOST_REQUIRE_EQUAL("", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(1, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsMasterField());
	chunk++;
	BOOST_REQUIRE_EQUAL(" Text", (*chunk).GetText());
}

/**
 * Check a clip with just a field works ok.
 */
BOOST_AUTO_TEST_CASE( just_a_field_works )
{
	Clip clip(tstring(L""), std::string(""), std::string("${3}"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(1, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// Only field $(3) should be a master field
	BOOST_REQUIRE_EQUAL("", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(3, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsMasterField());
}

/**
 * The only field insertion mechanism should be the $() type to avoid confusion.
 */
BOOST_AUTO_TEST_CASE( percent_not_treated_specially )
{
	Clip clip(tstring(L""), std::string(""), std::string("text % text"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(1, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// Only field $(3) should be a master field
	BOOST_REQUIRE_EQUAL("text % text", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(false, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(false, (*chunk).IsMasterField());
}

/**
 * Tools-style escapes should not be supported in clips.
 */
BOOST_AUTO_TEST_CASE( percent_not_treated_specially_2 )
{
	Clip clip(tstring(L""), std::string(""), std::string("text %f text"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(1, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// Only field $(3) should be a master field
	BOOST_REQUIRE_EQUAL("text %f text", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(false, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(false, (*chunk).IsMasterField());
}

/**
 * Check a clip with just a field works ok.
 */
BOOST_AUTO_TEST_CASE( just_multiple_fields_works )
{
	Clip clip(tstring(L""), std::string(""), std::string("${3}${2:Test}"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(2, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// Only instance of $(3) should be a master field
	BOOST_REQUIRE_EQUAL("", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(3, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsMasterField());

	// Only instance of $(2) should be a master field
	chunk++;
	BOOST_REQUIRE_EQUAL("Test", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(2, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsMasterField());
}

/**
 * Check initial field text
 */
BOOST_AUTO_TEST_CASE( initial_field_text_is_stored )
{
	Clip clip(tstring(L""), std::string(""), std::string("Clip: ${1:Monkey}"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(2, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// First instance of $(1) should be a master field
	chunk++;
	BOOST_REQUIRE_EQUAL("Monkey", (*chunk).GetText());
}

/**
 * Check a repeated field means one master and one slave.
 */
BOOST_AUTO_TEST_CASE( second_field_instance_is_slave )
{
	Clip clip(tstring(L""), std::string(""), std::string("Clip ${1} Text ${1}"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(4, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// First instance of $(1) should be a master field
	chunk++;
	BOOST_REQUIRE_EQUAL("", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(1, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsMasterField());
	
	// The second instance of $(1) should make a non-master field
	chunk++;
	chunk++;
	BOOST_REQUIRE_EQUAL("", (*chunk).GetText());
	BOOST_REQUIRE_EQUAL(1, (*chunk).Id);
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsField());
	BOOST_REQUIRE_EQUAL(false, (*chunk).IsMasterField());
}

/**
 * Tab stop zero is caret rest position.
 */
BOOST_AUTO_TEST_CASE( stop_zero_should_be_end_caret_pos )
{
	Clip clip(tstring(L""), std::string(""), std::string("Clip: ${0}"));
	std::vector<Chunk> chunks;
	clip.GetChunks(chunks);

	BOOST_REQUIRE_EQUAL(2, chunks.size());
	std::vector<Chunk>::const_iterator chunk(chunks.begin());
	
	// First instance of $(1) should be a master field
	chunk++;
	BOOST_REQUIRE_EQUAL(true, (*chunk).IsFinalCaretPos());
}

/**
 * Can set clipset filename.
 */
BOOST_AUTO_TEST_CASE( set_clipset_filename )
{
	TextClipSet set(_T(""), _T("test"), "default", false);
	BOOST_REQUIRE_EQUAL(_T(""), set.GetFilename());
	set.SetFilename(_T("BLAH"));
	BOOST_REQUIRE_EQUAL(_T("BLAH"), set.GetFilename());
}

/**
 * Retrieve by scheme name works.
 */
BOOST_AUTO_TEST_CASE( add_retrieve_by_schemename )
{
	TextClipsManager manager;
	
	BOOST_REQUIRE_EQUAL(0, manager.GetClips("default").size());

	manager.Add(new TextClipSet(_T(""), _T("Set"), "default", false));

	BOOST_REQUIRE_EQUAL(1, manager.GetClips("default").size());
	BOOST_REQUIRE_EQUAL(_T("Set"), manager.GetClips("default").front()->GetName());
}

/**
 * Can delete a set.
 */
BOOST_AUTO_TEST_CASE( remove_set )
{
	TextClipsManager manager;

	TextClipSet* set = new TextClipSet(_T(""), _T("Set"), "default", false);
	manager.Add(set);
	manager.Delete(set);

	BOOST_REQUIRE_EQUAL(0, manager.GetClips("default").size());
}

/**
 * Deleting one set of several works correctly.
 */
BOOST_AUTO_TEST_CASE( remove_one_of_set )
{
	TextClipsManager manager;

	TextClipSet* set = new TextClipSet(_T(""), _T("Set"), "default", false);
	TextClipSet* set2 = new TextClipSet(_T(""), _T("Set 2"), "default", false);
	manager.Add(set);
	manager.Add(set2);
	manager.Delete(set);

	BOOST_REQUIRE_EQUAL(1, manager.GetClips("default").size());
	BOOST_REQUIRE_EQUAL(set2, manager.GetClips("default").front());
}

/**
 * Reset copies clips from other manager instance.
 */
BOOST_AUTO_TEST_CASE( basic_reset )
{
	TextClipsManager manager;
	manager.Add(new TextClipSet(_T(""), _T("C++ Set"), "cpp", false));

	{
		TextClipsManager other;
		other.Add(new TextClipSet(_T(""), _T("Set"), "default", false));

		manager.Reset(other);
	}
	
	BOOST_REQUIRE_EQUAL(1, manager.GetClips("default").size());
	BOOST_REQUIRE_EQUAL(_T("Set"), manager.GetClips("default").front()->GetName());
	BOOST_REQUIRE_EQUAL(0, manager.GetClips("cpp").size());
}

/**
 * Adding new clip sets to the manager should set a suitable filename.
 */
BOOST_AUTO_TEST_CASE( adding_new_clip_set_sets_filename )
{
	TextClipSet* set = new TextClipSet(_T(""), _T("Set"), "default", false);

	TextClipsManager manager;
	manager.Add(set);

	BOOST_REQUIRE_EQUAL(_T("Tests\\User\\default\\default.clips"), set->GetFilename());
}


/**
 * Adding new clip sets to the manager should set a suitable filename.
 */
BOOST_AUTO_TEST_CASE( filename_allocation_avoids_dupes )
{
	TextClipsManager manager;
	manager.Add(new TextClipSet(_T("default\\default.clips"), _T("Set 2"), "default", false));
	manager.Add(new TextClipSet(_T("python\\default1.clips"), _T("Set 3"), "python", false));
	
	// Because default already exists for default scheme, this should become default1.clips.
	// default1 in c++ should be ignored.
	TextClipSet* set = new TextClipSet(_T(""), _T("Set"), "default", false);
	manager.Add(set);

	BOOST_REQUIRE_EQUAL(_T("Tests\\User\\default\\default1.clips"), set->GetFilename());
}

BOOST_AUTO_TEST_SUITE_END();