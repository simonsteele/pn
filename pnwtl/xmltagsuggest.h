/**
 * @file xtagsuggest.h
 * @brief Xml Tag Suggest Stuff
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include <hash_map>
typedef stdext::hash_map< std::string, string_list > string_list_map;

class XmlTagSuggest
{
public:
	/**
	 * Find the attributes for a tag
	 */
	const string_list* attributes_for(const std::string& tag) const;
	
	/**
	 * Find likely tags given this starting text
	 */
	void likely_tags(const std::string& starting_with, string_list& output) const;

private:
	/**
	 * Map listing tags and tying them to attribute lists
	 */
	string_list_map m_tags;
};