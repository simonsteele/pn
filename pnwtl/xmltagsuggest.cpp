#include "stdafx.h"
#include "xmltagsuggest.h"

/**
 * Find the attributes for a tag
 */
const string_list* XmlTagSuggest::attributes_for(const std::string& tag) const
{
	string_list_map::const_iterator i = m_tags.find(tag);
	if(i != m_tags.end())
	{
		return &((*i).second);
	}
	
	return NULL;
}

/**
 * Find likely tags given this starting text
 */
void XmlTagSuggest::likely_tags(const std::string& starting_with, string_list& output) const
{
	for(string_list_map::const_iterator i = m_tags.begin();
		i != m_tags.end();
		++i)
	{
		if( (*i).first.substr(0, starting_with.size()) == starting_with )
		{
			output.push_back( (*i).first );
		}
	}
}