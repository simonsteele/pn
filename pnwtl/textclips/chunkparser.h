/**
 * @file chunkparser.h
 * @brief Text Clips Chunk Parsing.
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
 
#ifndef CHUNKPARSER_H__INCLUDED
#define CHUNKPARSER_H__INCLUDED

#include "../textclips.h"

namespace extensions {
	class IScriptRunner;
}

namespace TextClips {

namespace Internal {
template <typename Iterator>
struct snippet;
}

/**
 * Parser to generate chunks from text clip text.
 */
class ChunkParser
{
public:
	explicit ChunkParser();
	~ChunkParser();

	bool Parse(const std::string& clip, std::vector<Chunk>& chunks);

	void SetVariableProvider(IVariableProvider* variables);
	void SetScriptRunner(extensions::IScriptRunner* runner);

private:
	Internal::snippet<std::string::const_iterator>* m_grammar;
};

} // namespace TextClips

#endif // #ifndef CHUNKPARSER_H__INCLUDED