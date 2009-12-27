/**
 * @file textclips.h
 * @brief Text Clips Classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclips_h__included
#define textclips_h__included

#include <list>

namespace TextClips {

/**
 * Single part of a smart text clip, can be plain text or a field.
 */
class Chunk
{
public:
	Chunk() : m_field(false), Id(0) {}
	Chunk(bool field, const std::string& text) : m_field(field), m_text(text), Id(0) {}
	Chunk(bool field, int id) : m_field(field), Id(id) {}

	int Id;

	bool IsField() const;
	std::string GetText() const;
	
	void SetText(const char* text);

private:
	bool m_field;
	std::string m_text;
};

/**
 * Simple class to represent a text clip.
 */
class Clip
{
	public:
		Clip(const tstring& name, const std::string& shortcut, const std::string& text) : 
		  Name(name),
		  Shortcut(shortcut),
		  Text(text)
		{
		}

		Clip(const Clip& copy) : Name(copy.Name), Shortcut(copy.Shortcut), Text(copy.Text){}

		tstring Name;
		std::string Shortcut;
		std::string Text;

		void Insert(CScintilla* scintilla) const;

		void GetChunks(std::vector<Chunk>& chunks) const;
};

typedef std::list<Clip*>	LIST_CLIPS;

/**
 * Represents a file full of text clips.
 */
class TextClipSet
{
	public:
		TextClipSet(LPCTSTR filename, LPCTSTR name, LPCSTR scheme, bool encodeClipNames);
		TextClipSet(const TextClipSet& copy);
		~TextClipSet();

		/**
		 * Add a clip
		 */
		void Add(Clip* clip);

		/**
		 * Builds a list of clips formatted for scintilla display
		 */
		std::string BuildSortedClipList() const;

		/**
		 * Find a Clip by its text shortcut
		 */
		const Clip* FindByShortcut(const std::string& shortcut) const;

		/**
		 * Get the clips
		 */
		const LIST_CLIPS& GetClips() const;

		/**
		 * Get the name of this clip set
		 */
		LPCTSTR GetName() const;

		/**
		 * Get the filename that stores these clips, or NULL if in the global store
		 */
		LPCTSTR GetFilename() const;

		/**
		 * Get the scheme name if scheme-tied, NULL otherwise
		 */
		LPCSTR GetScheme() const;

		/**
		 * Should we encode text clip names?
		 */
		bool GetEncodeClipNames() const;

		/**
		 * Remove a clip
		 */
		void Remove(TextClips::Clip* clip);

		/**
		 * Save this clipset to its file
		 */
		void Save();

	private:
		void clear();
		
		LIST_CLIPS	m_clips;

		tstring m_name;
		std::string m_scheme;
		tstring m_filename;
		bool m_encodeClipNames;
};

typedef std::list<TextClipSet*> LIST_CLIPSETS;
typedef std::map<std::string, TextClipSet*> MAP_CLIPSETS;

/**
 * Represents a set of text clip sets.
 */
class TextClipsManager : public XMLParseState
{
	public:
		TextClipsManager();
		TextClipsManager(const TextClipsManager& copy);
		~TextClipsManager();

		void OnFound(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/);

		const LIST_CLIPSETS& GetClipSets();

		TextClipSet* GetClips(LPCSTR schemeName);

		void Add(TextClipSet* clips);

		void Save(bool ignoreFilenames = false);

		void Reset(const TextClipsManager& copy);

		//XMLParseState
	public:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	private:
		typedef enum tagEncodings
			{
				eNone,
				eWindows1252,
				eANSI,
			} EEncoding;

		void clear();
		void copy(const TextClipsManager& copy);
		void decodeData();
		void findClips();
		void parse(LPCTSTR filename);

		LIST_CLIPSETS	m_clipSets;
		MAP_CLIPSETS	m_schemeClipSets;

		// Parse state:
		bool decodeNames;
		int	m_parseState;
		std::string m_cData;
		tstring m_curName;
		std::string m_curShortcut;
		tstring m_curFileName;
		TextClipSet* m_pCurSet;
		EEncoding m_curEncoding;
};

} // namespace TextClips

#endif //textclips_h__included