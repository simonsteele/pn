/**
 * @file textclips.h
 * @brief Text Clips Classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclips_h__included
#define textclips_h__included

#include <list>

namespace extensions {
	class ScriptRegistry;
}

namespace TextClips {

typedef enum { ctNone = 0, ctField = 0x01, ctMasterField = 0x03, ctFinalCaretPos = 0x4 } EChunkType;

class TextClipSet;

typedef std::list<TextClipSet*> LIST_CLIPSETS;
typedef std::map<std::string, LIST_CLIPSETS> MAP_CLIPSETS;

/**
 * Interface for variable providing.
 */
class IVariableProvider
{
public:
	virtual ~IVariableProvider() {}

	/**
	 * Get the value of a variable.
	 * @param value Output for the variable value
	 * @returns true if value stored, false otherwise
	 */
	virtual bool GetVariable(const char* name, std::string& value) = 0;
};

/**
 * Single part of a smart text clip, can be plain text or a field.
 */
class Chunk
{
public:
	explicit Chunk();
	explicit Chunk(/*EChunkType*/int field, const std::string& text);
	explicit Chunk(/*EChunkType*/int field, int id);
	explicit Chunk(/*EChunkType*/int field, int id, const std::string& text);

	int Id;

	bool IsText() const;
	bool IsField() const;
	bool IsMasterField() const;
	bool IsFinalCaretPos() const;
	std::string GetText() const;
	
	void SetText(const char* text);

	// Field Position tracking:
	void SetPos(int start, int end);
	void GetPos(int& start, int& end) const;
	void OffsetPos(int offset);

private:
	int m_flags;
	std::string m_text;
	std::string m_variable;
	int m_start;
	int m_end;
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
		void GetChunks(std::vector<Chunk>& chunks, CScintilla* scintilla, IVariableProvider* variables, extensions::IScriptRegistry* scriptRegistry) const;

	private:
		void FixText(CScintilla* scintilla, std::vector<Chunk>& chunks) const;
		std::string FixChunkText(std::string& theText, const std::string& theIndent, int eolMode) const;
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
		 * Get the filename that stores these clips.
		 */
		LPCTSTR GetFilename() const;

		/**
		 * Set the filename of this clip set.
		 */
		void SetFilename(LPCTSTR filename);

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
		virtual void Save();

	private:
		void clear();
		
		LIST_CLIPS	m_clips;

		tstring m_name;
		std::string m_scheme;
		tstring m_filename;
		bool m_encodeClipNames;
		bool m_bDirty;
};

} // namespace TextClips

#endif //textclips_h__included