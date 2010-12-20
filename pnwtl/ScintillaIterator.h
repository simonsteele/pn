#ifndef scintilla_iterator_h__included
#define scintilla_iterator_h__included

/**
 * std::iterator compatible iterator for Scintilla contents
 */
class ScintillaIterator : public std::iterator<std::bidirectional_iterator_tag, char>
{
public:
	ScintillaIterator() : 
		m_scintilla(0), 
		m_pos(0),
		m_end(0)
	{
	}

	ScintillaIterator(CScintilla* scintilla, int pos) : 
		m_scintilla(scintilla),
		m_pos(pos),
		m_end(scintilla->GetLength())
	{
	}

	ScintillaIterator(const ScintillaIterator& copy) :
		m_scintilla(copy.m_scintilla),
		m_pos(copy.m_pos),
		m_end(copy.m_end)
	{
	}

	bool operator == (const ScintillaIterator& other) const
	{
		return (ended() == other.ended()) && (m_scintilla == other.m_scintilla) && (m_pos == other.m_pos);
	}

	bool operator != (const ScintillaIterator& other) const
	{
		return !(*this == other);
	}

	char operator * () const
	{
		return charAt(m_pos);
	}

	ScintillaIterator& operator ++ ()
	{
		m_pos++;
		return *this;
	}

	ScintillaIterator& operator -- ()
	{
		m_pos--;
		return *this;
	}

	int pos() const
	{
		return m_pos;
	}

private:
	char charAt(int position) const
	{
		return m_scintilla->GetCharAt(position);
	}

	bool ended() const
	{
		return m_pos == m_end;
	}

	int m_pos;
	int m_end;
	CScintilla* m_scintilla;
};

#endif // #ifndef scintilla_iterator_h__included