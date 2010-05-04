#ifndef lexerutils_h__included
#define lexerutils_h__included

#include "scilexerdll.h"

class Accessor;

class Lexer
{
	public:
		virtual const char* GetName() const
		{
			return "error";
		}

		virtual void DoLex(unsigned int startPos, int length, int initStyle, char *words[],
                            Accessor &styler) const
		{

		}

		virtual void DoFold(unsigned int startPos, int length, int initStyle, WordList *keywordlists[],
                            Accessor &styler) const
		{

		}
};

class Lexers
{
	public:
		Lexers()
		{
			m_count = m_listsize = 0;
			m_pLexers = 0;
		}

		~Lexers()
		{
			if(m_pLexers)
			{
				free(m_pLexers);
			}
		}
		
		int GetCount()
		{
			return m_count;
		}

		void AddLexer(Lexer* pLexer)
		{
			if(!m_pLexers)
			{
				m_pLexers = static_cast<Lexer**>( malloc(2 * sizeof(Lexer*)));
				m_listsize = 2;
			}
			else if(m_count >= m_listsize)
			{
				int newsize = m_listsize * 2;
				Lexer** pNewList = static_cast<Lexer**>( realloc(m_pLexers, newsize * sizeof(Lexer*)));
				if(!pNewList)
				{
					// Argh!
					m_pLexers = NULL;
					m_listsize = m_count = 0;
					return;
				}
				m_pLexers = pNewList;
			}

			m_pLexers[m_count++] = pLexer;
		}

		const Lexer* operator [] (int index)
		{
			return m_pLexers[index];
		}

	protected:
		Lexer**	m_pLexers;
		int		m_count;
		int		m_listsize;
};

#endif 