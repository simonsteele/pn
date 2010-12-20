#ifndef lexerutils_h__included
#define lexerutils_h__included

#include "scilexerdll.h"

class Accessor;

class Lexer
{
	public:
		virtual ~Lexer(){}

		virtual const char* GetName() const
		{
			return "error";
		}

		virtual void DoLex(unsigned int startPos, int length, int initStyle, char *words[],
                            Accessor &styler) = 0;
		
		virtual void DoFold(unsigned int startPos, int length, int initStyle, char *words[],
                            Accessor &styler) const = 0;
};

class Lexers
{
	public:
		explicit Lexers()
		{
		}

		~Lexers()
		{
		}
		
		int GetCount() const
		{
			return m_lexers.size();
		}

		void AddLexer(Lexer* pLexer)
		{
			m_lexers.push_back(pLexer);
		}

		Lexer* operator [] (int index) const
		{
			return m_lexers[index];
		}

	private:
		std::vector<Lexer*> m_lexers;
};

#endif 