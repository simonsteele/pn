/**
 * @file wordcounter.h
 * @brief Word count
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef wordcounter_h__included
#define wordcounter_h__included

/**
 * Generic word-counting class
 * @author Simon Steele
 */
template<class T>
class WordCounter
{
	public:
		int count(const char* delimiters = " \t.,!?)}]>;:=@/\\%|*&^£$\"'~#+" ) //removed: -_
		{
			T* pT = static_cast<T*>(this);

			// this won't work for weenicode.
			bool delims[256];

			int length = pT->getLength();
			int pos = 0;
			int words = 0;
			
			buildDelimiters(delims, 256, delimiters);	
			
			bool inspace = true;
			
			while(pos < length)
			{
				char c = pT->getNextChar();

				if( inspace && c > 0 && c < 256 && isalnum(c) )
				{
					words++;
					inspace = false;
				}
				else if( !inspace )
				{
					inspace = delims[c];
				}

				pos++;
			}

			return words;
		}

	protected:
		void buildDelimiters(bool* delimsset, int size, const char* delimiters)
		{
			memset(delimsset, 0, size * sizeof(bool));
			const char* p = delimiters;
			while(*p)
			{
				if(*p < size)
					delimsset[*p] = true;
				p++;
			}
		}

		//char T::getNextChar() = 0;
		//int T::getLength() = 0;
};

#endif