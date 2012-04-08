/*
 * LiquidMetal++, version: 0.1 (2011-09-06)
 *
 * C++ Port of Javascript LiquidMetal library v0.1 by Simon Steele (http://untidy.net/)
 *
 * Original Notes:
 *
 * A mimetic poly-alloy of Quicksilver's scoring algorithm, essentially
 * LiquidMetal.
 *
 * For usage and examples, visit:
 * http://github.com/rmm5t/liquidmetal
 *
 * Licensed under the MIT:
 * http://www.opensource.org/licenses/mit-license.php
 *
 * Copyright (c) 2009, Ryan McGeary (ryanonjavascript -[at]- mcgeary [*dot*] org)
 */

namespace LiquidMetal {

const double SCORE_NO_MATCH = 0.0;
const double SCORE_MATCH = 1.0;
const double SCORE_TRAILING = 0.8;
const double SCORE_TRAILING_BUT_STARTED = 0.9;
const double SCORE_BUFFER = 0.85;

class QuickSilver
{
public:
	/**
	 * Initialize an instance of QuickSilver with the user-provided abbreviation that
	 * items will be evaluated against.
	 */
	explicit QuickSilver(const std::string& abbreviation)
	{
		m_abbreviation.resize(abbreviation.size());
		std::transform(abbreviation.begin(), abbreviation.end(), m_abbreviation.begin(), tolower);
	}

	/**
	 * Evaluate an item against the abbreviation.
	 */
	double Score(const std::string& str)
	{
        // Short circuits
        if (m_abbreviation.size() == 0) return SCORE_TRAILING;
        if (m_abbreviation.size() > str.size()) return SCORE_NO_MATCH;

        buildScoreArray(str, m_abbreviation);

		double sum = 0.0;
		for (std::vector<double>::const_iterator i = m_scores.begin(); i != m_scores.end(); ++i)
		{
			sum += (*i);
		}

        return (sum / (double)m_scores.size());
    }

private:
	void buildScoreArray(const std::string& str, const std::string& chars)
	{
		m_scores.resize(str.size());
        std::string lower;
		lower.resize(str.size());
		std::transform(str.begin(), str.end(), lower.begin(), tolower);

		int lastIndex = -1;
		bool started = false;
		for (auto charix = chars.begin(); charix != chars.end(); ++charix)
		{
			char c = *charix;
            size_t index = lower.find(c, lastIndex+1);
            if (index == lower.npos)
		    {
		        fillArray(m_scores, SCORE_NO_MATCH);
				return;
		    }
			
			if (index == 0)
			{
				// matched the start of the string...
				started = true;
			}

            if (isNewWord(str, index))
		    {
                m_scores[index - 1] = 1;
                fillArray(m_scores, SCORE_BUFFER, lastIndex+1, index-1);
            }
            else if (isUpperCase(str, index))
			{
                fillArray(m_scores, SCORE_BUFFER, lastIndex+1, index);
            }
            else
			{
				fillArray(m_scores, SCORE_NO_MATCH, lastIndex+1, index);
			}

			m_scores[index] = SCORE_MATCH;
			lastIndex = index;
		}

        int trailingScore = started ? SCORE_TRAILING_BUT_STARTED : SCORE_TRAILING;
        fillArray(m_scores, trailingScore, lastIndex + 1);
    }

    bool isUpperCase(const std::string& str, size_t index) const
    {
        char c = str.at(index);
        return ('A' <= c && c <= 'Z');
    }

    bool isNewWord(const std::string& str, size_t index) const
	{
		if (index == 0)
		{
			return false;
		}

		char c = str.at(index - 1);
		return (c == ' ' || c == '\t');
	}
	
    void fillArray(std::vector<double>& buffer, double value, int from = -1, int to = -1) const
    {
        from = from == -1 ? 0 : max(from, 0);
        to = to == -1 ? buffer.size() : min(to, buffer.size());
        for (int i = from; i < to; i++)
	    { 
		    buffer[i] = value; 
	    }
    }

private:
	std::vector<double> m_scores;
	std::string m_abbreviation;
};

} // namespace LiquidMetal