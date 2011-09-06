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
	double Score(const std::string& str, const std::string& abbr)
	{
        // Short circuits
        if (abbr.size() == 0) return SCORE_TRAILING;
        if (abbr.size() > str.size()) return SCORE_NO_MATCH;

        auto scores = buildScoreArray(str, abbr);

		double sum = 0.0;
		for (auto i = scores.begin(); i != scores.end(); ++i)
		{
			sum += (*i);
		}

        return (sum / (double)scores.size());
    }

private:
	std::vector<double> buildScoreArray(const std::string& str, const std::string& abbreviation)
	{
        auto scores = std::vector<double>(str.size());
        std::string lower(str);
		std::transform(str.begin(), str.end(), lower.begin(), tolower);
		std::string chars(abbreviation);
		std::transform(abbreviation.begin(), abbreviation.end(), chars.begin(), tolower);

		int lastIndex = -1;
		bool started = false;
		for (auto charix = chars.begin(); charix != chars.end(); ++charix)
		{
			char c = *charix;
            size_t index = lower.find(c, lastIndex+1);
            if (index == lower.npos)
		    {
		        return fillArray(scores, SCORE_NO_MATCH);
		    }
			
			if (index == 0)
			{
				// matched the start of the string...
				started = true;
			}

            if (isNewWord(str, index))
		    {
                scores[index - 1] = 1;
                fillArray(scores, SCORE_BUFFER, lastIndex+1, index-1);
            }
            else if (isUpperCase(str, index))
			{
                fillArray(scores, SCORE_BUFFER, lastIndex+1, index);
            }
            else
			{
				fillArray(scores, SCORE_NO_MATCH, lastIndex+1, index);
			}

			scores[index] = SCORE_MATCH;
			lastIndex = index;
		}

        int trailingScore = started ? SCORE_TRAILING_BUT_STARTED : SCORE_TRAILING;
        fillArray(scores, trailingScore, lastIndex + 1);
        return scores;
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
	
    std::vector<double> fillArray(std::vector<double>& array, double value, int from = -1, int to = -1) const
    {
        from = from == -1 ? 0 : max(from, 0);
        to = to == -1 ? array.size() : min(to, array.size());
        for (int i = from; i < to; i++)
	    { 
		    array[i] = value; 
	    }
        
		return array;
    }
};

} // namespace LiquidMetal