/**
 * @file wrapscintilla.h
 * @brief A scintilla wrapper for use outside PN, derives from the main PN one.
 * @author Simon Steele
 * @note Copyright (c) 2006-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pypnwrapscintilla_h__included
#define pypnwrapscintilla_h__included

#ifdef _MSC_VER
	#pragma once
#endif

#include "../scintillaif.h"

#define PN_OVERWRITETARGET	(WM_APP+19)
#define PN_INSERTCLIPTEXT   (WM_APP+23)

class PNScintilla : public CScintilla
{
public:
	PNScintilla(HWND hWndEditor)
	{
		m_scihWnd = hWndEditor;
		Perform = NULL;
	}

	PNScintilla(extensions::IDocument* document)
	{
		if (document == NULL)
		{
			throw std::exception("Invalid document object: None");
		}

		m_scihWnd = document->GetScintillaHWND();
		Perform = NULL;
	}

	void IndentLine(int line, int indent)
	{
		if (indent < 0)
			return;
		
		Scintilla::CharacterRange crange;
		GetSel(crange);

		int posBefore = GetLineIndentPosition(line);
		
		SetLineIndentation(line, indent);
		
		int posAfter = GetLineIndentPosition(line);
		int posDifference =  posAfter - posBefore;
		
		if (posAfter > posBefore) 
		{
			// Move selection on
			if (crange.cpMin >= posBefore) 
			{
				crange.cpMin += posDifference; 
			}

			if (crange.cpMax >= posBefore) 
			{
				crange.cpMax += posDifference; 
			}
		} 
		else if (posAfter < posBefore) 
		{
			// Move selection back
			if (crange.cpMin >= posAfter) 
			{
				if (crange.cpMin >= posBefore)
					crange.cpMin += posDifference; 
				else 
					crange.cpMin = posAfter; 
			}
			if (crange.cpMax >= posAfter) 
			{
				if (crange.cpMax >= posBefore)
					crange.cpMax += posDifference; 
				else 
					crange.cpMax = posAfter; 
			}
		}
		SetSel(crange.cpMin, crange.cpMax);
	}

	std::string GetSelTextAsString()
	{
		std::string buf;
		buf.resize(GetSelText(NULL)+1);
		size_t size = GetSelText(&buf[0]);
		// size includes the NULL terminator
		buf.resize(size - 1);

		return buf;
	}

	std::string GetLineAsString(int line)
	{
		std::string buf;
		buf.resize(LineLength(line) + 1);
		int len = GetLine(line, &buf[0]);
		buf.resize(len);
		return buf;
	}

	boost::python::tuple GetCurLineAsString()
	{
		std::string buf;
		buf.resize(GetCurLine(0, NULL));
		
		int linePos = GetCurLine(buf.size(), &buf[0]);
		
		return boost::python::make_tuple(buf, linePos);
	}

	std::string GetTextRangeAsString(int start, int end)
	{
		std::string buf;

		if(end <= start)
		{
			return buf;
		}

		buf.resize(end-start+1);
		Scintilla::TextRange tr;
		tr.chrg.cpMin = start;
		tr.chrg.cpMax = end;
		tr.lpstrText = &buf[0];
		size_t size = GetTextRange(&tr);
		buf.resize(size);
		
		return buf;
	}

	/**
	 * Implement GetText to return std::string.
	 * @param length Number of characters to be retrieved - no need to include null in length.
	 */
	std::string GetTextAsString(int length)
	{
		if (length == 0)
		{
			return std::string();
		}

		std::string buf;
		buf.resize(length + 1);
		length = GetText(length + 1, &buf[0]);
		buf.resize(length);
		return buf;
	}

	boost::python::object FindTextString(int start, int end, const char* string_to_find, int flags)
	{
		Scintilla::TextToFind f;	
		f.lpstrText = (char*)string_to_find;
		f.chrg.cpMin = start;
		f.chrg.cpMax = end;
		long result = FindText(flags, &f);
		
		if(result == -1)
		{
			return boost::python::object(false);
		}

		return boost::python::make_tuple(f.chrgText.cpMin, f.chrgText.cpMax);
	}

	void SetFocus()
	{
		::SetFocus(m_scihWnd);
	}

	void BeginOverwriteTarget()
	{
		::SendMessage(m_scihWnd, PN_OVERWRITETARGET, 0, 0);
	}

	void InsertClip(std::string clipText)
	{
		::SendMessage(m_scihWnd, PN_INSERTCLIPTEXT, 0, reinterpret_cast<LPARAM>(clipText.c_str()));
	}
};

#endif //#ifndef pypnwrapscintilla_h__included