/**
 * @file wrapscintilla.h
 * @brief A scintilla wrapper for use outside PN, derives from the main PN one.
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pypnwrapscintilla_h__included
#define pypnwrapscintilla_h__included

#ifdef _MSC_VER
	#pragma once
#endif

#include "../scintillaif.h"

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
		m_scihWnd = document->GetScintillaHWND();
		Perform = NULL;
	}

	void IndentLine(int line, int indent)
	{
		if (indent < 0)
			return;
		
		CharacterRange crange;
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
};

#endif //#ifndef pypnwrapscintilla_h__included