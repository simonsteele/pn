#ifndef DSSI_COLORBUTTON_H
#define DSSI_COLORBUTTON_H

//-----------------------------------------------------------------------------
// 
// @doc
//
// @module	ColorButton.h - IO monitor main window |
//
// This module contains the definition of the io monitor window.
//
// Based on work by Chris Maunder, Alexander Bischofberger and James White.
// (See following original copyright statement)
//
// http://www.codetools.com/miscctrl/colorbutton.asp
// http://www.codetools.com/miscctrl/colour_picker.asp
//
// Copyright (c) 2000-2002 - Descartes Systems Sciences, Inc.
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without 
// modification, are permitted provided that the following conditions are 
// met:
// 
// 1. Redistributions of source code must retain the above copyright notice, 
//    this list of conditions and the following disclaimer. 
// 2. Neither the name of Descartes Systems Sciences, Inc nor the names of 
//    its contributors may be used to endorse or promote products derived 
//    from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
// TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// --- ORIGINAL COPYRIGHT STATEMENT ---
//
// Written by Chris Maunder (chrismaunder@codeguru.com)
// Extended by Alexander Bischofberger (bischofb@informatik.tu-muenchen.de)
// Copyright (c) 1998.
//
// Updated 30 May 1998 to allow any number of colours, and to
//                     make the appearance closer to Office 97. 
//                     Also added "Default" text area.         (CJM)
//
//         13 June 1998 Fixed change of focus bug (CJM)
//         30 June 1998 Fixed bug caused by focus bug fix (D'oh!!)
//                      Solution suggested by Paul Wilkerson.
//
// ColourPopup is a helper class for the colour picker control
// CColourPicker. Check out the header file or the accompanying 
// HTML doc file for details.
//
// This code may be used in compiled form in any way you desire. This
// file may be redistributed unmodified by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever. It's free, so don't hassle me about it.
//
// Expect bugs.
// 
// Please use and enjoy. Please let me know of any bugs/mods/improvements 
// that you have found/implemented and I will fix/incorporate them into this
// file. 
//
// @end
//
// $History: ColorButton.h $
//      
//      *****************  Version 3  *****************
//      User: Tim Smith    Date: 9/10/01    Time: 9:05a
//      Updated in $/Omni_V2/exe_cnf
//      
//      *****************  Version 2  *****************
//      User: Tim Smith    Date: 8/28/01    Time: 4:25p
//      Updated in $/Omni_V2/exe_cnf
//      Updated copyright dates.
//      
//      *****************  Version 1  *****************
//      User: Tim Smith    Date: 8/28/01    Time: 3:19p
//      Created in $/Omni_V2/exe_cnf
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// Instructions on how to add CColorButton to you application:
//
//   1. Copy CColorButton.h and CColorButton.cpp into you application directory.
//
//   2. Add the two files into your project.
//
//   3. Many little features will not be enabled unless you have WINVER
//		and _WIN32_WINNT defined properly.  You can defined these values
//		in your stdafx.h.
//
//		_WIN32_WINNT >= 0x0501	- Theme support, XP flat menu support, XP drop 
//								  shadow support.
//
//		WINVER >= 0x0500		- Multi-monitor support, XP menu highlight 
//								  color support.
//
//		WINVER >= 0x0501		- XP menu highlight color support.
//
//		For MOST applications, both _WIN32_WINNT and WINVER should be 
//		defined to be 0x0501.
//
//   4. If you want XP theme support, add "#include <atltheme.h>" to your 
//		stdafx.h file.  This should be added after all the other atl includes.
//
//   5. CColorButton makes heavy use of helper types from ATL.  You will need
//		to make sure that "atltypes.h" and "atlgdi.h" are being included 
//		in stdafx.h.
//
//	 6. Add a button to the dialog in question using the resource editor.
//		You don't have to make and style adjustments to the button.
//
//   7. Edit the class definition for the dialog box.  To the message map,
//		add "REFLECT_NOTIFICATIONS ()".
//
//	 8. Add "#include "ColorButton.h"" prior to the definition of the dialog
//		box class.
//
//	 9. Add a new member variable to the dialog.  The class will be 
//		"CColorButton" and give it any name you desire.  For this example,
//		we will call it "m_btnMyColor".
//
//  10. Inside your OnInitDialog for the dialog, add a line to subclass
//		the m_btnMyColor control.  It is important that is it subclassed
//		and not just assigned a window handle.
//
//		m_btnMyColor .SubclassWindow (GetDlgItem (IDC_MY_COLOR));
//
//	11. Compile and enjoy.
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// Required include files
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// Forward definitions
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// Test for themes
//
//-----------------------------------------------------------------------------

#if !defined (COLORBUTTON_NOTHEMES) && !defined (__ATLTHEME_H__)
#define COLORBUTTON_NOTHEMES
#endif

//-----------------------------------------------------------------------------
//
// Notification messages
//
//-----------------------------------------------------------------------------

#define CPN_SELCHANGE        0x8000	/* Colour Picker Selection change */
#define CPN_DROPDOWN         0x8001	/* Colour Picker drop down */
#define CPN_CLOSEUP          0x8002	/* Colour Picker close up */
#define CPN_SELENDOK         0x8003	/* Colour Picker end OK */
#define CPN_SELENDCANCEL     0x8004	/* Colour Picker end (cancelled) */

struct NMCOLORBUTTON
{
	NMHDR		hdr;
	BOOL		fColorValid;
	COLORREF	clr;
};

//-----------------------------------------------------------------------------
//
// Class definition
//
//-----------------------------------------------------------------------------

class CColorButton :
	public CWindowImpl <CColorButton>
#if !defined (COLORBUTTON_NOTHEMES)
	, public CThemeImpl <CColorButton>
#endif
{
// @access Types and enumerations
public:

	struct ColorTableEntry
	{
		COLORREF	clrColor;
		LPCTSTR		pszName;
	};

// @access Construction and destruction
public:

	// @cmember General constructor

	CColorButton ();

	// @cmember General destructor

	~CColorButton ();

// @access Public inline methods
public:

	// @cmember Subclass the window

	BOOL SubclassWindow (HWND hWnd);

	// @cmember Get the current color

	COLORREF GetColor (void) const
	{
		return m_clrCurrent;
	}

	// @cmember Set the current color

	void SetColor (COLORREF clrCurrent)
	{
		m_clrCurrent = clrCurrent;
		if (IsWindow ())
			InvalidateRect (NULL);
	}

	// @cmember Get the default color

	COLORREF GetDefaultColor (void) const
	{
		return m_clrDefault;
	}

	// @cmember Set the default color

	void SetDefaultColor (COLORREF clrDefault)
	{
		m_clrDefault = clrDefault;
	}

	// @cmember Set the custom text

	void SetCustomText (LPCTSTR pszText)
	{
		if (m_pszCustomText)
			free (m_pszCustomText);
		if (pszText)
			m_pszCustomText = _tcsdup (pszText);
		else
			m_pszCustomText = NULL;
	}

	// @cmember Set the custom text via a resource string

	void SetCustomText (UINT nID)
	{
		if (nID == 0)
			SetCustomText ((LPCTSTR) NULL);
		else
		{
			TCHAR szText [256];
			if (LoadString (_Module .GetResourceInstance (),
				nID, szText, sizeof (szText) / sizeof (szText [0])) == 0)
				return;
			SetCustomText (szText);
		}
	}

	// @cmember Set the default text

	void SetDefaultText (LPCTSTR pszText)
	{
		if (m_pszDefaultText)
			free (m_pszDefaultText);
		if (pszText)
			m_pszDefaultText = _tcsdup (pszText);
		else
			m_pszDefaultText = NULL;
	}

	// @cmember Set the default text via a resource string

	void SetDefaultText (UINT nID)
	{
		if (nID == 0)
			SetDefaultText ((LPCTSTR) NULL);
		else
		{
			TCHAR szText [256];
			if (LoadString (_Module .GetResourceInstance (),
				nID, szText, sizeof (szText) / sizeof (szText [0])) == 0)
				return;
			SetDefaultText (szText);
		}
	}

	// @cmember Get the tracking flag

	BOOL GetTrackSelection (void) const
	{
		return m_fTrackSelection;
	}

	// @cmember Set the tracking flag

	void SetTrackSelection (BOOL fTrack)
	{
		m_fTrackSelection = fTrack;
	}

	// @cmember Set both strings from a resource

	void SetText (UINT nDefault, UINT nCustom)
	{
		SetDefaultText (nDefault);
		SetCustomText (nCustom);
	}

	// @cmember Do we have custom text

	BOOL HasCustomText () const
	{
		return m_pszCustomText && m_pszCustomText [0] != 0;
	}

	// @cmember Do we have default text

	BOOL HasDefaultText () const
	{
		return m_pszDefaultText && m_pszDefaultText [0] != 0;
	}

// @access ATL window support
public:

	BEGIN_MSG_MAP (CColorButton)
#if !defined (COLORBUTTON_NOTHEMES)
		CHAIN_MSG_MAP (CThemeImpl <CColorButton>)	// should be here, not at bottom
#endif

		MESSAGE_HANDLER (WM_MOUSEMOVE, OnMouseMove);
		MESSAGE_HANDLER (WM_MOUSELEAVE, OnMouseLeave);

	    MESSAGE_HANDLER (OCM__BASE + WM_DRAWITEM, OnDrawItem)

	    REFLECTED_COMMAND_CODE_HANDLER (BN_CLICKED, OnClicked)

		ALT_MSG_MAP (1)

		MESSAGE_HANDLER (WM_PAINT, OnPickerPaint);
		MESSAGE_HANDLER (WM_QUERYNEWPALETTE, OnPickerQueryNewPalette);
		MESSAGE_HANDLER (WM_PALETTECHANGED, OnPickerPaletteChanged);
	END_MSG_MAP ()

// @access ATL Message handlers
protected:

	// @cmember Handle draw item

	LRESULT OnDrawItem (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle mouse move

	LRESULT OnMouseMove (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle mouse leave

	LRESULT OnMouseLeave (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle key down for picker

	LRESULT OnPickerKeyDown (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle button up event for picker

	LRESULT OnPickerLButtonUp (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle mouse move for picker

	LRESULT OnPickerMouseMove (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle paint for picker

	LRESULT OnPickerPaint (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle palette query for picker

	LRESULT OnPickerQueryNewPalette (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

	// @cmember Handle palette change for picker

	LRESULT OnPickerPaletteChanged (UINT uMsg, 
		WPARAM wParam, LPARAM lParam, BOOL &bHandled);

// @access Notification handlers
public:

	// @cmember Handle on click

	LRESULT OnClicked (WORD wNotifyCode, WORD wID, 
		HWND hWndCtl, BOOL& bHandled);

// @access Protected methods
protected:

	// @cmember Display the picker popup

	BOOL Picker ();

	// @cmember Set the window size of the picker control

	void SetPickerWindowSize ();

	// @cmember Create the picker tooltips

	void CreatePickerToolTips (CToolTipCtrl &sToolTip);

	// @cmember Get the rect of a given cell

	BOOL GetPickerCellRect (int nIndex, RECT *pRect) const;

	// @cmember Set the selected color from the given color

	void FindPickerCellFromColor (COLORREF clr);

	// @cmember Set a new selection

	void ChangePickerSelection (int nIndex);

	// @cmember End the picker selection process

	void EndPickerSelection (BOOL fOked);

	// @cmember Draw a cell

	void DrawPickerCell (CDC &dc, int nIndex);

	// @cmember Send notification message

	void SendNotification (UINT nCode, COLORREF clr, BOOL fColorValid);

	// @cmember Do a hit test

	int PickerHitTest (const POINT &pt);

// @access Protected static methods
protected:

	// @cmember Draw an arrow

	static void DrawArrow (CDC &dc, const RECT &rect,
		int iDirection = 0, COLORREF clrArrow = RGB (0, 0, 0));

// @access Protected members
protected:

	//
	// THE FOLLOWING variables control the actual button
	//

	// @cmember Current color

	COLORREF				m_clrCurrent;

	// @cmember default color

	COLORREF				m_clrDefault;

	// @cmember Default text

	LPTSTR					m_pszDefaultText;

	// @cmember Custom text

	LPTSTR					m_pszCustomText;

	// @cmember True if popup active override

	BOOL					m_fPopupActive;

	// @cmember True if tracking selection

	BOOL					m_fTrackSelection;

	// @cmember True if the mouse is over

	BOOL					m_fMouseOver;

	//
	// THE FOLLOWING variables control the popup
	//

	// @cmember The contained picker control

	CContainedWindow		m_wndPicker;

	// @cmember Array of colors for the popup

    static ColorTableEntry	gm_sColors [];

	// @cmember Number of columns in the picker

	int						m_nNumColumns;

	// @cmember Number of rows in the picker

    int						m_nNumRows;

	// @cmember Total number of colors in the color array
    
	int						m_nNumColors;

	// @cmember Font used on the picker control

    CFont					m_font;

	// @cmember Pallete used on the picker control

	CPalette				m_palette;

	// @cmember Current picker color

	COLORREF				m_clrPicker;

	// @cmember Margins for the picker

	CRect					m_rectMargins;

	// @cmember Rectangle of the custom text

    CRect					m_rectCustomText;

	// @cmember Rectangle of the default text

	CRect					m_rectDefaultText;

	// @cmember Rectangle of the boxes

	CRect					m_rectBoxes;

	// @cmember If true, menu is flat

	BOOL					m_fPickerFlat;

	// @cmember Size of the color boxes 

	CSize					m_sizeBox;

	// @cmember Picker current selection

    int						m_nCurrentSel;

	// @cmember The original user selection

    int						m_nChosenColorSel;

	// @cmember If true, the picker was OK, and no canceled out

	BOOL					m_fOked;

	// @cmember Color used to draw background

	COLORREF				m_clrBackground;

	// @cmember Color used for highlight border

	COLORREF				m_clrHiLightBorder;

	// @cmember Color used for highlight 

	COLORREF				m_clrHiLight;

	// @cmember Color used for low-light

	COLORREF				m_clrLoLight;

	// @cmember Color used for highlight text

	COLORREF				m_clrHiLightText;

	// @cmember Color used for normal text

	COLORREF				m_clrText;
};

#endif // DSSI_COLORBUTTON_H
