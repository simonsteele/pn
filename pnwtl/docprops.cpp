/**
 * @file docprops.cpp
 * @brief Document properties property sheet
 * @author Simon Steele
 * @note Copyright (c) 2004-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "docprops.h"

#include "childfrm.h"
#include "schemes.h"

DocumentPropSheet::DocumentPropSheet(CChildFrame* pChild, LPCTSTR title) : 
	CPropertyPageImpl<DocumentPropSheet>(title),
	m_pChild(pChild),
	bModified(false)
{
}

DocumentPropSheet::~DocumentPropSheet()
{
	
}

bool DocumentPropSheet::ModifiedDocument()
{
	return bModified;
}

LRESULT DocumentPropSheet::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	Scheme* pCurScheme;

	CTextView* pView = m_pChild->GetTextView();
	pCurScheme = pView->GetCurrentScheme();
	
	CComboBox schemes(GetDlgItem(IDC_FILEPROP_SCHEME));
	
	SchemeManager* pM = SchemeManager::GetInstance();
	SCHEME_LIST* pSchemes = pM->GetSchemesList();

	Scheme* pDefScheme = pM->GetDefaultScheme();
	int index = schemes.AddString( pDefScheme->GetTitle() );
	schemes.SetItemDataPtr( index, pDefScheme );
	if( pCurScheme == pDefScheme )
		schemes.SetCurSel(index);

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		index = schemes.AddString( (*i).GetTitle() );
		schemes.SetItemDataPtr( index, &(*i) );
		if(pCurScheme == &(*i))
			schemes.SetCurSel(index);
	}

	CComboBox lineEndings(GetDlgItem(IDC_FILEPROP_LINEENDINGS));
	index = lineEndings.InsertString(0, _T("Windows (CRLF)"));
	lineEndings.SetItemData(index, PNSF_Windows);
	index = lineEndings.InsertString(1, _T("Unix (LF)"));
	lineEndings.SetItemData(index, PNSF_Unix);
	index = lineEndings.InsertString(2, _T("Macintosh (CR)"));
	lineEndings.SetItemData(index, PNSF_Mac);

	EPNSaveFormat actualLE = (EPNSaveFormat)pView->GetEOLMode();

	for(int j = 0; j < lineEndings.GetCount(); j++)
	{
		if( static_cast<EPNSaveFormat>(lineEndings.GetItemData(j)) == actualLE )
		{
			lineEndings.SetCurSel(j);
			break;
		}
	}

	EPNEncoding actualE = pView->GetEncoding();

	CComboBox encoding(GetDlgItem(IDC_FILEPROP_ENCODING));
	index = encoding.InsertString(0, _T("Default"));
	encoding.SetItemData(index, eUnknown);
	index = encoding.InsertString(1, _T("UTF-16 Big Endian"));
	encoding.SetItemData(index, eUtf16BigEndian);
	index = encoding.InsertString(2, _T("UTF-16 Little Endian"));
	encoding.SetItemData(index, eUtf16LittleEndian);
	index = encoding.InsertString(3, _T("UTF-8"));
	encoding.SetItemData(index, eUtf8);
	index = encoding.InsertString(4, _T("UTF-8 No BOM"));
	encoding.SetItemData(index, eUtf8NoBOM);

	for(int k = 0; k < encoding.GetCount(); k++)
	{
		if( static_cast<EPNEncoding>(encoding.GetItemData(k)) == actualE )
		{
			encoding.SetCurSel(k);
			break;
		}
	}

	TCHAR buf[15];

	_sntprintf(buf, 15, _T("%d"), pView->GetLineCount());
	::SetWindowText(GetDlgItem(IDC_FILEPROP_LINES), buf);

	_sntprintf(buf, 15, _T("%d"), pView->GetLength());
	::SetWindowText(GetDlgItem(IDC_FILEPROP_CHARS), buf);

	// Get word count...
	_sntprintf(buf, 15, _T("%d"), pView->GetWordCount());
	::SetWindowText(GetDlgItem(IDC_FILEPROP_WORDS), buf);

	_sntprintf(buf, 15, _T("%d"), pView->GetTabWidth());
	::SetWindowText(GetDlgItem(IDC_FILEPROP_TABWIDTH), buf);

	::SetWindowText(GetDlgItem(IDC_FILEPROP_FILENAME), m_pChild->GetFileName().c_str());

	// Disable controls if readonly
	GetDlgItem(IDC_FILEPROP_ENCODING).EnableWindow(!m_pChild->GetWriteProtect());
	GetDlgItem(IDC_FILEPROP_LINEENDINGS).EnableWindow(!m_pChild->GetWriteProtect());
	GetDlgItem(IDC_FILEPROP_TABWIDTH).EnableWindow(!m_pChild->GetWriteProtect());

	return 0;
}

BOOL DocumentPropSheet::OnApply()
{
	CTextView* pView = m_pChild->GetTextView();

	CComboBox schemes(GetDlgItem(IDC_FILEPROP_SCHEME));
	CComboBox lineEndings(GetDlgItem(IDC_FILEPROP_LINEENDINGS));
	CComboBox encoding(GetDlgItem(IDC_FILEPROP_ENCODING));

	Scheme* pScheme = reinterpret_cast<Scheme*>( schemes.GetItemData( schemes.GetCurSel() ) );

	if( pScheme != pView->GetCurrentScheme() )
		m_pChild->SetScheme( pScheme );

	EPNSaveFormat sf = (EPNSaveFormat)lineEndings.GetItemData( lineEndings.GetCurSel() );

	if( sf != pView->GetEOLMode() )
	{
		pView->SetEOLMode( sf );
		pView->ConvertEOLs( sf );
		bModified = true;
	}

	EPNEncoding enc = (EPNEncoding)encoding.GetItemData( encoding.GetCurSel() );

	if( enc != pView->GetEncoding() )
	{
		pView->SetEncoding(enc);
		bModified = true;
	}

	CWindowText wt(GetDlgItem(IDC_FILEPROP_TABWIDTH));
	int tabwidth = _ttoi( (LPCTSTR)wt );
	if( tabwidth != 0 && tabwidth != pView->GetTabWidth() )
		pView->SetTabWidth(tabwidth);

	return TRUE;
}