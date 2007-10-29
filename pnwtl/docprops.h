/**
 * @file docprops.h
 * @brief Document properties property sheet
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef docprops_h__included
#define docprops_h__included

class CChildFrame;
class CSchemeCombo;

class DocumentPropSheet : public CPropertyPageImpl<DocumentPropSheet>
{
	friend class CPropertyPageImpl<DocumentPropSheet>;
public:
	DocumentPropSheet(CChildFrame* pChild, LPCTSTR title);
	~DocumentPropSheet();

	enum { IDD = IDD_FILEPROPERTIES };

	BEGIN_MSG_MAP(DocumentPropSheet)
        MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
        CHAIN_MSG_MAP(CPropertyPageImpl<DocumentPropSheet>)
    END_MSG_MAP()

	bool ModifiedDocument();

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	BOOL OnApply();

protected:
	CChildFrame*	m_pChild;
	bool			bModified;
};


#endif // #ifndef docprops_h__included