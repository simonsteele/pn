#ifndef optionspageextensions_h__included
#define optionspageextensions_h__included

#include "include/optionsdialog.h"

/**
 * Autocomplete options page
 */
class COptionsPageExtensions : public COptionsPageImpl<COptionsPageExtensions>
{
	public:
		COptionsPageExtensions();

		BEGIN_MSG_MAP(COptionsPageExtensions)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		enum { IDD = IDD_PAGE_EXTENSIONS };

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();
		virtual void OnCancel();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	private:
		CListViewCtrl m_list;
};

#endif // #ifndef optionspageextensions_h__included