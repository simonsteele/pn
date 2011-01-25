/**
 * @file optionspagekeyboard.h
 * @brief Options Dialog Keyboard Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspagekeyboard_h__included
#define optionspagekeyboard_h__included

#include "include/optionsdialog.h"
#include "optionscontrols.h"
#include "controls/OptionsBlockHeader.h"

typedef enum { cdtCommand, cdtExtended, cdtScintilla } CommandDetailsType;

typedef struct CommandDetails_tag
{
	CommandDetailsType type;
	DWORD command;
} CommandDetails;

typedef struct ExtendedCommandDetails_tag : CommandDetails
{
	std::string group;
	std::string name;
} ExtendedCommandDetails;

class COptionsPageKeyboard : public COptionsPageImpl<COptionsPageKeyboard>,
								public CWinDataExchange<COptionsPageKeyboard>
{
	public:
		COptionsPageKeyboard(CommandDispatch* dispatcher);
		~COptionsPageKeyboard();

		BEGIN_MSG_MAP(COptionsPageKeyboard)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			
			COMMAND_HANDLER(IDC_KB_ADD, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_KB_REMOVE, BN_CLICKED, OnRemoveClicked)
			COMMAND_HANDLER(IDC_KB_HOTKEY, EN_CHANGE, OnHotKeyChanged)
			COMMAND_HANDLER(IDC_KB_ASSIGNEDLIST, LBN_SELCHANGE, OnKeySelChanged)

			NOTIFY_HANDLER(IDC_KB_COMMANDS, LVN_ITEMCHANGED, OnListItemChanged)
			
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_KEYBOARD };

		BEGIN_DDX_MAP(COptionsPageKeyboard)
			
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();
		virtual void OnCancel();
		
		bool IsDirty() const;

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnHotKeyChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnKeySelChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void addDynamicItems(CSMenuHandle& menu);
		void addItem(int index, LPCTSTR group, LPCTSTR item, DWORD command);
		int addItems(CSMenuHandle& menu, LPCTSTR group, int count = 0);
		void addExtensions();
		void addScintilla();
		void clear();
		void enableButtons();
		tstring findCommandName(DWORD command);
		tstring findCommandName(const std::string& extcommand);
		void updateSelection();
		void showCommandSelection(CommandDetails* command);
		void showExtendedSelection(ExtendedCommandDetails* command);
		void cleanUp();
		bool currentIsExtended();
		bool currentIsScintilla();

		CommandDetails* m_pCurrent;
		Commands::KeyMap* m_pKeyMap;
		Commands::KeyMap* m_pScintillaMap;
		CommandDispatch*m_pDispatch;
		CListViewCtrl	m_list;
		CListBox		m_shortcutlist;
		CPNHotkeyCtrl	m_hotkey;
		bool			m_bDirty;

		COptionsBlockHeader m_settingsHeader;
};

#endif