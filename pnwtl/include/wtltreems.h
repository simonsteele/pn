/**
 * @file WTLTreeMS.h
 * @brief Multiple-Selection TreeView for WTL
 * @author Simon Steele
 * @note Copyright (c) 2003 Simon Steele - http://untidy.net/
 *
 * This file is a port from MFC to WTL of the CTreeCtrlEx class 
 * written by Bendik Engebretsen <bendik@techsoft.no>.
 *
 * The following people are also credited in the original source
 * (search for CTreeCtrlEx on CodeGuru):
 *  Roel Schroeven, Yariv Elani, Yariv Ben-Tovim, Sergei Antonov,
 *  Cristian Rodriguez, Fernanda Diniz Tavarez, H.-Joachim Riedel.
 */
#ifndef wtltreeex_h__included
#define wtltreeex_h__included

#define TCEX_EDITLABEL 1

/**
 * Multiple-selection TreeView for WTL.
 */
class CMSTreeViewCtrl : public CWindowImpl<CMSTreeViewCtrl, CTreeViewCtrl>
{
	typedef CWindowImpl<CMSTreeViewCtrl, CTreeViewCtrl> baseClass;
public:
	DECLARE_WND_CLASS(_T("TreeEx"))

	CMSTreeViewCtrl()
	{
		selectPending = false;
		editPending = false;
		selectionComplete = true;
		m_hClickedItem = NULL;
		m_hFirstSelected = NULL;
		m_idTimer = 0;
	}

	BEGIN_MSG_MAP(CMSTreeViewCtrl)
		MESSAGE_HANDLER(WM_LBUTTONDOWN, OnLButtonDown)
		MESSAGE_HANDLER(WM_LBUTTONUP, OnLButtonUp)
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnLButtonDblClk)
		MESSAGE_HANDLER(WM_RBUTTONDOWN, OnRButtonDown)
		MESSAGE_HANDLER(WM_MOUSEMOVE, OnMouseMove)
		MESSAGE_HANDLER(WM_KEYDOWN, OnKeyDown)
		MESSAGE_HANDLER(WM_TIMER, OnTimer)
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDING, OnItemExpanding)
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnSelChanged)
	END_MSG_MAP()

	/**
	 * Clear all selections.
	 */
	void ClearSelection()
	{
		for ( HTREEITEM hItem = GetRootItem(); hItem != NULL; hItem = GetNextVisibleItem( hItem ) )
			if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
				SetItemState( hItem, 0, TVIS_SELECTED );
	}

	/**
	 * @return Number of selected items.
	 */
	UINT GetSelectedCount() const
	{
		// Only visible items should be selected!
		UINT uCount=0;
		for ( HTREEITEM hItem = GetRootItem(); hItem != NULL; hItem = GetNextVisibleItem( hItem ) )
			if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
				uCount++;

		return uCount;
	}

	/**
	 * Select/unselect item without unselecting other items
	 */
	BOOL SelectItemEx(HTREEITEM hItem, bool bSelect = true)
	{
		HTREEITEM hSelItem = GetSelectedItem();

		if ( hItem == hSelItem )
		{
			if ( !bSelect )
			{
				SelectItem( NULL );
				return TRUE;
			}

			return FALSE;
		}

		SelectItem( hItem );
		m_hFirstSelected = hItem;

		// Reselect previous "real" selected item which was unselected byt SelectItem()
		if ( hSelItem )
			SetItemState( hSelItem, TVIS_SELECTED, TVIS_SELECTED );

		return TRUE;
	}

	///////////////////////////////////////////////////////////////////////////////
	// Helpers to list out selected items. (Use similar to GetFirstVisibleItem(), 
	// GetNextVisibleItem() and GetPrevVisibleItem()!)

	/**
	 * Get the first selected item.
	 */
	HTREEITEM GetFirstSelectedItem()
	{
		for ( HTREEITEM hItem = GetRootItem(); hItem != NULL; hItem = GetNextVisibleItem( hItem ) )
			if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
			//if( GetItemState( hItem, TVIS_SELECTED | TVIS_DROPHILITED) == TVIS_SELECTED )
				return hItem;

		return NULL;
	}

	/**
	 * Get the next selected item.
	 * @param hItem The current selected item.
	 */
	HTREEITEM GetNextSelectedItem( HTREEITEM hItem )
	{
		for ( hItem = GetNextVisibleItem( hItem ); hItem!=NULL; hItem = GetNextVisibleItem( hItem ) )
			if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
			//if( GetItemState( hItem, TVIS_SELECTED | TVIS_DROPHILITED) == TVIS_SELECTED )
				return hItem;

		return NULL;
	}

	/**
	 * Get the previous selected item.
	 * @param hItem The current selected item.
	 */
	HTREEITEM GetPrevSelectedItem( HTREEITEM hItem )
	{
		for ( hItem = GetPrevVisibleItem( hItem ); hItem!=NULL; hItem = GetPrevVisibleItem( hItem ) )
			if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
			//if( GetItemState( hItem, TVIS_SELECTED | TVIS_DROPHILITED) == TVIS_SELECTED )
				return hItem;

		return NULL;
	}

// Message Handlers
protected:

	// Handle the mouse button press...
	LRESULT OnLButtonDown(UINT /*uMsg*/, WPARAM nFlags, LPARAM lParam, BOOL& bHandled)
	{
		CPoint point;
		point.x = GET_X_LPARAM(lParam); 
		point.y = GET_Y_LPARAM(lParam); 

		UINT nHitFlags = 0;
		HTREEITEM hClickedItem = HitTest( point, &nHitFlags );

		// Must invoke label editing explicitly. The base class OnLButtonDown would normally
		// do this, but we can't call it here because of the multiple selection...
		if( !( nFlags&( MK_CONTROL|MK_SHIFT ) ) && 
			( GetStyle() & TVS_EDITLABELS ) && 
			( nHitFlags & TVHT_ONITEMLABEL ) )
		{
			// We check to see if we have the focus, because if we don't then the
			// user clicked elsewhere between clicks so we shouldn't edit.
			if ( hClickedItem == GetSelectedItem() && ::GetFocus() == m_hWnd )
			{
				// Clear multple selection before label editing
				//ClearSelection();
				//SelectItem( hClickedItem );
				m_hClickedItem = hClickedItem;

				// Invoke label editing
				editPending = TRUE;
				m_idTimer = SetTimer(TCEX_EDITLABEL, GetDoubleClickTime(), NULL);

				return 0;
			}
		}

		editPending = FALSE;

		if( nHitFlags & TVHT_ONITEM )
		{
			SetFocus();

			m_hClickedItem = hClickedItem;

			// Is the clicked item already selected ?
			BOOL bIsClickedItemSelected = GetItemState( hClickedItem, TVIS_SELECTED ) & TVIS_SELECTED;

			if ( bIsClickedItemSelected )
			{
				// Maybe user wants to drag/drop multiple items!
				// So, wait until OnLButtonUp() to do the selection stuff. 
				selectPending = true;
			}
			else
			{
				SelectMultiple( m_hClickedItem, nFlags, point );
				selectPending = false;
			}

			m_ptLastClick = point;
		}
		else
			bHandled = FALSE;

		return 0;
	}

	LRESULT OnLButtonUp(UINT /*uMsg*/, WPARAM nFlags, LPARAM lParam, BOOL& bHandled)
	{
		CPoint point;
		point.x = GET_X_LPARAM(lParam); 
		point.y = GET_Y_LPARAM(lParam); 

		if ( selectPending )
		{
			// A select has been waiting to be performed here
			SelectMultiple( m_hClickedItem, nFlags, point );
			selectPending = false;
		}

		m_hClickedItem = NULL;

		bHandled = FALSE;

		return 0;
	}

	LRESULT OnLButtonDblClk(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		editPending = false;
		bHandled = FALSE;
		KillTimer(m_idTimer);

		return 0;
	}

	LRESULT OnRButtonDown(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = FALSE;

		// Cancel any edit.
		editPending = false;
		// Cancel any select.
		selectPending = false;
		
		CPoint point;
		point.x = GET_X_LPARAM(lParam); 
		point.y = GET_Y_LPARAM(lParam); 

		UINT nHitFlags = 0;
		HTREEITEM hClickedItem = HitTest( point, &nHitFlags );

		if( nHitFlags & TVHT_ONITEM )
		{

			// Is the item that was right-clicked selected?
			BOOL bIsClickedItemSelected = GetItemState( hClickedItem, TVIS_SELECTED ) & TVIS_SELECTED;

			if ( bIsClickedItemSelected )
			{
				// If it is, then we let events proceed...
			}
			else
			{
				ClearSelection();
				SelectItem( hClickedItem );
			}
		}

		return 0;
	}

	/**
	 * If there is a select pending, check if cursor has moved so much away from the 
	 * down-click point that we should cancel the pending select and initiate
	 * a drag/drop operation instead!
	 */
	LRESULT OnMouseMove(UINT /*uMsg*/, WPARAM nFlags, LPARAM lParam, BOOL& bHandled)
	{
		CPoint point;
		point.x = GET_X_LPARAM(lParam); 
		point.y = GET_Y_LPARAM(lParam); 

		if ( m_hClickedItem )
		{
			CSize sizeMoved = m_ptLastClick-point;

			if ( abs(sizeMoved.cx) > GetSystemMetrics( SM_CXDRAG ) || 
				abs(sizeMoved.cy) > GetSystemMetrics( SM_CYDRAG ) )
			{
				KillTimer(m_idTimer);
				selectPending = false;
				editPending = false;

				// Notify parent that he may begin drag operation
				// Since we have taken over OnLButtonDown(), the default handler doesn't
				// do the normal work when clicking an item, so we must provide our own
				// TVN_BEGINDRAG notification for the parent!

				HWND hWndParent = GetParent();
				if ( hWndParent && !( GetStyle() & TVS_DISABLEDRAGDROP ) )
				{
					NM_TREEVIEW tv;

					tv.hdr.hwndFrom = m_hWnd;
					tv.hdr.idFrom = GetWindowLong( GWL_ID );
					tv.hdr.code = TVN_BEGINDRAG;

					tv.itemNew.hItem = m_hClickedItem;
					tv.itemNew.state = GetItemState( m_hClickedItem, 0xffffffff );
					tv.itemNew.lParam = GetItemData( m_hClickedItem );

					tv.ptDrag.x = point.x;
					tv.ptDrag.y = point.y;

					::SendMessage( hWndParent, WM_NOTIFY, tv.hdr.idFrom, (LPARAM)&tv );
				}

				m_hClickedItem = NULL;
			}
		}
	
		bHandled = FALSE;

		return 0;
	}

	LRESULT OnTimer(UINT /*uMsg*/, WPARAM nIDEvent, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if (nIDEvent == TCEX_EDITLABEL)
		{
			// Stop the timer.
			KillTimer(m_idTimer);

			// Invoke label editing.
			if (editPending)
			{
				// Remove multiple selection before editing...
				HTREEITEM hSelectedItem = GetSelectedItem();
				ClearSelection();
				SelectItem( hSelectedItem );
				m_hClickedItem = NULL;
				
				EditLabel(GetSelectedItem());
			}

			editPending = FALSE;

			return 0;
		}

		bHandled = FALSE;

		return 0;
	}

	/**
	 * Handle key movement to allow multiple selections.
	 * //TODO: Disparate selection.
	 */
	LRESULT OnKeyDown(UINT uMsg, WPARAM nChar, LPARAM lParam, BOOL& bHandled)
	{
		/*SHORT nRepCnt = HIWORD(lParam);
		SHORT nFlags = LOWORD(lParam);*/

		HWND hWndParent = GetParent();

		if ( nChar == VK_NEXT || nChar == VK_PRIOR )
		{
			if ( !( GetKeyState( VK_SHIFT ) & 0x8000 ) )
			{
				// User pressed Pg key without holding 'Shift':
				// Clear multiple selection (if multiple) and let base class do 
				// normal selection work!
				if ( GetSelectedCount() > 1 )
					ClearSelection( );

				bHandled = FALSE;
				m_hFirstSelected = GetSelectedItem();
				return 0;
			}

			// Flag signaling that selection process is NOT complete.
			// (Will prohibit TVN_SELCHANGED from being sent to parent)
			selectionComplete = FALSE;

			// Let base class select the item
			DefWindowProc( uMsg, nChar, lParam );
			HTREEITEM hSelectedItem = GetSelectedItem();

			// Then select items in between
			SelectItems( m_hFirstSelected, hSelectedItem );

			// Selection process is now complete. Since we have 'eaten' the TVN_SELCHANGED 
			// notification provided by Windows' treectrl, we must now produce one ourselves,
			// so that our parent gets to know about the change of selection.
			selectionComplete = TRUE;

			if (hWndParent)
			{
				NM_TREEVIEW tv;
				memset(&tv.itemOld, 0, sizeof(tv.itemOld));

				tv.hdr.hwndFrom = m_hWnd;
				tv.hdr.idFrom = GetWindowLong(GWL_ID);
				tv.hdr.code = TVN_SELCHANGED;

				tv.itemNew.hItem = hSelectedItem;
				tv.itemNew.state = GetItemState(hSelectedItem, 0xffffffff);
				tv.itemNew.lParam = GetItemData(hSelectedItem);
				tv.itemNew.mask = TVIF_HANDLE|TVIF_STATE|TVIF_PARAM;

				tv.action = TVC_UNKNOWN;

				::SendMessage(hWndParent, WM_NOTIFY, tv.hdr.idFrom, (LPARAM)&tv);
			}

			return 0;
		}
		else if ( nChar==VK_UP || nChar==VK_DOWN )
		{
			// Find which item is currently selected
			HTREEITEM hSelectedItem = GetSelectedItem();

			HTREEITEM hNextItem;
			if ( nChar == VK_UP )
				hNextItem = GetPrevVisibleItem( hSelectedItem );
			else
				hNextItem = GetNextVisibleItem( hSelectedItem );

			if ( !( GetKeyState( VK_SHIFT )&0x8000 ) )
			{
				// User pressed arrow key without holding 'Shift':
				// Clear multiple selection (if multiple) and let base class do 
				// normal selection work!
				if ( GetSelectedCount() > 1 )
					ClearSelection(  );

				if ( hNextItem )
					DefWindowProc(uMsg, nChar, lParam); //CTreeCtrl::OnKeyDown( nChar, nRepCnt, nFlags );
					
				m_hFirstSelected = GetSelectedItem();
				return 0;
			}

			if ( hNextItem )
			{
				// Flag signaling that selection process is NOT complete.
				// (Will prohibit TVN_SELCHANGED from being sent to parent)
				selectionComplete = FALSE;

				// If the next item is already selected, we assume user is
				// "moving back" in the selection, and thus we should clear 
				// selection on the previous one
				BOOL bSelect = !( GetItemState( hNextItem, TVIS_SELECTED ) & TVIS_SELECTED );

				// Select the next item (this will also deselect the previous one!)
				SelectItem( hNextItem );

				// Now, re-select the previously selected item
				if ( bSelect || ( !( GetItemState( hSelectedItem, TVIS_SELECTED ) & TVIS_SELECTED ) ) )
					SelectItems( m_hFirstSelected, hNextItem );

				// Selection process is now complete. Since we have 'eaten' the TVN_SELCHANGED 
				// notification provided by Windows' treectrl, we must now produce one ourselves,
				// so that our parent gets to know about the change of selection.
				selectionComplete = TRUE;

				if (hWndParent)
				{
					NM_TREEVIEW tv;
					memset(&tv.itemOld, 0, sizeof(tv.itemOld));

					tv.hdr.hwndFrom = m_hWnd;
					tv.hdr.idFrom = GetWindowLong(GWL_ID);
					tv.hdr.code = TVN_SELCHANGED;

					tv.itemNew.hItem = hNextItem;
					tv.itemNew.state = GetItemState(hNextItem, 0xffffffff);
					tv.itemNew.lParam = GetItemData(hNextItem);
					tv.itemNew.mask = TVIF_HANDLE|TVIF_STATE|TVIF_PARAM;

					tv.action = TVC_UNKNOWN;

					::SendMessage(m_hWnd, WM_NOTIFY, tv.hdr.idFrom, (LPARAM)&tv);
				}
			}

			// Since the base class' OnKeyDown() isn't called in this case,
			// we must provide our own TVN_KEYDOWN notification to the parent

			HWND hWndParent = GetParent();
			if ( hWndParent )
			{
				NMTVKEYDOWN tvk;

				tvk.hdr.hwndFrom = m_hWnd;
				tvk.hdr.idFrom = GetWindowLong( GWL_ID );
				tvk.hdr.code = TVN_KEYDOWN;

				tvk.wVKey = nChar;
				tvk.flags = 0;

				::SendMessage( hWndParent, WM_NOTIFY, tvk.hdr.idFrom, (LPARAM)&tvk );
			}

			return 0;
		}
		else
			// Behave normally
			return DefWindowProc(uMsg, nChar, lParam); //CTreeCtrl::OnKeyDown( nChar, nRepCnt, nFlags );
	}

	LRESULT OnItemExpanding(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
	{
		NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pnmh;

		if ( pNMTreeView->action == TVE_COLLAPSE )
		{
			HTREEITEM hItem = GetChildItem( pNMTreeView->itemNew.hItem );

			while ( hItem )
			{
				if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
					SetItemState( hItem, 0, TVIS_SELECTED );

				// Get the next node: First see if current node has a child
				HTREEITEM hNextItem = GetChildItem( hItem );
				if ( !hNextItem )
				{
					// No child: Get next sibling item
					if ( !( hNextItem = GetNextSiblingItem( hItem ) ) )
					{
						HTREEITEM hParentItem = hItem;
						while ( !hNextItem )
						{
							// No more children: Get parent
							if ( !( hParentItem = GetParentItem( hParentItem ) ) )
								break;

							// Quit when parent is the collapsed node
							// (Don't do anything to siblings of this)
							if ( hParentItem == pNMTreeView->itemNew.hItem )
								break;

							// Get next sibling to parent
							hNextItem = GetNextSiblingItem( hParentItem );
						}

						// Quit when parent is the collapsed node
						if ( hParentItem == pNMTreeView->itemNew.hItem )
							break;
					}
				}

				hItem = hNextItem;
			}
		}
		
		bHandled = FALSE;	// Allow parent to handle this notification as well
	
		return 0;
	}

	LRESULT OnSelChanged(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
	{
		if(selectionComplete)
			bHandled = FALSE;

		return 0;
	}

// Our Code:
protected:

	void SelectMultiple( HTREEITEM hClickedItem, UINT nFlags, CPoint point )
	{
		// Start preparing an NM_TREEVIEW struct to send a notification after selection is done
		NM_TREEVIEW tv;
		memset(&tv.itemOld, 0, sizeof(tv.itemOld));

		HWND hWndParent = GetParent();

		HTREEITEM hOldItem = GetSelectedItem();

		if ( hOldItem )
		{
			tv.itemOld.hItem = hOldItem;
			tv.itemOld.state = GetItemState( hOldItem, 0xffffffff );
			tv.itemOld.lParam = GetItemData( hOldItem );
			tv.itemOld.mask = TVIF_HANDLE|TVIF_STATE|TVIF_PARAM;
		}

		// Flag signaling that selection process is NOT complete.
		// (Will prohibit TVN_SELCHANGED from being sent to parent)
		selectionComplete = false;

		// Action depends on whether the user holds down the Shift or Ctrl key
		if ( nFlags & MK_SHIFT )
		{
			// Select from first selected item to the clicked item
			if ( !m_hFirstSelected )
				m_hFirstSelected = GetSelectedItem();

			SelectItems( m_hFirstSelected, hClickedItem );
		}
		else if ( nFlags & MK_CONTROL )
		{
			// Find which item is currently selected
			HTREEITEM hSelectedItem = GetSelectedItem();

			// Is the clicked item already selected ?
			BOOL bIsClickedItemSelected = GetItemState( hClickedItem, TVIS_SELECTED ) & TVIS_SELECTED;
			BOOL bIsSelectedItemSelected = FALSE;
			if ( hSelectedItem )
				bIsSelectedItemSelected = GetItemState( hSelectedItem, TVIS_SELECTED ) & TVIS_SELECTED;

			// Must synthesize a TVN_SELCHANGING notification
			if ( hWndParent )
			{
				tv.hdr.hwndFrom = m_hWnd;
				tv.hdr.idFrom = GetWindowLong( GWL_ID );
				tv.hdr.code = TVN_SELCHANGING;

				tv.itemNew.hItem = hClickedItem;
				tv.itemNew.state = GetItemState( hClickedItem, 0xffffffff );
				tv.itemNew.lParam = GetItemData( hClickedItem );

				tv.itemOld.hItem = NULL;
				tv.itemOld.mask = 0;

				tv.action = TVC_BYMOUSE;

				tv.ptDrag.x = point.x;
				tv.ptDrag.y = point.y;

				::SendMessage( hWndParent, WM_NOTIFY, tv.hdr.idFrom, (LPARAM)&tv );
			}

			// If the previously selected item was selected, re-select it
			if ( bIsSelectedItemSelected )
				SetItemState( hSelectedItem, TVIS_SELECTED, TVIS_SELECTED );

			// We want the newly selected item to toggle its selected state,
			// so unselect now if it was already selected before
			if ( bIsClickedItemSelected )
				SetItemState( hClickedItem, 0, TVIS_SELECTED );
			else
			{
				SelectItem(hClickedItem);
				SetItemState( hClickedItem, TVIS_SELECTED, TVIS_SELECTED );
			}

			// If the previously selected item was selected, re-select it
			if ( bIsSelectedItemSelected && hSelectedItem != hClickedItem )
				SetItemState( hSelectedItem, TVIS_SELECTED, TVIS_SELECTED );

			// Store as first selected item (if not already stored)
			if ( m_hFirstSelected == NULL )
				m_hFirstSelected = hClickedItem;
		}
		else
		{
			// Clear selection of all "multiple selected" items first
			ClearSelection();

			// Then select the clicked item
			SelectItem( hClickedItem );
			SetItemState( hClickedItem, TVIS_SELECTED, TVIS_SELECTED );

			// Store as first selected item
			m_hFirstSelected = hClickedItem;
		}

		// Selection process is now complete. Since we have 'eaten' the TVN_SELCHANGED 
		// notification provided by Windows' treectrl, we must now produce one ourselves,
		// so that our parent gets to know about the change of selection.
		selectionComplete = TRUE;

		if ( hWndParent )
		{
			tv.hdr.hwndFrom = m_hWnd;
			tv.hdr.idFrom = GetWindowLong( GWL_ID );
			tv.hdr.code = TVN_SELCHANGED;

			tv.itemNew.hItem = m_hClickedItem;
			tv.itemNew.state = GetItemState( m_hClickedItem, 0xffffffff );
			tv.itemNew.lParam = GetItemData( m_hClickedItem );
			tv.itemNew.mask = TVIF_HANDLE|TVIF_STATE|TVIF_PARAM;

			tv.action = TVC_UNKNOWN;

			::SendMessage( hWndParent, WM_NOTIFY, tv.hdr.idFrom, (LPARAM)&tv );
		}
	}

	/**
	 * Select visible items between specified 'from' and 'to' item (including these!)
	 * If the 'to' item is above the 'from' item, it traverses the tree in reverse 
	 * direction. Selection on other items is cleared!
	 */
	BOOL SelectItems( HTREEITEM hFromItem, HTREEITEM hToItem )
	{
		// Determine direction of selection 
		// (see what item comes first in the tree)
		HTREEITEM hItem = GetRootItem();

		while ( hItem && hItem != hFromItem && hItem != hToItem )
			hItem = GetNextVisibleItem( hItem );

		if ( !hItem )
			return FALSE;	// Items not visible in tree

		BOOL bReverse = (hItem == hToItem);

		// "Really" select the 'to' item (which will deselect 
		// the previously selected item)

		SelectItem( hToItem );

		// Go through all visible items again and select/unselect

		hItem = GetRootItem();
		BOOL bSelect = FALSE;

		while ( hItem )
		{
			if ( hItem == ( bReverse ? hToItem : hFromItem ) )
				bSelect = TRUE;

			if ( bSelect )
			{
				if ( !( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED ) )
					SetItemState( hItem, TVIS_SELECTED, TVIS_SELECTED );
			}
			else
			{
				if ( GetItemState( hItem, TVIS_SELECTED ) & TVIS_SELECTED )
					SetItemState( hItem, 0, TVIS_SELECTED );
			}

			if ( hItem == ( bReverse ? hFromItem : hToItem ) )
				bSelect = FALSE;

			hItem = GetNextVisibleItem( hItem );
		}

		return TRUE;
	}

protected:
	CPoint		m_ptLastClick;		// the last point a LButtonDown was received for.
	bool		selectPending;		// is there a selection pending?
	bool		editPending;		// are we pending an edit?
	bool		selectionComplete;	// is the selection process complete?
	HTREEITEM	m_hFirstSelected;	// first selected item.
	HTREEITEM	m_hClickedItem;		// last clicked item.
	UINT		m_idTimer;
};

#endif