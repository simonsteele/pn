/**
 * @file sslistctrl.h
 * @brief A WTL list control subclass with some useful functionality.
 * @author Simon Steele
 * @note Copyright (c) 2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef sslistctrl_h__included
#define sslistctrl_h__included

/**
 * MoveItemUp and MoveItemDown: These functions are not the most efficient
 *     implementations of the functionality possible. They have to allocate
 *     arrays of data about the items moved around in order to store all of
 *     the column information. An alternative method is to use the sort, this
 *     can be faster / slower depending on the number of items in the list. That
 *     method also requires the data properties of each list item to be set 
 *     uniquely.
 */
class CSSListCtrl : public CListViewCtrl
{
	public:
		void MoveItemUp(int index)
		{
			EntireItemData_t* pEntireItem = getEntireItem(index);
			DeleteItem(index);
			index--;
			insertEntireItem(index, pEntireItem);
			freeEntireItem(pEntireItem);

			// Sort based moving:
			//ListCtrlMoveOneData_t mod = {reinterpret_cast<LPARAM>(pDef), 0, 0};
			//m_list.SortItems(ListCtrlMoveOneCompareFunc, reinterpret_cast<LPARAM>(&mod));
		}

		void MoveItemDown(int index)
		{
			EntireItemData_t* pEntireItem = getEntireItem(index);
			DeleteItem(index);
			index++;
			insertEntireItem(index, pEntireItem);
			freeEntireItem(pEntireItem);

			// Sort based moving:
			//ListCtrlMoveOneData_t mod = {reinterpret_cast<LPARAM>(pDef), 1, 0};
			//m_list.SortItems(ListCtrlMoveOneCompareFunc, reinterpret_cast<LPARAM>(&mod));
		}

		unsigned int GetColumnCount()
		{
			return GetHeader().GetItemCount();
		}

	protected:
		struct EntireItemData_t
		{
			LVITEM* pItems;
			UINT columns;
		};

		struct ListCtrlMoveOneData_t
		{
			LPARAM ItemData;
			bool bMoveDown;
			bool bDoneOne;
		};

		/**
		 * @brief Note this function doesn't work properly in the up direction.
		 */
		static int CALLBACK MoveOneCompareFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
		{
			ListCtrlMoveOneData_t* pD = reinterpret_cast<ListCtrlMoveOneData_t*>(lParamSort);
			LPARAM lpCompare = pD->bMoveDown ? lParam1 : lParam2;
			if( !pD->bDoneOne && lpCompare == pD->ItemData )
			{
				pD->bDoneOne = true;
				
				return 1;
			}
			return 0;
		}
		
		/**
		 * @brief return an array of LVITEMs representing an entire item with subitems
		 */
		EntireItemData_t* getEntireItem(int index)
		{
			ATLASSERT(m_hWnd != NULL);

			CString csBuffer;

			EntireItemData_t* pData = new EntireItemData_t;
			pData->columns = GetColumnCount();
			pData->pItems = new LVITEM[pData->columns];

			for(UINT i = 0; i < pData->columns; i++)
			{
				memset(&pData->pItems[i], 0, sizeof(LVITEM));
				pData->pItems[i].mask = LVIF_IMAGE | LVIF_PARAM | LVIF_STATE;
				pData->pItems[i].stateMask = (UINT)-1;
				pData->pItems[i].iItem = index;
				pData->pItems[i].iSubItem = i;
				
				GetItem(&pData->pItems[i]);
				
				GetItemText(index, i, csBuffer);

				pData->pItems[i].pszText = new TCHAR[csBuffer.GetLength()+1];
				_tcscpy(pData->pItems[i].pszText, (LPCTSTR)csBuffer);

				pData->pItems[i].mask |= LVIF_TEXT;
			}

			return pData;
		}

		/**
		 * @brief Insert a list item based on an EntireItemData_t object.
		 * @param pItem Item data to insert using.
		 * @param index Index to insert the item at.
		 */
		void insertEntireItem(int index, EntireItemData_t* pItem)
		{
			pItem->pItems[0].iItem = index;
			InsertItem(&pItem->pItems[0]);
			for(UINT i = 1; i < pItem->columns; i++)
			{
				pItem->pItems[i].iItem = index;
				pItem->pItems[i].mask = LVIF_IMAGE | LVIF_TEXT;
				SetItem(&pItem->pItems[i]);
			}
		}

		void freeEntireItem(EntireItemData_t* pData)
		{
			if(pData->pItems)
			{
				// Free text pointers...
				for(UINT i = 0; i < pData->columns; i++)
				{
					if( pData->pItems[i].pszText != NULL )
						delete pData->pItems[i].pszText;
				}

				delete pData->pItems;
			}
			
			delete pData;
		}
};

#endif