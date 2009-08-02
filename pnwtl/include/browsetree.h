#ifndef browsetree_h__included
#define browsetree_h__included

class CBrowseTree : public CWindowImpl<CBrowseTree, CTreeViewCtrl>
{
	typedef CWindowImpl<CBrowseTree, CTreeViewCtrl> baseClass;
	typedef CBrowseTree thisClass;

	public:
		DECLARE_WND_CLASS(_T("BrowseTree"))

		CBrowseTree()
		{
			// Setup defaults.
			m_dwDontShow	 = FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_TEMPORARY | FILE_ATTRIBUTE_OFFLINE;
			m_bShowFiles	 = TRUE;
			m_bDoubleBuffer  = FALSE;
			m_bSilent	 = FALSE;
			m_strFilter	 = _T( "*.*" );
			m_strLoading	 = _T( "Loading..." );
			m_crLoadingColor = CLR_DEFAULT;
		}

		BEGIN_MSG_MAP(thisClass)
			MESSAGE_HANDLER(WM_PRINTCLIENT, OnPrintClient)
			//MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
			MESSAGE_HANDLER(WM_PAINT, OnPaint)
			NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDING, OnItemExpanding)
			NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDED, OnItemExpanded)
			REFLECTED_NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDING, OnItemExpanding)
			REFLECTED_NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDED, OnItemExpanded)
		END_MSG_MAP()

		// Selection types as returned by
		// TypeOfSelection().
		enum 
		{
			STYPE_FILE = 0,
			STYPE_DIRECTORY,
			STYPE_UNKNOWN,
			STYPE_NONE
		};

		// Implementation.
		BOOL SetupTree( LPCTSTR pszRoot = NULL )
		{
			ATLASSERT( ::IsWindow(m_hWnd) ); // Must be valid.

			// Make sure the tree is empty...
			if ( ! m_bSilent ) 
				SetRedraw( FALSE );
			
			SelectItem( NULL );
			DeleteAllItems();
			
			if ( ! m_bSilent ) 
				SetRedraw( TRUE );
			
			TCHAR drive = GetFirstDriveLetter();
			tstring path;
			path += drive;
			path += _T(":\\");

			// Dirty trick to get the system image list...
			SHFILEINFO fi;
			HIMAGELIST hSystemImages = ( HIMAGELIST )::SHGetFileInfo( path.c_str(), 0, &fi, sizeof( fi ), SHGFI_SYSICONINDEX | SHGFI_SMALLICON );
			if ( hSystemImages == NULL )
				return FALSE;

			// Do we need to set the images?
			if ( GetImageList( TVSIL_NORMAL ) != hSystemImages )
				SetImageList( hSystemImages, TVSIL_NORMAL );

			// Make sure we are the LVS_EDITLABELS style is not
			// specified.
			ModifyStyle( TVS_EDITLABELS, TVS_LINESATROOT );

			// Use a specific root?
			if ( pszRoot && _tcslen( pszRoot ))
			{
				// Copy initial root.
				m_strInitial = pszRoot;

				// Make sure the backslash is present.
				CString str = m_strInitial;
				if ( str.GetLength() && str[ str.GetLength() - 1 ] != _T( '\\' )) 
					str += _T( '\\' );

				// Add the root item.
				HTREEITEM hItem = AddTreeItem( TVI_ROOT, str, FALSE );
				if ( hItem == NULL )
					return FALSE;

				// Does it have any files/dirs?
				if ( HasEntries( hItem ))
				{
					// Add a single empty item.
					if ( AddTreeItem( hItem, _T( "" )) == NULL )
						return FALSE;
				}
				return TRUE;
			}

			// No specific root. Add all logical drives.
			return DisplayDrives();
		}
		
		BOOL GetSelectedPath( CString& strPath ) const
		{
			ATLASSERT( ::IsWindow(m_hWnd) ); // Must be valid.

			// Get info about the selected entry.
			TVITEM tvi;
			tvi.mask = TVIF_STATE;
			tvi.hItem = GetSelectedItem();

			if ( GetItem( &tvi ))
			{
				// Obtain it's path.
				strPath = GetPath( tvi.hItem );

				// Is it a drive?
				if ( tvi.lParam )
					return TRUE;

				// Can we get it's attributes?
				DWORD dwAttr = ::GetFileAttributes( strPath );
				if ( dwAttr == 0xFFFFFFFF || ( ! ( dwAttr & FILE_ATTRIBUTE_DIRECTORY )))
				{
					// Does it have a pending backslash?
					if ( strPath[ strPath.GetLength() - 1 ] == _T( '\\' )) 
					{
						// Strip it and try again.
						strPath.Truncate( strPath.GetLength() - 1 );
						if (( dwAttr = ::GetFileAttributes( strPath )) == 0xFFFFFFFF )
							return FALSE;
					}
				}
				return TRUE;
			}
			return FALSE;
		}

		BOOL SelectPath( LPCTSTR pszPath )
		{
			return SelectPath( CString(pszPath) );
		}

		BOOL SelectPath( CString& strPath )
		{
			ATLASSERT( ::IsWindow(m_hWnd) ); // Must be valid.

			// 4KChars max.
			TCHAR	szBuffer[ 4096 ];

			// Setup the structure to obtain the item
			// text.
			TVITEM	tvi;
			tvi.mask	= TVIF_TEXT | TVIF_PARAM;
			tvi.pszText	= szBuffer;

			// We start at the root.
			HTREEITEM hParent = TVI_ROOT;

			// Maximum index into the path.
			int nMax = strPath.GetLength() - 1;

			// Setup data.
			int nPos = 0;
			CString strComp;
			HTREEITEM hSelected = NULL;

			// Iterate the path to get it's components.
			for ( ;; )
			{
				// Get the index of the next backslash.
				int nPos2 = strPath.Find( _T( '\\' ), nPos );

				// Extract the path component.
				strComp = strPath.Mid( nPos, nPos2 == -1 ? nPos2 : nPos2 - nPos );

				// Obtain the first child item.
				HTREEITEM hChild = GetChildItem( hParent );
				if ( hChild )
				{
					// Iterate children.
					do
					{
						// Setup structure.
						tvi.hItem      = hChild;
						tvi.cchTextMax = 4096;

						// Obtain item name.
						if ( GetItem( &tvi ))
						{
							// Is it a drive name?
							if ( tvi.lParam )
							{
								// Build "real" path string.
								_tcscpy( szBuffer, _T( " :" ));

								// Add drive letter.
								szBuffer[ 0 ] = ( TCHAR )( tvi.lParam );
							}

							// Is this the item we are looking for?
							if ( _tcsicmp( szBuffer, strComp ) == 0 )
							{
								// Expand the item.
								Expand( hChild, TVE_EXPAND );

								// Make this child the new parent.
								hParent = hChild;

								// Were we processing the final
								// part of the path?
								if ( nPos2 == -1 || nPos2 == nMax )
								{
									// Mark the selected item.
									hSelected = hChild;

									// Yes. Select the item.
									SelectItem( hChild );
								}
								break;
							}
							// Get the next child.
							hChild = GetNextSiblingItem( hChild );
						}
						else
							// Failure...
							break;
					} while ( hChild );
				}

				// Did we process the last part of the path?
				if ( nPos2 == -1 || nPos2 == nMax )
				{
					// Did we select an item?
					if ( hSelected == NULL )
					{
						// No. Simply select the parent if it is
						// not the root.
						if ( hParent != TVI_ROOT )
							SelectItem( hParent );
					}
					break;
				}

				// Skip to the next part of the path.
				nPos = nPos2 + 1;
			}
			return TRUE;
		}
		
		void ShowFiles( BOOL bShow = TRUE )
		{
			// We must have contents.
			if ( GetRootItem() == NULL ) 
			{
				// Simply set the value.
				m_bShowFiles = bShow;
				return;
			}

			// Did the mode change?
			if ( bShow != m_bShowFiles )
			{
				// Turn of rendering.
				SetRedraw( FALSE );
				m_bSilent = TRUE;
					
				// Set new value.
				m_bShowFiles = bShow;
				
				// Get current selection.
				CString path;
				GetSelectedPath( path );

				// Reset the contents.
				SetupTree( m_strInitial );

				// Select the path.
				SelectPath( path );

				// Re-render.
				m_bSilent = FALSE;
				SetRedraw( TRUE );
				UpdateWindow();
			}
		}
		
		int TypeOfSelection() const
		{
			ATLASSERT( ::IsWindow(m_hWnd) ); // Must be valid.

			// Any selection?
			if ( GetSelectedItem())
			{
				// Get the path.
				CString strPath;
				if ( GetSelectedPath( strPath ))
				{
					// Determine wether it is a file or a directory.
					DWORD dwAttr = ::GetFileAttributes( strPath );
					if ( dwAttr == 0xFFFFFFFF )
					{
						// Does it have a pending backslash?
						if ( strPath[ strPath.GetLength() - 1 ] == _T( '\\' )) 
						{
							// Strip it and try again.
							
							strPath.Truncate(strPath.GetLength() - 1);
							if (( dwAttr = ::GetFileAttributes( strPath )) == 0xFFFFFFFF )
								return STYPE_UNKNOWN;
						}
					}

					// Is it a directory?
					if ( dwAttr & FILE_ATTRIBUTE_DIRECTORY ) 
						return STYPE_DIRECTORY;
					else
						return STYPE_FILE;
				}
			}
			return STYPE_NONE;
		}

		BOOL GetDoubleBuffer() { return m_bDoubleBuffer; }
		void SetDoubleBuffer(bool bDoubleBuffer) { m_bDoubleBuffer = bDoubleBuffer; }
		
		DWORD GetDisplayMask() { return m_dwDontShow; }
		void SetDisplayMask(DWORD dwVal){ m_dwDontShow = dwVal; }
		
		CString GetFileFilter(){ return m_strFilter; }
		void SetFileFilter(LPCTSTR filter){ m_strFilter = filter; }
		
		CString GetLoadingText(){ return m_strLoading; }
		void SetLoadingText(LPCTSTR text){ m_strLoading = text; }
		
		COLORREF GetLoadingTextColor(){ return m_crLoadingColor; }
		void SetLoadingTextColor(COLORREF color){ m_crLoadingColor = color; }

	protected:
		// The default way of rendering the item displayed
		// during the loading of the tree. When this overridable
		// is called the device context has already been setup 
		// to use the treeview font.
		virtual void OnRenderLoadingItem( CDC *pDC, LPRECT lprcBounds )
		{
			// Setup the color to use for rendering the
			// loading item text. If the color specified equals
			// -1 we get the control it's text color. If this 
			// equals -1 aswell we use the system window text color.
			COLORREF color = ( m_crLoadingColor == CLR_DEFAULT ? GetTextColor() : m_crLoadingColor );
			if ( color == CLR_DEFAULT ) color = ::GetSysColor( COLOR_WINDOWTEXT );
			COLORREF crold = pDC->SetTextColor( color );

			// Obtain font height.
			CSize size;
			pDC->GetTextExtent( _T( " " ), 1, &size );

			CRect crBounds = lprcBounds;

			// Render the text...
			pDC->TextOut( crBounds.left + 2, crBounds.top + (( crBounds.Height() / 2 ) - ( size.cy / 2 )), m_strLoading );

			// Restore text color.
			pDC->SetTextColor( crold );
		}
		
		virtual void OnDriveEmpty( CString strPath ){}

		// Helpers.
		BOOL DisplayDrives()
		{
			// Delete current contents.
			DeleteAllItems();

			// Get the available logical drives.
			DWORD dwDrives = ::GetLogicalDrives(), dwMask = 1;
			for ( int i = 0; i < 32; i++, dwMask <<= 1 )
			{
				// Drive available?
				if ( dwDrives & dwMask )
				{
					// Create drive string.
					CString drive;
					drive.Format( _T( "%c:\\" ), _T( 'A' ) + i );

					// We do not want to see error dialogs...
					DWORD dwOldMode = SetErrorMode( SEM_FAILCRITICALERRORS );

					// Obtain display name.
					SHFILEINFO sfi;
					SHGetFileInfo( drive, 0, &sfi, sizeof( sfi ), SHGFI_DISPLAYNAME );

					// Reset error mode.
					SetErrorMode( dwOldMode );

					// Add the drive to the list.
					HTREEITEM hDrive;
					if (( hDrive = AddTreeItem( TVI_ROOT, drive, TRUE, sfi.szDisplayName )) == NULL )
						return FALSE;

					// Add an empty item so we get the LVN_ITEMEXPANDED
					// notification message which we use to load or
					// remove the directory entries.
					if ( AddTreeItem( hDrive, _T("") ) == NULL )
						return FALSE;
				}
			}
			return TRUE;
		}

		// Add a treeview item.
		HTREEITEM AddTreeItem( HTREEITEM hParent, LPCTSTR pszPath, BOOL Parse = TRUE, LPCTSTR pszDriveName = NULL )
		{
			TVINSERTSTRUCT tvi;
			SHFILEINFO fi;

			// Get the index of the normal icon.
			if ( ::SHGetFileInfo( pszPath, 0, &fi, sizeof( fi ), SHGFI_ICON | SHGFI_SMALLICON ) == 0 )
				return NULL;
			
			// Store the index.
			tvi.item.iImage = fi.iIcon;
			::DestroyIcon( fi.hIcon );

			// Get the index of the open icon.
			if ( ::SHGetFileInfo( pszPath, 0, &fi, sizeof( fi ), SHGFI_ICON | SHGFI_SMALLICON | SHGFI_OPENICON ) == 0 )
				return NULL;

			// Store the index.
			tvi.item.iSelectedImage = fi.iIcon;
			::DestroyIcon( fi.hIcon );

			// Is the path valid?
			CString strPath = pszPath;
			if ( Parse )
			{
				int nLen = strPath.GetLength();
				if ( nLen )
				{
					// If the path ends with a back-slash we remove
					// it.
					if ( strPath[ nLen - 1 ] == _T( '\\' ))
						strPath.Truncate( nLen - 1 );

					// Now we look for the right-most backslash. If it exists
					// we copy the string right from that back slash leaving us
					// with the last part of the path.
					//
					// If it does not exist we are left with the drive name. In this
					// case we append a backslash to it.
					int nPos = strPath.ReverseFind( _T( '\\' ));
					if ( nPos != -1 )
						strPath = strPath.Mid( nPos + 1 );
					else
						strPath += _T( '\\' );
				}
			}

			// Did we get a drive name?
			if ( pszDriveName )
			{
				// Bits 24 through 31 represent the drive letter.
				tvi.item.lParam = strPath[ 0 ];

				// Simply let the drive name be the name
				// shownd to the user.
				strPath = pszDriveName;
			}
			else
				tvi.item.lParam = 0;

			// Setup the rest of the structure.
			tvi.hParent	 = hParent;
			tvi.hInsertAfter = NULL;
			tvi.item.mask	 = TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_TEXT | TVIF_PARAM;
			tvi.item.pszText = ( LPTSTR )(LPCTSTR)strPath;

			// Insert the item.
			return InsertItem( &tvi );
		}

		CString GetPath( HTREEITEM hItem ) const
		{
			// 4KChars max.
			TCHAR	szBuffer[ 4096 ];

			// Setup the structure to obtain the item
			// text.
			TVITEM	tvi;
			tvi.mask	= TVIF_TEXT | TVIF_PARAM;
			tvi.pszText	= szBuffer;

			// Iterate the items until we reached the
			// root.
			CString strPath = _T("");
			while ( hItem )
			{
				// Set item.
				tvi.hItem	= hItem;
				tvi.cchTextMax  = 4096;

				// Get item name.
				if ( GetItem( &tvi ))
				{
					// Is it a drive name?
					if ( tvi.lParam )
					{
						// Build "real" path from the drive letter.
						_tcscpy( szBuffer, _T( " :\\" ));

						// Add the drive letter.
						szBuffer[ 0 ] = ( TCHAR )( tvi.lParam );
					}
					else
						// Append the backslash if necessary.
						if ( szBuffer[ _tcslen( szBuffer ) - 1 ] != _T( '\\' ))
							_tcsncat( szBuffer, _T( "\\" ), 4096 );

					// Insert the name of the item at the start
					// of the path string.
					strPath.Insert( 0, szBuffer );

					// Get the parent item
					hItem = GetParentItem( hItem );
				}
				else
					break;
			}
			return strPath;
		}

#ifdef _UNICODE
		typedef std::wstring mystlstring;
#else
		typedef std::string mystlstring;
#endif
		
		BOOL AddDirectory( HTREEITEM hParent )
		{
			// Put up the wait cursor.
			CWaitCursor wait;

			// Get the full path of the directory.
			CString strPath = GetPath( hParent );
			
			// Add the "*.*" filter.
			if ( strPath[ strPath.GetLength() - 1 ] != _T( '\\' )) 
				strPath += _T( '\\' );
			
			mystlstring basePath(strPath);

			strPath += _T( "*.*" );

			// Scan the directory.
			std::list<mystlstring> aDirs;
			std::list<mystlstring> aFiles;

			WIN32_FIND_DATA fd;
			HANDLE hFind = ::FindFirstFile(strPath, &fd);
			if(hFind != INVALID_HANDLE_VALUE)
			{
				do
				{
					if( (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && 
						!( fd.dwFileAttributes & m_dwDontShow ) && !isDots(fd.cFileName))
					{
						aDirs.insert(aDirs.end(), basePath + fd.cFileName);
					}
					else if( m_bShowFiles && !(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
						!( fd.dwFileAttributes & m_dwDontShow) && !isDots(fd.cFileName))
					{
						mystlstring s(fd.cFileName);
						aFiles.insert(aFiles.end(), basePath + fd.cFileName);
					}

				} while(::FindNextFile(hFind, &fd));

				::FindClose(hFind);
			}

			// Device not ready?
			if ( ::GetLastError() == ERROR_NOT_READY )
				return FALSE;

			// Sort both arrays.
			aDirs.sort();
			aFiles.sort();

			// Preset result.
			BOOL bRC = TRUE;

			// No rendering.
			if ( ! m_bSilent ) 
				SetRedraw( FALSE );

			// Add the directories.
			std::list<mystlstring>::const_iterator i;
			for ( i = aDirs.begin(); i != aDirs.end(); ++i )
			{
				// Add the directory entry.
				HTREEITEM hItem = AddTreeItem( hParent, (*i).c_str() );
				if ( hItem == NULL )
				{
					bRC = FALSE;
					break;
				}

				// Is the directory empty?
				if ( HasEntries( hItem ))
				{
					// Add a single, empty, item so we get a LVN_ITEMEXPANDED
					// notification.
					if ( AddTreeItem( hItem, _T( "" )) == NULL )
					{
						bRC = FALSE;
						break;
					}
				}
			}

			// Show files?
			if ( m_bShowFiles )
			{
				// Add the files.
				for ( i = aFiles.begin(); i != aFiles.end(); ++i )
				{
					// Add the entry.
					if ( AddTreeItem( hParent, (*i).c_str() ) == NULL )
					{
						bRC = FALSE;
						break;
					}
				}
			}

			// Re-render.
			if ( ! m_bSilent ) 
			{
				SetRedraw();
				UpdateWindow();
			}

			return bRC;
		}
		
		bool isDots(LPCTSTR fn)
		{
			return (_tcscmp(fn, _T("..")) == 0 || _tcscmp(fn, _T(".")) == 0);
		}

		BOOL HasEntries( HTREEITEM hItem )
		{
			CString strPath = GetPath( hItem );
	
			// Add the *.* filter for a directory search.
			if ( strPath[ strPath.GetLength() - 1 ] != _T( '\\' )) 
				strPath += _T( '\\' );

			strPath += _T("*.*");

			DWORD dwOldMode = SetErrorMode( SEM_FAILCRITICALERRORS );

			BOOL bRet = FALSE;

			WIN32_FIND_DATA fd;
			HANDLE hFind = ::FindFirstFile(strPath, &fd);
			if(hFind != INVALID_HANDLE_VALUE)
			{
				do
				{
					if((fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0 && 
						!isDots(fd.cFileName))
					{
						bRet = TRUE;
						break;
					}
					else if(m_bShowFiles)
					{
						if(!((m_dwDontShow & fd.dwFileAttributes) != 0) && !isDots(fd.cFileName))
						{
							bRet = TRUE;
							break;
						}
					}
				} while( ::FindNextFile(hFind, &fd) );

				::FindClose(hFind);
			}

			::SetErrorMode(dwOldMode);

			return bRet;
		}

		/*void ConvertDriveName( CString& strDrive )
		{

		}*/

	protected:

		LRESULT OnItemExpanding(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			// Obtain the item data.
			LPNMTREEVIEW pTreeView = (LPNMTREEVIEW)pnmh;

			TVITEM tvi;
			tvi.mask  = TVIF_PARAM;
			tvi.hItem = pTreeView->itemNew.hItem;
			if ( GetItem( &tvi ))
			{
				// Get the entry it's drive type.
				CString strPath = GetPath( pTreeView->itemNew.hItem );

				// Are we expanding?
				if ( ! ( pTreeView->itemNew.state & TVIS_EXPANDED ))
				{
					// Is it a drive?
					if ( tvi.lParam )
					{
						// Any entries?
						if ( HasEntries( pTreeView->itemNew.hItem ) == FALSE )
						{
							// Call the overidable.
							OnDriveEmpty( strPath );
							return TRUE;
						}
					}
				}
			}
			return FALSE;
		}

		LRESULT OnItemExpanded(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			LPNMTREEVIEW pTreeView = (LPNMTREEVIEW)pnmh;

			TVITEM tvi;
			tvi.mask  = TVIF_PARAM;
			tvi.hItem = pTreeView->itemNew.hItem;
			if ( GetItem( &tvi ))
			{
				// Did it exand?
				if ( pTreeView->itemNew.state & TVIS_EXPANDED )
				{
					// Force a redraw of the
					if ( ! m_bSilent ) 
					{
						SetRedraw();
						UpdateWindow();
					}

					// Compute the rectangle of the text label
					// including the icon.
					CRect rc, rctot;
					HTREEITEM hChild = GetChildItem( pTreeView->itemNew.hItem );
					GetItemRect( hChild, &rctot, FALSE );
					GetItemRect( hChild, &rc, TRUE );
					rc.right = rctot.right;

					rc.left -= ::GetSystemMetrics( SM_CXSMICON ) + 3;

					// Prepare a device context for the rendering.
					{
						// Get a DC.
						CDC dc(GetDC());
						
						// Device context valid?
						if ( dc.m_hDC != NULL ) //dc.IsValid())
						{
							// Setup the treeview font.
							CFontHandle	font = GetFont();
							HFONT oldFont = dc.SelectFont(font);

							// Erase the item background.
							CBrush brush;
							COLORREF color = GetBkColor();
							if ( color == ( COLORREF )-1 ) color = ::GetSysColor( COLOR_WINDOW );
							brush.CreateSolidBrush( color );
							dc.FillRect( rc, brush );

							// Call the overridable.
							OnRenderLoadingItem( &dc, rc );

							dc.SelectFont(oldFont);
						}
					}

					// Delete sub-items.
					hChild = GetChildItem( pTreeView->itemNew.hItem );
					while ( hChild )
					{
						DeleteItem( hChild );
						hChild = GetChildItem( pTreeView->itemNew.hItem );
					}
					
					// Add directory.
					AddDirectory( pTreeView->itemNew.hItem );
				}
				else
				{
					// Delete sub-items.
					HTREEITEM hChild = GetChildItem( pTreeView->itemNew.hItem );
					while ( hChild )
					{
						DeleteItem( hChild );
						hChild = GetChildItem( pTreeView->itemNew.hItem );
					}

					// Add a single empty item so the parent displays
					// the expansion box.
					AddTreeItem( pTreeView->itemNew.hItem, _T( "" ));
				}
			}
			return 0;
		}

		LRESULT OnPrintClient(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
		{
			// Render what?
			if ( lParam & PRF_ERASEBKGND ) SendMessage( WM_ERASEBKGND, wParam );
			if ( lParam & PRF_CLIENT     ) SendMessage( WM_PAINT, wParam );
			return 0;
		}
		
		// We handle the background painting in the
		// WM_PAINT message handler if the double buffering
		// flag is truned on.
		//
		// If it is not we return -1 which tells the message
		// handler to pass the message to the default message
		// handler.
		LRESULT OnEraseBkgnd(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			return m_bDoubleBuffer ? 0 : -1;
			return 0;
		}

		// WM_PAINT message handler. Will paint the tree contents
		// into a buffer DC if appropiate.
		LRESULT OnPaint(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
		{
			// Use double buffered rendering?
			if ( m_bDoubleBuffer )
			{
				// Get the client rectangle.
				CRect rc;
				GetClientRect( &rc );

				CDC *pdc;
				if ( wParam != NULL )
				{
					// Use the supplied DC.
					pdc = new CDC((HDC)wParam);
					//pdc->Attach();
				}
				else	  
				{ 
					// Use the DC create by the ClsPaintDC object.
					// The DC is the one returned by the BeginPaint()
					// API.
					pdc = new CPaintDC( m_hWnd ); 
				}


				// Preset the return code.
				LRESULT lResult = -1;

				// Brace the code so that the MemDC runs
				// out of scope before the CMemDC object
				// is deleted.
				{
					// Create DC buffer.
					//ClsBufferDC dc( *pdc, rc );
					CMemDC dc(pdc->m_hDC, &rc);

					// Render the background.
					SendMessage( WM_ERASEBKGND, ( WPARAM )pdc->m_hDC );

					// Call the default procedure to render the tree contents.
					lResult = DefWindowProc( WM_PAINT, ( WPARAM )( HDC )pdc->m_hDC, 0 );
				}

				// Delete the DC
				delete pdc;

				// Return the result.
				return lResult;
			}
			else
				bHandled = FALSE;

			return 0;
		}

	protected:
		// Data.
		CString		m_strFilter;
		CString		m_strLoading;
		CString		m_strInitial;
		COLORREF	m_crLoadingColor;
		DWORD		m_dwDontShow;
		BOOL		m_bShowFiles;
		BOOL		m_bDoubleBuffer;
		BOOL		m_bSilent;

	private:
		/**
		 * Get a valid drive letter to use for image retrieval hack
		 */
		TCHAR GetFirstDriveLetter()
		{
			DWORD logDrives = ::GetLogicalDrives();
			
			// Skip 'A' and 'B'
			logDrives >>=2;

			TCHAR drive = 'C';
			while(drive < _T('Z'))
			{
				if (logDrives & 1)
				{
					tstring path;
					path += drive;
					path += _T(":\\");
					if (::GetDriveType(path.c_str()) == DRIVE_FIXED)
					{
						break;
					}
				}
				
				drive++;
				logDrives >>=1;
			}
			
			return drive;
		}
};

#endif // #ifndef browsetree_h__included