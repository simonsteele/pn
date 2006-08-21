/**
 * @file DropTargetImpl.h
 * @brief Simple COM mixin and callback interface for IDropTarget
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 */
#ifndef droptargetimpl_h__included
#define droptargetimpl_h__included

/**
 * @brief IDropTarget Mixin implementation - ATL style...
 */
template <class T>
class IDropTargetImpl : public IDropTarget
{
public:

	//////////////////////////////////////////////////
	// IDropTarget Interface Entries

	STDMETHOD(DragEnter)(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDragEnter(pDataObject, dwKeyState, pt, pdwEffect);
	}

	STDMETHOD(DragOver)(DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDragOver(dwKeyState, pt, pdwEffect);
	}

	STDMETHOD(DragLeave)(void)
	{
		return static_cast<T*>(this)->OnDragLeave();
	}

	STDMETHOD(Drop)(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDrop(pDataObject, dwKeyState, pt, pdwEffect);
	}

	//////////////////////////////////////////////////
	// Default implementations

	HRESULT OnDragEnter(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDragOver(DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDragLeave(void)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDrop(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

};

/**
 * Simple COM object implementing IDropTarget and using callbacks
 * to allow another object to handle the calls. Prevents needing
 * to turn everything into a COM object.
 */
template<class TCallbackTarget>
class DropTargetImpl : public CComObjectRoot,
	public IDropTargetImpl< DropTargetImpl<TCallbackTarget> >
{	

	/**
	 * Let ATL handle the COM mapping (IUnknown etc.) for us.
	 */
	BEGIN_COM_MAP(DropTargetImpl)
        COM_INTERFACE_ENTRY(IDropTarget)
    END_COM_MAP()

	public:
		/**
		 * Constructor
		 */
		DropTargetImpl()
		{
			pCallbacks = NULL;
		}

		/**
		 * Set the target for the callbacks.
		 */
		void SetCallbackTarget(TCallbackTarget* pCallbackTarget)
		{
			pCallbacks = pCallbackTarget;
		}

		HRESULT OnDragEnter(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragEnter(pDataObject, dwKeyState, pt, pdwEffect);
		}

		HRESULT OnDragOver(DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragOver(dwKeyState, pt, pdwEffect);
		}

		HRESULT OnDragLeave(void)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragLeave();
		}

		HRESULT OnDrop(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDrop(pDataObject, dwKeyState, pt, pdwEffect);
		}

	protected:
		TCallbackTarget* pCallbacks;
};

#endif // #ifndef droptargetimpl_h__included