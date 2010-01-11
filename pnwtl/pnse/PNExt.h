// PNExt.h : Declaration of the CPNExt

#pragma once

#include "resource.h"       // main symbols
#include "pnse.h"

// CPNExt

class ATL_NO_VTABLE CPNExt : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CPNExt, &CLSID_PNExt>,
	public IDispatchImpl<IPNExt, &IID_IPNExt, &LIBID_pnseLib, /*wMajor =*/ 1, /*wMinor =*/ 0>,
	public IShellExtInit,
	public IContextMenu
{
public:
	CPNExt();

	DECLARE_REGISTRY_RESOURCEID(IDR_PNEXT)

	BEGIN_COM_MAP(CPNExt)
		COM_INTERFACE_ENTRY(IPNExt)
		COM_INTERFACE_ENTRY(IDispatch)
		
		// IShellExtInit is called by explorer to initialise the shell session
		COM_INTERFACE_ENTRY(IShellExtInit)

		// We are providing a context menu extension, we implement IContextMenu
		COM_INTERFACE_ENTRY_IID(IID_IContextMenu, IContextMenu)
	END_COM_MAP()

	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}
	
	void FinalRelease() 
	{
	}

// IShellExtInit
public:
    STDMETHOD(Initialize)(LPCITEMIDLIST, LPDATAOBJECT, HKEY);

// IContextMenu
public:
    STDMETHOD(GetCommandString)(UINT, UINT, UINT*, LPSTR, UINT);
    STDMETHOD(InvokeCommand)(LPCMINVOKECOMMANDINFO);
    STDMETHOD(QueryContextMenu)(HMENU, UINT, UINT, UINT, UINT);

private:
	void InformPN();
	void InformNewPN();
    
	std::list< tstring > m_files;
};

OBJECT_ENTRY_AUTO(__uuidof(PNExt), CPNExt)
