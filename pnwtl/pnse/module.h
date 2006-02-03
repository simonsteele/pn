class CpnseModule : public CAtlDllModuleT< CpnseModule >
{
public :
	DECLARE_LIBID(LIBID_pnseLib)
	DECLARE_REGISTRY_APPID_RESOURCEID(IDR_PNSE, "{CAA32054-46E7-467D-AF8C-98D3A8843254}")
};

extern CpnseModule _AtlModule;

extern HINSTANCE _hInstance;