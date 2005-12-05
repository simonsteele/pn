#include "include/plugin.h"

namespace extensions
{

class Extension : public Plugin
{
public:
	Extension(LPCTSTR filename) : Plugin(filename), m_bValid(false)
	{
		if(Plugin::Valid())
		{
			extensions::pn_ext_init_fn ext_init = (extensions::pn_ext_init_fn)FindFunction("init_pn_extension");
			if(ext_init != NULL)
			{
				//if( ext_init(PN_EXT_IFACE_VERSION, &g_Context.ExtApp) )
				//	m_bValid = true;
			}
		}
	}

	~Extension()
	{
		if(Valid())
		{
			extensions::pn_ext_exit_fn ext_exit = (extensions::pn_ext_exit_fn)FindFunction("exit_pn_extension");
			if(ext_exit != NULL)
				ext_exit();
		}
	}

	virtual bool Valid()
	{
		return Plugin::Valid() && m_bValid;
	}

private:
	bool m_bValid;
};

}