#include "stdafx.h"
#include "extension.h"

namespace extensions {

Extension::Extension(LPCTSTR filename, IPN* app) : Plugin(filename), m_bValid(false)
{
	if(Plugin::Valid())
	{
		extensions::pn_ext_init_fn ext_init = (extensions::pn_ext_init_fn)FindFunction("init_pn_extension");
		if(ext_init != NULL)
		{
			if( app != NULL )
			{
				if( ext_init(PN_EXT_IFACE_VERSION, app) )
					m_bValid = true;
			}
			else
			{
				// Assume valid without changing version...
				// TODO: Perhaps change the interface to allow checking compat without
				// passing a valid App for initialisation
				m_bValid = true;
			}
		}
	}
}

void Extension::Unload()
{
	if(Valid())
	{
		extensions::pn_ext_exit_fn ext_exit = (extensions::pn_ext_exit_fn)FindFunction("exit_pn_extension");
		if(ext_exit != NULL)
			ext_exit();
	}

	Plugin::Unload();
}

bool Extension::Valid()
{
	return Plugin::Valid() && m_bValid;
}

}