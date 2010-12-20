#include "stdafx.h"
#include "extension.h"

namespace extensions {

Extension::Extension(LPCTSTR filename, IPN* app) : Plugin(filename), m_bValid(false)
{
	if(Plugin::Valid())
	{
		extensions::pn_ext_init_fn ext_init = (extensions::pn_ext_init_fn)FindFunction("pn_init_extension");
		if(ext_init != NULL)
		{
			if( app != NULL )
			{
#ifndef _DEBUG
				try
				{
#endif
				if( ext_init(PN_EXT_IFACE_VERSION, app) )
					m_bValid = true;
#ifndef _DEBUG
				}
				catch (std::exception& cppexception)
				{
					CFileName fn(filename);
					tstring msg(_T("Extension "));
					msg += fn.GetFileName();
					msg += _T(" has experienced an error on load, and will not be available:\n");
					
					CA2CT what(cppexception.what());
					msg += what;
					UNEXPECTED(msg.c_str());
				}
				catch (...)
				{
					UNEXPECTED(_T("Loading an extension for Programmer's Notepad has caused a problem, and the application will now close. Please try running in safe mode and disabling extensions to resolve this problem."));
					throw;
				}
#endif
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
		extensions::pn_ext_exit_fn ext_exit = (extensions::pn_ext_exit_fn)FindFunction("pn_exit_extension");
		if(ext_exit != NULL)
			ext_exit();
	}

	Plugin::Unload();
}

bool Extension::Valid()
{
	return Plugin::Valid() && m_bValid;
}

bool Extension::GetDetails(PN::BaseString& name, PN::BaseString& version)
{
	if (Valid())
	{
		extensions::pn_ext_info_fn ext_info = (extensions::pn_ext_info_fn)FindFunction("pn_get_extension_info");
		if(ext_info != NULL)
		{
			ext_info(name, version);
			return true;
		}
	}

	return false;
}

}