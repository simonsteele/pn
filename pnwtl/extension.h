#ifndef extension_h__included
#define extension_h__included

#include "extapp.h"
#include "include/plugin.h"

namespace extensions
{

class Extension : public Plugin
{
public:
	Extension(LPCTSTR filename, IPN* app);

	virtual void Unload();
	virtual bool Valid();

	bool GetDetails(PN::BaseString& name, PN::BaseString& version);

private:
	bool m_bValid;
};

}

#endif // #ifndef extension_h__included