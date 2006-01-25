#include "stdafx.h"
#include "extiface.h"
#include "extapp.h"

#include "scriptregistry.h"

namespace extensions {

IScriptRegistry* App::GetScriptRegistry()
{
	return ScriptRegistry::GetInstance();
}

}