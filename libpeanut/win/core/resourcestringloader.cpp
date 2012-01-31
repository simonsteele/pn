#include "stdafx.h"
#include "l10n.h"

tstring ResourceStringLoader::load(UINT dwStringID)
{
        CString s;
        s.LoadString(m_hInstance, dwStringID);
        return (LPCTSTR)s;
}

