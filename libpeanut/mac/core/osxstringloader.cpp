/**
 * @file osxstringloader.cpp
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include <stdafx.h>
#include "../../../pnwtl/l10n.h"
#include "osxstringloader.h"

namespace PN { namespace L10N {

tstring OsxStringLoader::load(UINT dwStringID)
{
    switch (dwStringID)
    {
#include "../../../pnwtl/translations/id_to_string.inc"
    }
    
    return "";
}
    
} } // namespace PN::L10N