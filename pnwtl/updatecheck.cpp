/**
 * @file updatecheck.cpp
 * @brief Check for updates
 * @author Simon Steele
 * @note Copyright (c) 2008-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "version.h"
#include "updatecheck.h"
#include "inet.h"

using namespace Updates;

#define UPDATE_BUFFER_SIZE 2048

/**
 * Details of an update check operation
 */
typedef struct tagUpdateCheckDetails
{
	tstring UpdateUrl;
	tstring UnstableUpdateUrl;
	bool CheckUnstable;
	bool IgnoreLastOffer;
	HWND NotifyWnd;
} UpdateCheckDetails;

/**
 * XML Parser event handler for the update xml, format:
 * <release major="2" minor="0" revision="9" build="12345" launchUrl=""/>
 */
class UpdateParseState : public XMLParseState
{
public:
	UpdateParseState() : Valid(false) {}

	virtual void startElement(LPCTSTR name, const XMLAttributes& atts)
	{
		if (_tcscmp(name, _T("release")) == 0)
		{
			LPCTSTR temp;

			temp = atts.getValue(_T("major"));
			if (!temp)
				return;

			Details.Major = _ttoi(temp);

			temp = atts.getValue(_T("minor"));
			if (!temp)
				return;

			Details.Minor = _ttoi(temp);

			temp = atts.getValue(_T("revision"));
			if (!temp)
				return;

			Details.Revision = _ttoi(temp);

			temp = atts.getValue(_T("build"));
			if (!temp)
				return;

			Details.Build = _ttoi(temp);

			temp = atts.getValue(_T("launchUrl"));
			if (!temp)
				return;

			Details.UpdateUrl = temp;
			Valid = true;
		}
	}

	virtual void endElement(LPCTSTR name) {}
	virtual void characterData(LPCTSTR data, int len) {}

	UpdateAvailableDetails Details;
	bool Valid;
};

/**
 * Get the most recent version of PN available as signalled in the file at the provided url
 * @param url URL for an update xml file
 */
UpdateAvailableDetails GetVersionDetails(LPCTSTR url)
{
	UpdateParseState parseState;
	Inet inet;
	if (inet.Open(url))
	{
		BYTE buffer[UPDATE_BUFFER_SIZE];
		DWORD dwRead;
		XMLParser parser;
		parser.SetParseState(&parseState);
		
		while (inet.ReadFile(&buffer[0], UPDATE_BUFFER_SIZE, &dwRead))
		{
			parser.ParseBuffer((const char*)buffer, dwRead, dwRead == 0);
			if (dwRead == 0)
			{
				break;
			}
		}

		if (!parseState.Valid)
		{
			parseState.Details.Major = 0;
			parseState.Details.Minor = 0;
			parseState.Details.Revision = 0;
			parseState.Details.Build = 0;
		}
	}

	return parseState.Details;
}

bool CheckAndNotify(UpdateCheckDetails* details)
{
	if (details == NULL)
	{
		return false;
	}

	Version lastOffered;
	
	if (!details->IgnoreLastOffer)
	{
		lastOffered = GetLastOfferedVersion();
	}

	UpdateAvailableDetails stable = GetVersionDetails(details->UpdateUrl.c_str());
	UpdateAvailableDetails unstable;

	if (details->CheckUnstable)
	{
		unstable = GetVersionDetails(details->UnstableUpdateUrl.c_str());
	}

	if ((unstable > stable) && (unstable > PNVersion()) && (unstable > lastOffered))
	{
		::SendMessage(details->NotifyWnd, PN_NOTIFY, reinterpret_cast<WPARAM>(&unstable), PN_UPDATEAVAILABLE);
		return true;
	}
	else if (stable > PNVersion() && stable > lastOffered)
	{
		::SendMessage(details->NotifyWnd, PN_NOTIFY, reinterpret_cast<WPARAM>(&stable), PN_UPDATEAVAILABLE);
		return true;
	}
	
	return false;
}

/**
 * Thread proc used via QueueUserWorkItem to check for updates outside the UI loop
 */
DWORD WINAPI CheckForUpdatesThreadProc(__in LPVOID lpDetails)
{
	std::auto_ptr<UpdateCheckDetails> details(reinterpret_cast<UpdateCheckDetails*>( lpDetails ));

	CheckAndNotify(details.get());
	
	return 0;
}

/**
 * Simple factory function for the update details object.
 */
UpdateCheckDetails* MakeUpdateDetails(HWND notifyWnd)
{
	// Start the update check process:
	UpdateCheckDetails* details = new UpdateCheckDetails;
	details->NotifyWnd = notifyWnd;		
	details->CheckUnstable = OPTIONS->Get(PNSK_GENERAL, _T("CheckForUnstableUpdates"), false);
	details->UnstableUpdateUrl = OPTIONS->Get(PNSK_GENERAL, _T("UnstableUpdateUrl"), _T("http://updates.pnotepad.org/unstable.xml"));
	details->UpdateUrl = OPTIONS->Get(PNSK_GENERAL, _T("UpdateUrl"), _T("http://updates.pnotepad.org/stable.xml"));
	details->IgnoreLastOffer = false;

	return details;
}

/**
 * Get the date as a 64-bit unsigned int for storage and comparison
 */
uint64_t GetDateAsUInt64()
{
	SYSTEMTIME time;
	::GetSystemTime(&time);
	return (static_cast<uint64_t>(time.wYear) << 32) | (time.wMonth << 16) | time.wDay;
}

namespace Updates {

/**
 * Spawn a user work item to check updates if necessary
 */
void CheckForUpdates(HWND notifyWnd)
{
	if (!OPTIONS->Get(PNSK_GENERAL, _T("CheckForUpdates"), true))
	{
		return;
	}

	// Don't check more than once a day:
	uint64_t lastCheck = OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateCheck"), static_cast<uint64_t>(0));
	uint64_t thisCheck = GetDateAsUInt64();
	if (thisCheck == lastCheck)
	{
		return;
	}
	
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateCheck"), thisCheck);

	// Start the update check process:
	::QueueUserWorkItem(CheckForUpdatesThreadProc, MakeUpdateDetails(notifyWnd), WT_EXECUTEDEFAULT);
}

/**
 * Perform a synchronous check for updates on user request
 */
bool CheckForUpdatesSync(HWND notifyWnd)
{
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateCheck"), GetDateAsUInt64());
	std::auto_ptr<UpdateCheckDetails> details(MakeUpdateDetails(notifyWnd));
	
	// We don't want to hide anything we've offered before, always offer the most recent update:
	details->IgnoreLastOffer = true;

	return CheckAndNotify(details.get());
}

Version GetLastOfferedVersion()
{
	Version lastOffered(
		OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateOfferedMajor"), 0),
		OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateOfferedMinor"), 0),
		OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateOfferedRevision"), 0),
		OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateOfferedBuild"), 0));

	return lastOffered;
}

void SetLastOfferedVersion(const Version& details)
{
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateOfferedMajor"), details.Major);
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateOfferedMinor"), details.Minor);
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateOfferedRevision"), details.Revision);
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateOfferedBuild"), details.Build);
}

} // namespace Updates