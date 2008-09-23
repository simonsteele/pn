/**
 * @file updatecheck.cpp
 * @brief Check for updates
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
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

typedef struct tagUpdateCheckDetails
{
	tstring UpdateUrl;
	tstring UnstableUpdateUrl;
	bool CheckUnstable;
	HWND NotifyWnd;
} UpdateCheckDetails;

/*
 <release major="2" minor="0" revision="9" build="12345" launchUrl=""/>
 */

class UpdateParseState : public XMLParseState
{
public:
	UpdateParseState() : Valid(false)
	{
	}

	virtual void startElement(LPCTSTR name, XMLAttributes& atts)
	{
		if (_tcscmp(name, _T("release")) == 0)
		{
			LPCTSTR temp;

			temp = atts.getValue("major");
			if (!temp)
				return;

			Details.Major = _ttoi(temp);

			temp = atts.getValue("minor");
			if (!temp)
				return;

			Details.Minor = _ttoi(temp);

			temp = atts.getValue("revision");
			if (!temp)
				return;

			Details.Revision = _ttoi(temp);

			temp = atts.getValue("build");
			if (!temp)
				return;

			Details.Build = _ttoi(temp);

			temp = atts.getValue("launchUrl");
			if (!temp)
				return;

			Details.UpdateUrl = temp;
			Valid = true;
		}
	}

	virtual void endElement(LPCTSTR name)
	{
	}

	virtual void characterData(LPCTSTR data, int len)
	{
	}

public:
	UpdateAvailableDetails Details;
	bool Valid;
};

UpdateAvailableDetails GetVersionDetails(LPCTSTR url)
{
	UpdateParseState parseState;
	Inet inet;
	if (inet.Open(url))
	{
		TCHAR buffer[UPDATE_BUFFER_SIZE];
		DWORD dwRead;
		XMLParser parser;
		parser.SetParseState(&parseState);
		
		while(inet.ReadFile(&buffer[0], UPDATE_BUFFER_SIZE, &dwRead))
		{
			parser.ParseBuffer(buffer, dwRead, dwRead == 0);
			if (dwRead == 0)
				break;
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

DWORD WINAPI CheckForUpdatesThreadProc(
  __in  LPVOID lpDetails
)
{
	std::auto_ptr<UpdateCheckDetails> details(reinterpret_cast<UpdateCheckDetails*>( lpDetails ));

	Version lastOffered = GetLastOfferedVersion();
	UpdateAvailableDetails stable = GetVersionDetails(details->UpdateUrl.c_str());
	UpdateAvailableDetails unstable;

	if (details->CheckUnstable)
	{
		unstable = GetVersionDetails(details->UnstableUpdateUrl.c_str());
	}

	if (unstable > PNVersion() && unstable > lastOffered)
	{
		::SendMessage(details->NotifyWnd, PN_NOTIFY, reinterpret_cast<WPARAM>(&unstable), PN_UPDATEAVAILABLE);
	}
	else if (stable > PNVersion() && stable > lastOffered)
	{
		::SendMessage(details->NotifyWnd, PN_NOTIFY, reinterpret_cast<WPARAM>(&stable), PN_UPDATEAVAILABLE);
	}
	
	return 0;
}

namespace Updates {

void CheckForUpdates(HWND notifyWnd)
{
	if (!OPTIONS->Get(PNSK_GENERAL, _T("CheckForUpdates"), true))
	{
		return;
	}

	// Don't check more than once a day:
	uint64_t lastCheck = OPTIONS->Get(PNSK_GENERAL, _T("LastUpdateCheck"), static_cast<uint64_t>(0));
	SYSTEMTIME time;
	::GetSystemTime(&time);
	uint64_t thisCheck = (static_cast<uint64_t>(time.wYear) << 32) | (time.wMonth << 16) | time.wDay;
	if (thisCheck == lastCheck)
	{
		return;
	}
	
	OPTIONS->Set(PNSK_GENERAL, _T("LastUpdateCheck"), thisCheck);

	// Start the update check process:
	UpdateCheckDetails* details = new UpdateCheckDetails;
	details->NotifyWnd = notifyWnd;		
	details->CheckUnstable = OPTIONS->Get(PNSK_GENERAL, _T("CheckForUnstableUpdates"), false);
	details->UnstableUpdateUrl = OPTIONS->Get(PNSK_GENERAL, _T("UnstableUpdateUrl"), _T("http://updates.pnotepad.org/unstable.xml"));
	details->UpdateUrl = OPTIONS->Get(PNSK_GENERAL, _T("UpdateUrl"), _T("http://updates.pnotepad.org/stable.xml"));

	::QueueUserWorkItem(CheckForUpdatesThreadProc, details, WT_EXECUTEDEFAULT);
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

}