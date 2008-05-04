/**
 * @file updatecheck.h
 * @brief Check for updates
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef updatecheck_h__included
#define updatecheck_h__included

namespace Updates
{

class Version
{
public:
	Version() :
		Major(0),
		Minor(0),
		Revision(0),
		Build(0)
	{
	}

	Version(int major, int minor, int revision, int build) :
		Major(major),
		Minor(minor),
		Revision(revision),
		Build(build)
	{
	}

	Version& operator = (const Version& other)
	{
		Major = other.Major;
		Minor = other.Minor;
		Revision = other.Revision;
		Build = other.Build;

		return *this;
	}

	bool operator > (const Version& other) const
	{
		return 
		   (Major > other.Major) 
		|| (Major == other.Major && Minor > other.Minor)
		|| (Major == other.Major && Minor == other.Minor && Revision > other.Revision)
		|| (Major == other.Major && Minor == other.Minor && Revision == other.Revision && Build > other.Build);
	}

	int Major;
	int Minor;
	int Revision;
	int Build;
};

class PNVersion : public Version
{
public:
	PNVersion() : Version(PN_MAJOR, PN_MINOR, PN_REVISION, PN_BUILD)
	{}
};

class UpdateAvailableDetails : public Version
{
public:
	tstring UpdateUrl;

	UpdateAvailableDetails& operator = (const UpdateAvailableDetails& other)
	{
		Major = other.Major;
		Minor = other.Minor;
		Revision = other.Revision;
		Build = other.Build;
		UpdateUrl = other.UpdateUrl;

		return *this;
	}
};

void CheckForUpdates(HWND notifyWnd);
Version GetLastOfferedVersion();
void SetLastOfferedVersion(const Version& version);

}

#endif