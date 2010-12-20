/**
 * @file singleinstance.h
 * @brief Single Instance Controller
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef MULTINSTANCE_H__INCLUDED
#define MULTINSTANCE_H__INCLUDED

class MultipleInstanceManager
{
public:
	MultipleInstanceManager(LPCTSTR pszKey);
	~MultipleInstanceManager();

	void ActivateOther();
	bool AlreadyActive();
	void SendParameters();
	void SendParameters(const std::list<tstring>& args);

	void AllowParameters();

	bool GetParameters(std::list<tstring>& params, DWORD size);

	UINT GetMessageID();

	enum EMIEvents
	{
		MIM_ACTIVATE = 0,
		MIM_PARAMETER_ARRAY = 1,
	};

private:
	typedef long (__stdcall *PFNBroadcastSystemMessage)(DWORD dwFlags,
		LPDWORD lpdwRecipients,
		UINT uiMessage,
		WPARAM wParam,
		LPARAM lParam);

	bool CreateSharedData(BYTE** buffer, HANDLE* hMappedFile, size_t size);
	void ReleaseSharedData(BYTE* buffer, HANDLE hMappedFile);
	bool RequestPermission();
	void Release();

private:
	HANDLE	m_hMutex;
	bool	m_bAlreadyActive;
	tstring	m_sKey;
	UINT	m_uiMessage;
	
	PFNBroadcastSystemMessage	m_pfnBSM;
	HMODULE						m_hUser32;
};

#endif //#ifndef MULTINSTANCE_H__INCLUDED