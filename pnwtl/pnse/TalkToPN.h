#ifndef talktopn_h__included
#define talktopn_h__included

class PNCommunicator
{
public:
	PNCommunicator(LPCTSTR szKey);
	~PNCommunicator();

	void ActivateOther();
	void SendParameters(std::list< std::basic_string<TCHAR> > files);

private:
	bool CreateSharedData(BYTE** buffer, HANDLE* hMappedFile, size_t size);
	void ReleaseSharedData(BYTE* buffer, HANDLE hMappedFile);

private:
	enum EMIEvents
	{
		MIM_ACTIVATE = 0,
		MIM_PARAMETER_ARRAY = 1,
	};

	typedef long (__stdcall *PFNBroadcastSystemMessage)(DWORD dwFlags,
		LPDWORD lpdwRecipients,
		UINT uiMessage,
		WPARAM wParam,
		LPARAM lParam);

	UINT						m_uiMessage;
	std::basic_string<TCHAR>	m_sKey;
	PFNBroadcastSystemMessage	m_pfnBSM;
	HMODULE						m_hUser32;
};

#endif // #ifndef talktopn_h__included