// Copyright (c) 2002
// Sergey Klimov (kidd@ukr.net)

#ifndef __WTL_DW__SSTATE_H__
#define __WTL_DW__SSTATE_H__

#pragma once

#include<memory>
#include<utility>
#include<algorithm>
#include<map>
#include<cassert>
#include<limits>
#include<string>
#include<sstream>
#include<stg.h>

namespace sstate{

const	TCHAR ctxtGeneral[]		= _T("General");
const	TCHAR ctxtCXScreen[]	=_T("SM_CXSCREEN");
const	TCHAR ctxtCYScreen[]	=_T("SM_CYSCREEN");
const	TCHAR ctxtPlacement[]	=_T("placement");
const	TCHAR ctxtMainWindow[]	=_T("MainWindow");
const	TCHAR ctxtVisible[]		=_T("visible");
const	TCHAR ctxtBand[]		=_T("band");
const	TCHAR ctxtWndPrefix[]	=_T("Wnd-");
typedef std::basic_string<TCHAR> tstring;
typedef unsigned long ID;
//typedef tstring ID;


struct IState
{
	virtual ~IState(void){}
	virtual bool Store(IStorge& /*stg*/)=0;
	virtual bool Restore(IStorge& /*stg*/,float /*xratio*/,float /*yratio*/)=0;
	virtual bool RestoreDefault(void)=0;
	virtual void AddRef(void)=0;
	virtual void Release(void)=0;
};

template<class T>
class CStateBase : public T
{
public:
	CStateBase():m_ref(1)
	{
	}
	virtual void AddRef(void)
	{
		m_ref++;
	}
	virtual void Release(void)
	{
		if(--m_ref==0)
			delete this;
	}
	virtual ~CStateBase(void)
	{
		assert(m_ref==0);
	}
private:
	CStateBase(const CStateBase& );
	const CStateBase& operator=(const CStateBase& );
protected:
	unsigned long m_ref;
};

template<class T=IState>
class CStateHolder
{
	typedef CStateHolder<T> thisClass;
public:
	CStateHolder(void)
		:m_pState(0)
	{
	}
	CStateHolder(T* pState)
	{
		pState->AddRef();
		m_pState=pState;
	}
	CStateHolder(const thisClass& sholder)
	{
		*this=(sholder);
	}
	thisClass& operator = (const thisClass& sholder)
	{
		m_pState=const_cast<T*>(sholder.m_pState);
		if(m_pState!=0)
			m_pState->AddRef();
		return *this;
	}
	~CStateHolder(void)
	{
		if(m_pState!=0)
			m_pState->Release();
	}
	const T* operator ->() const
	{
		return m_pState;
	}
	T* operator ->()
	{
		return m_pState;
	}
protected:
	T* m_pState;
};

template<class TStorage>
class CContainerImpl
	: public CStateBase<IState>
{
protected:
	typedef CStateHolder<IState> CItem;
	typedef std::map<ID,CItem> CBunch;
    class CStorer
    {
    public:
		CStorer(IStorge& stgTop)
				:m_stgTop(stgTop)
        {
        }
        void operator() (std::pair<const ID,CItem>& x) const
        {
            std::basic_ostringstream<TCHAR> sstrKey;
            sstrKey.flags(std::ios::hex | std::ios::showbase );
			sstrKey<<ctxtWndPrefix<<x.first;
/*
            CRegKey key;
            DWORD dwDisposition;
            LONG lRes = key.Create(m_keyTop,sstrKey.str().c_str(),REG_NONE,
                                    REG_OPTION_NON_VOLATILE, KEY_WRITE|KEY_READ,
                                    NULL,&dwDisposition);
            if(lRes==ERROR_SUCCESS)
				x.second->Store(key);
*/
			TStorage stg;
			if(stg.Create(m_stgTop,sstrKey.str().c_str(),IStorge::ReadWrite)==ERROR_SUCCESS)
                x.second->Store(stg);
        }
    protected:
		IStorge& m_stgTop;
    };

	class CRestorer
	{
	public:
		CRestorer(IStorge& stgTop,float xratio,float yratio)
				:m_stgTop(stgTop)
				,m_xratio(xratio)
				,m_yratio(yratio)
		{
		}
		void operator() (std::pair<const ID,CItem>& x) const
		{
            std::basic_ostringstream<TCHAR> sstrKey;
            sstrKey.flags(std::ios::hex | std::ios::showbase );
			sstrKey<<ctxtWndPrefix<<x.first;
/*
            CRegKey key;
            LONG lRes = key.Open(m_keyTop,sstrKey.str().c_str(),KEY_READ);
            if(lRes==ERROR_SUCCESS)
                x.second->Restore(key,m_xratio,m_yratio);
			else
				x.second->RestoreDefault();
*/
			TStorage stg;
			if(stg.Open(m_stgTop,sstrKey.str().c_str(),IStorge::Read)!=ERROR_SUCCESS
				|| !x.second->Restore(stg,m_xratio,m_yratio))
					x.second->RestoreDefault();
		}
	protected:
		IStorge& m_stgTop;
		float	m_xratio;
		float	m_yratio;
	};
    struct CDefRestorer
    {
        void operator() (std::pair<const ID,CItem>& x) const
        {
			x.second->RestoreDefault();
        }
    };
public:
	CContainerImpl(void)
		:m_nextFreeID(/*std::numeric_limits<ID>::max()*/ULONG_MAX)
	{
	}
	ID GetUniqueID(void) const
	{
		return m_nextFreeID--;
	}
	virtual bool Store(IStorge& stg)
	{
        std::for_each(m_bunch.begin(),m_bunch.end(),CStorer(stg));
		return true;
	}
	virtual bool Restore(IStorge& stg,float xratio,float yratio)
	{
        std::for_each(m_bunch.begin(),m_bunch.end(),CRestorer(stg,xratio,yratio));
		return true;
	}
	virtual bool RestoreDefault(void)
	{
        std::for_each(m_bunch.begin(),m_bunch.end(),CDefRestorer());
		return true;
	}
	ID Add(IState* pState)
	{
		ID id=GetUniqueID();
		Add(id,pState);
		return id;
	}
	void Add(ID id,IState* pState)
	{
		CStateHolder<IState> h (pState);
		m_bunch[id]=h;
	}
	void Remove(ID id)
	{
		assert(m_bunch.find(id)!=m_bunch.end());
		m_bunch.erase(id);
	}
protected:
	mutable ID	m_nextFreeID;
	CBunch m_bunch;
};

template<class TStorage>
class CWindowStateMgr
{
protected:
	class CImpl 
		: public CContainerImpl<TStorage>
	{
		typedef CContainerImpl<TStorage> baseClass;
	public:
		CImpl(HWND hWnd=NULL,int nDefCmdShow=SW_SHOWDEFAULT)
			:m_hWnd(hWnd),m_nDefCmdShow(nDefCmdShow)
		{
		}
        void SetWindow(HWND hWnd=NULL,int nDefCmdShow=SW_SHOWDEFAULT)
        {
            assert(::IsWindow(hWnd));
            m_hWnd=hWnd;
            m_nDefCmdShow=nDefCmdShow;
        }
		virtual bool Store(IStorge& stg)
		{
			assert(IsWindow(m_hWnd));

            WINDOWPLACEMENT wp;
            wp.length = sizeof(WINDOWPLACEMENT);
            bool bRes=false;
            if (::GetWindowPlacement(m_hWnd,&wp))
            {
                wp.flags = 0;
                if(::IsZoomed(m_hWnd))
					wp.flags |= WPF_RESTORETOMAXIMIZED;
				if(wp.showCmd==SW_SHOWMINIMIZED)
					wp.showCmd=SW_SHOWNORMAL;				
				bRes=(stg.SetBinary(ctxtPlacement,&wp,sizeof(WINDOWPLACEMENT))==ERROR_SUCCESS);
/*
                bRes=(::RegSetValueEx(key,ctxtPlacement,NULL,REG_BINARY,
										reinterpret_cast<CONST BYTE *>(&wp),
										sizeof(WINDOWPLACEMENT))==ERROR_SUCCESS);
*/
            }
			return baseClass::Store(stg);
		}
		virtual bool Restore(IStorge& stg,float xratio,float yratio)
		{
			assert(IsWindow(m_hWnd));
            WINDOWPLACEMENT wp;
/*
            DWORD dwType;
            DWORD cbData=sizeof(WINDOWPLACEMENT);
            bool bRes=(::RegQueryValueEx(key,ctxtPlacement,NULL,&dwType,
											reinterpret_cast<LPBYTE>(&wp),&cbData)==ERROR_SUCCESS)
											&&(dwType==REG_BINARY);
*/
			size_t size=sizeof(WINDOWPLACEMENT);
			bool bRes=(stg.GetBinary(ctxtPlacement,&wp,size)==ERROR_SUCCESS
							&& (size==sizeof(WINDOWPLACEMENT)));
            if(bRes)
			{
				UINT nCmdShow=wp.showCmd;
//				LockWindowUpdate(m_hWnd);
				if(wp.showCmd==SW_MAXIMIZE)
					::ShowWindow(m_hWnd,nCmdShow);
				wp.showCmd=SW_HIDE;
                ::SetWindowPlacement(m_hWnd,&wp);
				bRes=baseClass::Restore(stg,xratio,yratio);
				::ShowWindow(m_hWnd,nCmdShow);
//				LockWindowUpdate(NULL);
			}
			else
				bRes=/*baseClass::*/RestoreDefault();
			return bRes;
		}
		virtual bool RestoreDefault(void)
		{
			assert(IsWindow(m_hWnd));
			bool bRes=baseClass::RestoreDefault();
			ShowWindow(m_hWnd,m_nDefCmdShow);
			return bRes;
		}
	protected:
		HWND	m_hWnd;
		int		m_nDefCmdShow;
	};
public:
	CWindowStateMgr(HWND hWnd=NULL,int nDefCmdShow=SW_SHOWDEFAULT)
	{
		m_pImpl=new CImpl(hWnd,nDefCmdShow);
	}
	~CWindowStateMgr(void)
	{
		assert(m_pImpl);
		m_pImpl->Release();
	}
	operator IState* (void)
	{
		return m_pImpl;
	}
	ID Add(IState* pState)
	{
		return m_pImpl->Add(pState);
	}
	void Add(ID id,IState* pState)
	{
		m_pImpl->Add(id,pState);
	}
	void Remove(ID id)
	{
		m_pImpl->Remove(id);
	}

    void Initialize(HWND hWnd,int nDefCmdShow=SW_SHOWDEFAULT)
    {
		m_pImpl->SetWindow(hWnd,nDefCmdShow);
    }

    bool Store(TStorage& stgMain)
    {
		TStorage general;
		if(general.Create(stgMain,ctxtGeneral,IStorge::ReadWrite)==ERROR_SUCCESS)
		{
			DWORD val=::GetSystemMetrics(SM_CXSCREEN);
			general.SetBinary(ctxtCXScreen,&val,sizeof(DWORD));
			val=::GetSystemMetrics(SM_CYSCREEN);
			general.SetBinary(ctxtCYScreen,&val,sizeof(DWORD));
		}

		TStorage stg;
		bool bRes=(stg.Create(stgMain,ctxtMainWindow,IStorge::ReadWrite)==ERROR_SUCCESS);
		if(bRes)
			bRes=m_pImpl->Store(stg);

//         DWORD dwDisposition;
// 		CRegKey keyMain;
//         if(keyMain.Create(HKEY_CURRENT_USER,m_strMainKey.c_str(),REG_NONE,
//                                     REG_OPTION_NON_VOLATILE, KEY_WRITE|KEY_READ,
//                                     NULL,&dwDisposition)==ERROR_SUCCESS)
//         {
//             CRegKey keyGeneral;
//             if(keyGeneral.Create(keyMain,ctxtGeneral,REG_NONE,
// 										REG_OPTION_NON_VOLATILE, KEY_WRITE|KEY_READ,
// 										NULL,&dwDisposition)==ERROR_SUCCESS)
//             {
// 
// 				DWORD val=::GetSystemMetrics(SM_CXSCREEN);
// 				::RegSetValueEx(keyGeneral, ctxtCXScreen, NULL, REG_DWORD,
// 								reinterpret_cast<BYTE*>(&val), sizeof(DWORD));
// 				val=::GetSystemMetrics(SM_CYSCREEN);
// 				::RegSetValueEx(keyGeneral, ctxtCYScreen, NULL, REG_DWORD,
// 								reinterpret_cast<BYTE*>(&val), sizeof(DWORD));
// /*
// 				keyGeneral.SetValue(::GetSystemMetrics(SM_CXSCREEN),ctxtCXScreen);
// 				keyGeneral.SetValue(::GetSystemMetrics(SM_CYSCREEN),ctxtCYScreen);
// */
//             }
//             CRegKey key;
//             if(key.Create(keyMain,ctxtMainWindow,REG_NONE,
// 							REG_OPTION_NON_VOLATILE, KEY_WRITE|KEY_READ,
// 							NULL,&dwDisposition)==ERROR_SUCCESS)
// 									m_pImpl->Store(key);
//         }
		return bRes;
    }

    bool Restore(TStorage& stgMain)
    {
		TStorage general;
		bool bRes=(general.Open(stgMain,ctxtGeneral,IStorge::Read)==ERROR_SUCCESS);
		if(bRes)
		{
            SIZE szScreen;
			size_t size = sizeof(DWORD);
			int res = general.GetBinary(ctxtCXScreen, &szScreen.cx, size);
			if (res != ERROR_NO_DATA)
			{
				// Nothing to load, we'll bail on loading settings here.
				float xratio=(res == ERROR_SUCCESS
								 && (size == sizeof(DWORD))
												?float(::GetSystemMetrics(SM_CXSCREEN))/szScreen.cx
												:float(1.0));
					
				size = sizeof(DWORD);
				float yratio=(general.GetBinary(ctxtCYScreen,&szScreen.cy,size)==ERROR_SUCCESS
								&& (size == sizeof(DWORD))
												?float(::GetSystemMetrics(SM_CYSCREEN))/szScreen.cy
												:float(1.0));

				TStorage stg;
				bRes=(stg.Open(stgMain,ctxtMainWindow,IStorge::Read)==ERROR_SUCCESS
								&& m_pImpl->Restore(stg,xratio,yratio));
			}
			else
			{
				bRes = false;
			}
		}

		if(!bRes)
		{
			m_pImpl->RestoreDefault();
		}

        return bRes;
    }

	bool RestoreDefault(void)
	{
		return m_pImpl->RestoreDefault();
	}
protected:
	CImpl*	m_pImpl;
};

class CWindowStateAdapter
{
protected:
	class CImpl : public CStateBase<IState>
	{
	public:
		CImpl(HWND hWnd,int nDefCmdShow=SW_SHOWNA)
			:m_hWnd(hWnd),m_nDefCmdShow(nDefCmdShow)
		{
			assert(::IsWindow(hWnd));
		}
		virtual bool Store(IStorge& stg)
		{
            WINDOWPLACEMENT wp;
            wp.length = sizeof(WINDOWPLACEMENT);
            assert(::IsWindow(m_hWnd));
            bool bRes=false;
            if (::GetWindowPlacement(m_hWnd,&wp))
            {
                wp.flags = 0;
                if (::IsZoomed(m_hWnd))
                        wp.flags |= WPF_RESTORETOMAXIMIZED;
/*
                bRes=(::RegSetValueEx(key,ctxtPlacement,NULL,REG_BINARY,
										reinterpret_cast<CONST BYTE *>(&wp),
										sizeof(WINDOWPLACEMENT))==ERROR_SUCCESS);
*/
				bRes=(stg.SetBinary(ctxtPlacement,&wp,sizeof(WINDOWPLACEMENT))==ERROR_SUCCESS);
            }
			return bRes;
		}
		virtual bool Restore(IStorge& stg,float /*xratio*/,float /*yratio*/)
		{
            assert(::IsWindow(m_hWnd));
            WINDOWPLACEMENT wp;
/*
            DWORD dwType;
            DWORD cbData=sizeof(WINDOWPLACEMENT);
            bool bRes=(::RegQueryValueEx(key,ctxtPlacement,NULL,&dwType,
											reinterpret_cast<LPBYTE>(&wp),&cbData)==ERROR_SUCCESS)
											&&(dwType==REG_BINARY);
*/
            size_t size=sizeof(WINDOWPLACEMENT);
			bool bRes=(stg.GetBinary(ctxtPlacement,&wp,size)==ERROR_SUCCESS
						&& (size == sizeof(WINDOWPLACEMENT) ) );
            if(bRes)
                    bRes=(::SetWindowPlacement(m_hWnd,&wp)!=FALSE);
            return bRes;
		}
		virtual bool RestoreDefault(void)
		{
			::ShowWindow(m_hWnd,m_nDefCmdShow);
			return true;
		}
	protected:
		HWND	m_hWnd;
		int		m_nDefCmdShow;
	};
public:
    CWindowStateAdapter(HWND hWnd,int nDefCmdShow=SW_SHOWNOACTIVATE)
    {
		m_pImpl = new CImpl(hWnd,nDefCmdShow);
    }
	~CWindowStateAdapter(void)
	{
		assert(m_pImpl);
		m_pImpl->Release();
	}
	operator IState* (void)
	{
		return m_pImpl;
	}
protected:
	CImpl* m_pImpl;
};

class CToggleWindowAdapter
{
protected:
	class CImpl : public CStateBase<IState>
	{
	public:
		CImpl(HWND hWnd,int nDefCmdShow=SW_SHOWNA)
			:m_hWnd(hWnd),m_nDefCmdShow(nDefCmdShow)
		{
			assert(::IsWindow(hWnd));
		}
		virtual bool Store(IStorge& stg)
		{
            DWORD visible=::IsWindowVisible(m_hWnd);
/*
			return (::RegSetValueEx(key, ctxtVisible, NULL, REG_DWORD,
								reinterpret_cast<BYTE*>(&visible), sizeof(DWORD))==ERROR_SUCCESS);
//            return (key.SetValue(visible,ctxtVisible)==ERROR_SUCCESS);
*/
			return (stg.SetBinary(ctxtVisible,&visible,sizeof(DWORD))==ERROR_SUCCESS);
		}
		virtual bool Restore(IStorge& stg,float /*xratio*/,float /*yratio*/)
		{
            DWORD visible;
//          bool bRes=(key.QueryValue(visible,ctxtVisible)==ERROR_SUCCESS);
/*
			DWORD dwCount = sizeof(DWORD);
			bool bRes=(::RegQueryValueEx(key,ctxtVisible,NULL,NULL,
								reinterpret_cast<LPBYTE>(&visible),&dwCount)==ERROR_SUCCESS
									 && (dwCount == sizeof(DWORD)));
*/
			size_t size=sizeof(DWORD);
			bool bRes=(stg.GetBinary(ctxtVisible,&visible,size)==ERROR_SUCCESS
							&& (size==sizeof(DWORD)));
            if(bRes)
                    ::ShowWindow(m_hWnd, (visible!=0) ? SW_SHOWNA : SW_HIDE);
            else
                    RestoreDefault();
            return bRes;
		}
		virtual bool RestoreDefault(void)
		{
            ::ShowWindow(m_hWnd,m_nDefCmdShow);
            return true;
		}
	protected:
		HWND	m_hWnd;
		int		m_nDefCmdShow;
	};
public:
    CToggleWindowAdapter(HWND hWnd,int nDefCmdShow=SW_SHOWNOACTIVATE)
    {
		m_pImpl = new CImpl(hWnd,nDefCmdShow);
    }
	~CToggleWindowAdapter(void)
	{
		assert(m_pImpl);
		m_pImpl->Release();
	}
	operator IState* (void)
	{
		return m_pImpl;
	}
protected:
	CImpl* m_pImpl;
};

class CRebarStateAdapter
{
protected:
	class CImpl : public CStateBase<IState>
	{
	public:
		CImpl(HWND hWnd)
			:m_rebar(hWnd)
		{
			assert(::IsWindow(hWnd));
		}

		virtual bool Store(IStorge& stg)
		{
			assert(m_rebar.IsWindow());
			unsigned int bandCount=m_rebar.GetBandCount();
			for(unsigned int i=0;i<bandCount;i++)
			{
				std::basic_ostringstream<TCHAR> sstrKey;
				sstrKey<<ctxtBand<<i;
				REBARBANDINFO rbi;
				ZeroMemory(&rbi,sizeof(REBARBANDINFO));
				rbi.cbSize = sizeof(REBARBANDINFO);
				rbi.fMask = RBBIM_ID | RBBIM_COLORS |
							RBBIM_SIZE | RBBIM_STYLE
							| RBBIM_CHILDSIZE
							#if (_WIN32_IE >= 0x0400)
								| /*RBBIM_HEADERSIZE |*/ RBBIM_IDEALSIZE
							#endif
								;
				m_rebar.GetBandInfo(i, &rbi);
/*
				::RegSetValueEx(key,sstrKey.str().c_str(),NULL,REG_BINARY,
								reinterpret_cast<CONST BYTE *>(&rbi),
								rbi.cbSize);
*/
				stg.SetBinary(sstrKey.str().c_str(),&rbi,size_t(rbi.cbSize));
			}
			return true;
		}

		virtual bool Restore(IStorge& stg,float /*xratio*/,float /*yratio*/)
		{
			unsigned int bandCount=m_rebar.GetBandCount();
			for(unsigned int i=0;i<bandCount;i++)
			{
				std::basic_ostringstream<TCHAR> sstrKey;
				sstrKey<<ctxtBand<<i;
				REBARBANDINFO rbi;
				
				REBARBANDINFO rbiOrig;
				ZeroMemory(&rbiOrig, sizeof(REBARBANDINFO));
				rbiOrig.cbSize = sizeof(REBARBANDINFO);
				rbiOrig.fMask = RBBIM_CHILDSIZE;
				m_rebar.GetBandInfo(i, &rbiOrig);

				//ZeroMemory(&rbi,sizeof(REBARBANDINFO));
/*
				DWORD dwType;
				DWORD cbData=sizeof(REBARBANDINFO);
	            if((::RegQueryValueEx(key,sstrKey.str().c_str(),NULL,&dwType,
							reinterpret_cast<LPBYTE>(&rbi),&cbData)==ERROR_SUCCESS)
							&&(dwType==REG_BINARY))
*/
				// We don't want to reset the vertical size of the rebar
				// or we'll break vertical font sizing
				
				size_t size = sizeof(REBARBANDINFO);
				if (stg.GetBinary(sstrKey.str().c_str(), &rbi, size) == ERROR_SUCCESS &&
					(size == sizeof(REBARBANDINFO)))
				{
					rbi.cyMinChild = rbiOrig.cyMinChild;
					rbi.cyMaxChild = rbiOrig.cyMaxChild;
					m_rebar.MoveBand(m_rebar.IdToIndex(rbi.wID), i);
					m_rebar.SetBandInfo(i, &rbi);
				}
			}
			return true;

		}
		virtual bool RestoreDefault(void)
		{
			return true;
		}
	protected:
		CReBarCtrl m_rebar;
	};
public:
    CRebarStateAdapter(HWND hWnd)
    {
		m_pImpl = new CImpl(hWnd);
    }
	~CRebarStateAdapter(void)
	{
		assert(m_pImpl);
		m_pImpl->Release();
	}
	operator IState* (void)
	{
		return m_pImpl;
	}
protected:
	CImpl* m_pImpl;
};

}//namespace sstate
#endif // __WTL_DW__SSTATE_H__
