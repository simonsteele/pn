#ifndef projectholder_h__included
#define projectholder_h__included

MIDL_INTERFACE("41B6F50B-4779-4ff4-A6D4-7F6BF21F1A78")
IProjectHolder : public IUnknown
{
public:
	//virtual HRESULT STDMETHODCALLTYPE put_Project(Projects::Project* project) = 0;
	//virtual HRESULT STDMETHODCALLTYPE put_ProjectGroup(Projects::Workspace* workspace) = 0;
	virtual HRESULT STDMETHODCALLTYPE get_Project(Projects::Project** project) = 0;
	virtual HRESULT STDMETHODCALLTYPE get_ProjectGroup(Projects::Workspace** workspace) = 0;
};

class CProjectHolder : public CComObjectRootEx<CComMultiThreadModel>,
	public IProjectHolder
{
public:
	CProjectHolder() : pProject(NULL), pProjectGroup(NULL)
	{
	}

	BEGIN_COM_MAP(CProjectHolder)
		COM_INTERFACE_ENTRY(IProjectHolder)
	END_COM_MAP()

	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}

	void FinalRelease()
	{
	}

// Non-interface methods
public:
	STDMETHOD(put_Project)(Projects::Project* project)
	{
		pProject = project;
		
		return S_OK;
	}

	STDMETHOD(put_ProjectGroup)(Projects::Workspace* projectGroup)
	{
		pProjectGroup = projectGroup;

		return S_OK;
	}

// IProjectHolder
public:
	STDMETHOD(get_Project)(Projects::Project** project)
	{
		*project = pProject;
		
		return S_OK;
	}

	STDMETHOD(get_ProjectGroup)(Projects::Workspace** projectGroup)
	{
		*projectGroup = pProjectGroup;

		return S_OK;
	}

protected:
	Projects::Workspace*	pProjectGroup;
	Projects::Project*		pProject;
};

HRESULT CreateProjectHolder(Projects::Project* pProject, Projects::Workspace* pProjectGroup,
	IProjectHolder** holder)
{
	HRESULT hr = E_NOINTERFACE;

	CComObject<CProjectHolder>* newHolder = NULL;
	hr = CComObject<CProjectHolder>::CreateInstance(&newHolder);
	if(newHolder != NULL)
	{
		newHolder->AddRef();

		hr = newHolder->put_Project(pProject);
		
		if(SUCCEEDED(hr))
			hr = newHolder->put_ProjectGroup(pProjectGroup);

		if(SUCCEEDED(hr))
		{
			hr = newHolder->QueryInterface(holder);
		}

		newHolder->Release();
	}

	return hr;
}

#endif // #ifndef projectholder_h__included