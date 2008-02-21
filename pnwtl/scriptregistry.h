#ifndef scriptregistry_h__included
#define scriptregistry_h__included

class Script;
class ScriptGroup;

typedef std::list<Script*> script_list_t;
typedef std::list<ScriptGroup*> group_list_t;
typedef std::map<tstring, extensions::IScriptRunner*> s_runner_map;

class IScriptRegistryEventSink
{
public:
	virtual void OnScriptAdded(ScriptGroup* group, Script* script) = 0;
};

/**
 * Singleton managing references to scripts throughout the system.
 */
class ScriptRegistry : public Singleton<ScriptRegistry, true>,
	public extensions::IScriptRegistry
{
friend class Singleton<ScriptRegistry, true>;
public:	
	void Clear();

	const group_list_t& GetGroups();

	virtual void Add(LPCTSTR group, LPCTSTR name, LPCTSTR scriptref);

	virtual void RegisterRunner(LPCTSTR id, extensions::IScriptRunner* runner);
	virtual void RemoveRunner(LPCTSTR id);
	virtual extensions::IScriptRunner* GetRunner(LPCTSTR id);

	void SetEventSink(IScriptRegistryEventSink* sink);

protected:
	ScriptRegistry();
	~ScriptRegistry();

	void clear();

protected:
	IScriptRegistryEventSink* m_sink;
	group_list_t m_groups;
	s_runner_map m_runners;
};

class ScriptGroup
{
public:
	ScriptGroup(LPCTSTR name);
	~ScriptGroup();

	Script* Add(LPCTSTR name, LPCTSTR scriptref);

	void Clear();

	const script_list_t& GetScripts();

	LPCTSTR GetName() const;

protected:
	void clear();

	tstring			m_name;
	script_list_t	m_scripts;
};

class Script
{
public:
	Script(LPCTSTR name, LPCTSTR scriptref) : Name(name), ScriptRef(scriptref){}

	Script(const Script& copy)
	{
		*this = copy;
	}

	Script& operator = (const Script& copy)
	{
		Name = copy.Name;
		ScriptRef = copy.ScriptRef;
		return *this;
	}
	
	tstring Name;
	tstring ScriptRef;

	void Run();
};

#endif