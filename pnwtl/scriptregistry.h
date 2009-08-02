/**
 * @file scriptregistry.h
 * @brief Define the script registry and related classes
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef scriptregistry_h__included
#define scriptregistry_h__included

class Script;
class ScriptGroup;

typedef std::list<Script*> script_list_t;
typedef std::list<ScriptGroup*> group_list_t;
typedef std::map<std::string, extensions::IScriptRunner*> s_runner_map;

class IScriptRegistryEventSink
{
public:
	virtual void OnScriptAdded(ScriptGroup* group, Script* script) = 0;
	virtual void OnScriptRemoved(ScriptGroup* group, Script* script) = 0;
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

	void Add(const char* group, Script* script);
	void Remove(const char* group, Script* script);

	virtual void Add(const char* group, const char* name, const char* scriptref);

	Script* FindScript(const char* group, const char* name);

	virtual void RegisterRunner(const char* id, extensions::IScriptRunner* runner);
	virtual void RemoveRunner(const char* id);
	virtual extensions::IScriptRunner* GetRunner(const char* id);

	/**
	 * Enable scheme scripts for a given scheme via this runner.
	 */
	virtual void EnableSchemeScripts(const char* scheme, const char* runnerId);

	/**
	 * Find out if a particular scheme has a registered script runner.
	 * If it does, then open documents can be run as scripts by that runner.
	 */
	bool SchemeScriptsEnabled(const char* scheme);
	bool SchemeScriptsEnabled(const char* scheme, std::string& runner);

	void SetEventSink(IScriptRegistryEventSink* sink);

protected:
	ScriptRegistry();
	virtual ~ScriptRegistry();

	void clear();

	ScriptGroup* getOrMakeGroup(const char* group);
	ScriptGroup* getGroup(const char* group);

protected:
	IScriptRegistryEventSink* m_sink;
	group_list_t m_groups;
	s_runner_map m_runners;
	string_map m_scriptableSchemes;
};

/**
 * Named group of scripts
 */
class ScriptGroup
{
public:
	explicit ScriptGroup(const char* name);
	~ScriptGroup();

	/**
	 * Add a script with a name and a unique reference.
	 */
	Script* Add(const char* name, const char* scriptref);
	
	/**
	 * Add a Script instance.
	 */
	void Add(Script* script);
	
	/**
	 * Remove a Script instance.
	 */
	void Remove(Script* script);
	
	/**
	 * Get a script instance by name, or null if not found.
	 */
	Script* Get(const char* name);

	void Clear();

	const script_list_t& GetScripts();

	/**
	 * Get the name of this group.
	 */
	const char* GetName() const;

private:
	void clear();

	std::string		m_name;
	script_list_t	m_scripts;
};

/**
 * Reference to a single script.
 */
class Script
{
public:
	explicit Script(const char* name, const char* scriptref) : Name(name), ScriptRef(scriptref){}

	explicit Script(const Script& copy)
	{
		*this = copy;
	}

	Script& operator = (const Script& copy)
	{
		Name = copy.Name;
		ScriptRef = copy.ScriptRef;
		return *this;
	}
	
	std::string Name;
	std::string ScriptRef;

	virtual void Run();
};

/**
 * This is a script that is an open PN document.
 */
class DocScript : public Script
{
public:
	explicit DocScript(const char* name, const char* runner, DocumentPtr& doc) 
		: Script(name, ""), m_runner(runner), m_doc(doc){}

	virtual void Run();

private:
	DocScript(const DocScript& copy) : Script(copy)
	{
		PNASSERT(false);
	}

	DocScript& operator = (const DocScript& copy)
	{
		PNASSERT(false);
		return *this;
	}

	std::string m_runner;
	DocumentPtr m_doc;
};

#endif