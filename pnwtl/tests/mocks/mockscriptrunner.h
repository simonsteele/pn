#ifndef MOCKSCRIPTRUNNER_H__INCLUDED
#define MOCKSCRIPTRUNNER_H__INCLUDED

class FakeScriptRunner : public extensions::IScriptRunner
{
public:
	virtual ~FakeScriptRunner(){}

	/**
	 * This method requests that a runner runs a named
	 * script that it has previously registered with the
	 * registry.
	 */
	virtual void RunScript(const char* name) { throw "Not Implemented"; }
	
	/**
	 * This method requests that a runner runs the text
	 * of a given document as a script.
	 */
	virtual void RunDocScript(extensions::IDocumentPtr& doc) { throw "Not Implemented"; }

	/**
	 * Request a runner to attempt to run the script fragment provided,
	 * this is used for allowing scripting to be used in tool parameters
	 * etc.
	 */
	virtual void Eval(const char* script, PN::BaseString& output)
	{
		output = "Hello World";
	}

	virtual void Exec(const char *, const char *, int, PN::BaseString& output)
	{
		output = "Hello World";
	}
};

#endif