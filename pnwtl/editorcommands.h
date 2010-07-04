/**
 * @file editorcommands.h
 * @brief Built in text buffer commands
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef editorcommands_h__included
#define editorcommands_h__included

class CScintillaImpl;

namespace Commands
{

/**
 * Base class for Editor Commands
 */
class EditorCommand
{
public:
	/**
	 * Apply this command to the editor.
	 */
	void Apply(CScintillaImpl& editor)
	{
		DoApply(editor);
	}

	/**
	 * Get an ID for this command.
	 */
	int GetCommandID()
	{
		return DoGetCommand();
	}

protected:
	/**
	 * Do the editor work in this method
	 */
	virtual void DoApply(CScintillaImpl& editor) = 0;

	/**
	 * Get the command ID
	 */
	virtual int DoGetCommand() = 0;

	/**
	 * Avoid instantiation of base
	 */
	EditorCommand() {}
};

namespace Internal
{
	class EditorCommandFn : public EditorCommand
	{
	public:
		EditorCommandFn(int commandId, boost::function<void (CScintillaImpl&)> handler) :
		  m_commandId(commandId),
		  m_handler(handler)
		{}
	protected:
		virtual void DoApply(CScintillaImpl& editor)
		{
			m_handler(editor);
		}

		virtual int DoGetCommand()
		{
			return m_commandId;
		}

	private:
		int m_commandId;
		boost::function<void (CScintillaImpl&)> m_handler;
	};
} // namespace Internal

extern void GetEditorCommands(std::list<EditorCommand*>& commands);

}

#endif //#ifndef editorcommands_h__included