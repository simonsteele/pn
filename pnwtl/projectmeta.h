/**
 * @file projectmeta.h
 * @brief Project MetaData
 * @author Simon Steele
 * @note Copyright (c) 2004-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projectmeta_h__included_49A5C619_DF7C_44c3_B139_7F26780C832E
#define projectmeta_h__included_49A5C619_DF7C_44c3_B139_7F26780C832E

namespace Projects
{

class ProjectWriter;

class XmlNode;
class XmlAttribute;

typedef std::list<XmlNode*>			LIST_NODES;
typedef std::list<XmlAttribute*>	LIST_ATTRS;

typedef LIST_NODES::iterator		XN_IT;
typedef LIST_NODES::const_iterator	XN_CIT;

typedef LIST_ATTRS::const_iterator	XA_IT;

/**
 * Represent a single XML metadata node in the project file.
 */
class XmlNode
{
	public:
		explicit XmlNode(LPCTSTR qualifiedName);
		explicit XmlNode(LPCTSTR lpszNamespace, LPCTSTR lpszName);
		explicit XmlNode(const XmlNode& copy);
		
		~XmlNode();

		XmlNode& operator = (const XmlNode& copy);
		
		void Write(ProjectWriter* writer);

		void AddAttributes(const XMLAttributes& atts);
		void AddChild(XmlNode* pChild);

		XmlNode* GetParent();
		LIST_NODES& GetChildren();

		LPCTSTR GetText();
		void SetText(LPCTSTR text);

		bool Matches(LPCTSTR ns, LPCTSTR name);

	private:
		void clear();

		tstring		sNamespace;
		tstring		sName;
		tstring		sText;

		XmlNode*	pParent;
		
		LIST_NODES	children;
		LIST_ATTRS	attributes;
};

/**
 * Represent a single attribute in a node.
 */
class XmlAttribute
{
	public:
		XmlAttribute(LPCTSTR lpszNamespace, LPCTSTR lpszName, LPCTSTR lpszValue);
		XmlAttribute(const XmlAttribute& copy);

		XmlAttribute& operator = (const XmlAttribute& copy);

		void Write(ProjectWriter* writer);

	private:
		tstring		sNamespace;
		tstring		sName;
		tstring		sValue;
};

///**
// * Interface class for things that want to provide meta data.
// */
//class IMetaDataProvider
//{
//public:
//	virtual bool Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool defval) = 0;
//	virtual int Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int defval) = 0;
//	virtual LPCTSTR Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR defval) = 0;
//
//	virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool val) = 0;
//	virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int val) = 0;
//	virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR val) = 0;
//};

/**
 * IMetaDataProvider implementation for XML nodes under project data types.
 */
class UserData /*: public IMetaDataProvider*/
{
	public:
		~UserData();
		
		UserData& operator = (const UserData& copy);

		void Add(XmlNode* node);

		const LIST_NODES& GetNodes();

		const int GetCount();

		void Write(ProjectWriter* writer);

		virtual bool Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool defval);
		virtual int Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int defval);
		virtual LPCTSTR Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR defval);

		virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool val);
		virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int val);
		virtual void Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR val);

		XmlNode* GetCategoryNode(LPCTSTR ns, LPCTSTR group, LPCTSTR category);
		XmlNode* GetGroupNode(LPCTSTR ns, LPCTSTR group);

		XN_CIT	begin();
		XN_CIT	end();

	private:
		void clear();
		XmlNode* locate(const LIST_NODES& nodes, LPCTSTR ns, LPCTSTR node);
		XmlNode* lookUp(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value);
		XmlNode* lookUpOrCreate(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value);

		LIST_NODES nodes;
		//LIST_ATTRS attrs;
};

}

#endif //#ifndef projectmeta_h__included_49A5C619_DF7C_44c3_B139_7F26780C832E