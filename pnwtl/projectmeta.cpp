/**
 * @file projectmeta.cpp
 * @brief Project MetaData
 * @author Simon Steele
 * @note Copyright (c) 2004-2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "projectmeta.h"
#include "project.h"

#include "include/encoding.h"
#include "projectwriter.h"

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// XmlNode
//////////////////////////////////////////////////////////////////////////////

XmlNode::XmlNode(LPCTSTR lpszNamespace, LPCTSTR lpszName)
{
	sNamespace = lpszNamespace;
	sName = lpszName;
	pParent = NULL;
}

XmlNode::XmlNode(LPCTSTR qualifiedName)
{
	LPCTSTR pSep = _tcsrchr(qualifiedName, _T(':'));

	if(pSep != NULL)
	{
		TCHAR* buf = new TCHAR[_tcslen(qualifiedName)+1];

		sName = pSep + 1;
		int nslen = pSep - qualifiedName;
		_tcsncpy(buf, qualifiedName, nslen);
		buf[nslen] = _T('\0');
		sNamespace = buf;
		
		delete [] buf;
	}
	else
		sName = qualifiedName;

	pParent = NULL;
}

XmlNode::XmlNode(const XmlNode& copy)
{
	pParent = NULL;
	*this = copy;
}

XmlNode::~XmlNode()
{
	clear();
}

XmlNode& XmlNode::operator = (const XmlNode& copy)
{
	sNamespace = copy.sNamespace;
	sName = copy.sName;
	sText = copy.sText;

	// Don't copy: pParent;
	clear();
	
	for(XA_IT i = copy.attributes.begin(); i != copy.attributes.end(); ++i)
	{
		XmlAttribute* newAtt = new XmlAttribute( *(*i) );
		attributes.insert(attributes.end(), newAtt);
	}

	for(XN_CIT j = copy.children.begin(); j != copy.children.end(); ++j)
	{
		XmlNode* newNode = new XmlNode( *(*j) );
		AddChild(newNode);
	}

	return *this;
}

void XmlNode::AddAttributes(const XMLAttributes& atts)
{
	LPCTSTR pSep;
	for(int i = 0; i < atts.getCount(); i++)
	{
		XmlAttribute* a;

		LPCTSTR nm = atts.getName(i);
		pSep = _tcsrchr(nm, _T(':'));
		if(pSep)
		{
			int nslen = (pSep - nm) / sizeof(TCHAR);
			TCHAR* buf = new TCHAR[nslen+1];
			_tcsncpy(buf, nm, nslen);
			buf[nslen] = _T('\0');
			
			a = new XmlAttribute(buf, pSep+1, atts.getValue(i));

			delete [] buf;
		}
		else
		{
			a = new XmlAttribute(NULL, nm, atts.getValue(i));
		}
		
		attributes.insert(attributes.end(), a);
	}
}

void XmlNode::AddChild(XmlNode* pChild)
{
	children.insert(children.end(), pChild);
	pChild->pParent = this;
}

XmlNode* XmlNode::GetParent()
{
	return pParent;
}

LIST_NODES& XmlNode::GetChildren()
{
	return children;
}

void XmlNode::Write(ProjectWriter* writer)
{
	Tcs_Utf8 ns(sNamespace.c_str());
	Tcs_Utf8 name(sName.c_str());
	genxStartElementLiteral(writer->GetWriter(), sNamespace.length() ? (constUtf8)ns : (constUtf8)NULL, name);
	
	for(LIST_ATTRS::iterator i = attributes.begin(); i != attributes.end(); ++i)
	{
		(*i)->Write(writer);
	}
	
	for(LIST_NODES::iterator j = children.begin(); j != children.end(); ++j)
	{
		(*j)->Write(writer);
	}

	if(sText.length() > 0)
	{
		Tcs_Utf8 ustr(sText.c_str());
		genxAddText(writer->GetWriter(), ustr);
	}
	
	genxEndElement(writer->GetWriter());
}

LPCTSTR XmlNode::GetText()
{
	return sText.c_str();
}

void XmlNode::SetText(LPCTSTR text)
{
	sText = text;
}

bool XmlNode::Matches(LPCTSTR ns, LPCTSTR name)
{
	return (_tcscmp(ns, sNamespace.c_str()) == 0 && _tcscmp(name, sName.c_str()) == 0);
}

void XmlNode::clear()
{
	for(XA_IT i = attributes.begin(); i != attributes.end(); ++i)
	{
		delete (*i);
	}
	attributes.clear();

	for(XN_IT j = children.begin(); j != children.end(); ++j)
	{
		delete (*j);
	}
	children.clear();
}

//////////////////////////////////////////////////////////////////////////////
// XmlAttribute
//////////////////////////////////////////////////////////////////////////////

XmlAttribute::XmlAttribute(LPCTSTR lpszNamespace, LPCTSTR lpszName, LPCTSTR lpszValue)
{
	sNamespace = lpszNamespace != NULL ? lpszNamespace : _T("");
	sName = lpszName;
	sValue = lpszValue;
}

XmlAttribute::XmlAttribute(const XmlAttribute& copy)
{
	*this = copy;
}

XmlAttribute& XmlAttribute::operator = (const XmlAttribute& copy)
{
	sNamespace = copy.sNamespace;
	sName = copy.sName;
	sValue = copy.sValue;

	return *this;
}

void XmlAttribute::Write(ProjectWriter* writer)
{
	Tcs_Utf8 ns(sNamespace.c_str());
	Tcs_Utf8 name(sName.c_str());
	Tcs_Utf8 att(sValue.c_str());
	genxAddAttributeLiteral(writer->GetWriter(), sNamespace.length() ? (constUtf8)ns : (constUtf8)NULL, name, att);
}

//////////////////////////////////////////////////////////////////////////////
// UserData
//////////////////////////////////////////////////////////////////////////////

UserData::~UserData()
{
	clear();
}

UserData& UserData::operator = (const UserData& copy)
{
	clear();

	for(XN_CIT i = copy.nodes.begin(); i != copy.nodes.end(); ++i)
	{
		XmlNode* pNewNode = new XmlNode( *(*i) );
		Add(pNewNode);
	}

	return *this;
}
		
void UserData::Add(XmlNode* node)
{
	nodes.insert(nodes.end(), node);
}

const LIST_NODES& UserData::GetNodes()
{
	return nodes;
}

const int UserData::GetCount()
{
	return nodes.size();
}

void UserData::Write(ProjectWriter* writer)
{
	for(XN_CIT i = nodes.begin(); i != nodes.end(); ++i)
	{
		(*i)->Write(writer);
	}
}

bool UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL && _tcslen(text) > 0)
			return _tcsicmp(text, _T("true")) == 0;
	}
	
	return defval;
}

int UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL && _tcslen(text) > 0)
			return _ttoi(text);
	}
	
	return defval;
}

LPCTSTR UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL)
			return text;
	}
	
	return defval;
}

void UserData::Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool val)
{
	XmlNode* pNode = lookUpOrCreate(ns, group, category, value);
	if(pNode != NULL)
	{
		pNode->SetText( val ? _T("true") : _T("false") );
	}
	else
		UNEXPECTED(_T("Could not lookUpOrCreate Node"));
}

void UserData::Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int val)
{
	XmlNode* pNode = lookUpOrCreate(ns, group, category, value);
	if(pNode != NULL)
	{
		tstring sVal = IntToTString(val);
		pNode->SetText( sVal.c_str() );
	}
	else
		UNEXPECTED(_T("Could not lookUpOrCreate Node"));
}

void UserData::Set(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR val)
{
	XmlNode* pNode = lookUpOrCreate(ns, group, category, value);
	if(pNode != NULL)
	{
		pNode->SetText( val );
	}
	else
		UNEXPECTED(_T("Could not lookUpOrCreate Node"));
}

/**
 * This function calls GetGroupNode to find the correct node for the requested group
 * and then searches that group for the relevant category.
 */
XmlNode* UserData::GetCategoryNode(LPCTSTR ns, LPCTSTR group, LPCTSTR category)
{
	PNASSERT(ns != NULL);
	PNASSERT(group != NULL);
	PNASSERT(category != NULL);

	XmlNode* pGroupNode = GetGroupNode(ns, group);

	if(!pGroupNode)
		return NULL;

	for(LIST_NODES::const_iterator j = pGroupNode->GetChildren().begin();
		j != pGroupNode->GetChildren().end();
		++j)
	{
		if( (*j)->Matches(ns, category) )
		{
			return *j;
		}
	}

	return NULL;
}

/**
 * This finds the top-level UserData element, the group. Groups can be nested,
 * and the group string can represent a nested target:
 * "Compiler\\Optimisations"
 */
XmlNode* UserData::GetGroupNode(LPCTSTR ns, LPCTSTR group)
{
	PNASSERT(ns != NULL);
	PNASSERT(group != NULL);

	if(_tcschr(group, _T('\\')) != NULL)
	{
		// We have a nested path to walk.
		std::vector<tstring> elements;
		StringTokenise(tstring(group), elements, tstring(_T("\\")));

		XmlNode* group = NULL;

		for(std::vector<tstring>::const_iterator i = elements.begin();
			i != elements.end();
			++i)
		{
			// See if we can find the group node at this level.
			group = locate( group == NULL ? nodes : group->GetChildren(), ns, (*i).c_str() );
			if(group == NULL)
				break;
		}

		return group;
	}
	else
	{
		// Simple top-level group.
		return locate(nodes, ns, group);
	}

	return NULL;
}

void UserData::clear()
{
	for(XN_CIT i = nodes.begin(); i != nodes.end(); ++i)
	{
		delete (*i);
	}
	nodes.clear();
}

XmlNode* UserData::locate(const LIST_NODES& nodelist, LPCTSTR ns, LPCTSTR node)
{
	for(LIST_NODES::const_iterator i = nodelist.begin(); i != nodelist.end(); ++i)
	{
		if( (*i)->Matches(ns, node) )
		{
			return *i;
		}
	}

	return NULL;
}

/**
 * This all-important function is used to walk the XmlNodes collection looking
 * for the correct metadata node.
 */
XmlNode* UserData::lookUp(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value)
{
	XmlNode* pCatNode = GetCategoryNode(ns, group, category);

	if(!pCatNode)
		return NULL;

	for(LIST_NODES::const_iterator k = pCatNode->GetChildren().begin();
		k != pCatNode->GetChildren().end();
		++k)
	{
		if( (*k)->Matches(ns, value) )
			return (*k);
	}

	return NULL;
}

XmlNode* UserData::lookUpOrCreate(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value)
{
	XmlNode* pNode = NULL;
	pNode = lookUp(ns, group, category, value);
	
	// If we didn't find it, then we create it - making sure 
	// we have the group and category as well...
	if(pNode == NULL)
	{
		XmlNode* pGroupNode = GetGroupNode(ns, group);
		if(pGroupNode == NULL)
		{
			pGroupNode = new XmlNode(ns, group);
			Add(pGroupNode);
		}
		
		XmlNode* pCatNode = GetCategoryNode(ns, group, category);
		if(pCatNode == NULL)
		{
			pCatNode = new XmlNode(ns, category);
			pGroupNode->AddChild(pCatNode);
		}

		pNode = new XmlNode(ns, value);
		pCatNode->AddChild(pNode);
	}
	
	return pNode;
}

XN_CIT UserData::begin()
{
	return nodes.begin();
}

XN_CIT UserData::end()
{
	return nodes.end();
}

} // namespace Projects