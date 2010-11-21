/**
 * @file usersettingswriter.h
 * @brief Write out the UserSettings file.
 * @author Simon Steele
 * @note Copyright (c) 2005-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef usersettingswriter_h__included
#define usersettingswriter_h__included

#ifdef _MSC_VER
	#pragma once
#endif

/////////////////////////////////////////////////////////
// Writer
/////////////////////////////////////////////////////////

namespace Schemes
{
	class Writer : public GenxXMLWriter
	{
		public:
			void beginDoc()
			{
				genxStartElementLiteral(m_writer, NULL, u("UserSettings"));
			}

			void endDoc()
			{
				pop();
			}

			void beginSchemes()
			{
				genxStartElementLiteral(m_writer, NULL, u("schemes"));
			}

			void endSchemes()
			{
				pop();
			}

			void beginScheme(SchemeDetails* scheme)
			{
				genxStartElement(m_eScheme);
				genxAddAttribute(m_aName, u(scheme->Name.c_str()));

				if((scheme->CustomFlags & USETABFOLDFLAGSMASK) != (scheme->Flags & USETABFOLDFLAGSMASK))
				{
					genxAddAttribute(m_aOverrideTabs, (scheme->CustomFlags & schOverrideTabs) ? u("true") : u("false"));
					genxAddAttribute(m_aUseTabs, (scheme->CustomFlags & schUseTabs) ? u("true") : u("false"));
				}

				if(scheme->CustomFlags & schOverrideTabSize)
				{
					genxAddAttribute(m_aTabWidth, u(IntToString(scheme->CustomTabWidth).c_str()));
				}
			}

			void endScheme()
			{
				pop();
			}

			void beginOverrideStyles()
			{
				genxStartElement(m_eOvStyles);
			}

			void endOverrideStyles()
			{
				pop();
			}

			void beginOverrideKeywords()
			{
				genxStartElement(m_eOvKeywords);
			}

			void endOverrideKeywords()
			{
				pop();
			}

			void beginOverrideClasses()
			{
				genxStartElement(m_eOvClasses);
			}

			void endOverrideClasses()
			{
				pop();
			}

			void beginStyle(int key)
			{
				genxStartElement(m_eStyle);
				genxAddAttribute(m_aKey, u(IntToString(key).c_str()));
			}

			void beginStyleClass(LPCTSTR name)
			{
				genxStartElement(m_eStyleClass);
				addAttributeConvertUTF8(m_aName, name);
			}

			void writeKeywords(int key, LPCSTR keywords)
			{
				genxStartElement(m_eKeywords);
				genxAddAttribute(m_aKey, u(IntToString(key).c_str()));
				Windows1252_Utf8 kwconv(keywords);
				genxAddText(m_writer, u(kwconv));
				pop();
			}

			void writeEditorColours(EditorColours* colours)
			{
				if(!colours->HasColours())
					return;
	
				genxStartElement(m_eColours);
				writeColours(colours);
				pop();
			}

			void writeStyleClass(const StyleDetails& style)
			{
				beginStyleClass(style.name.c_str());
				writeStyle(style, true);
				endStyleClass();
			}

			void writeStyle(const StyleDetails& style)
			{
				beginStyle(style.Key);
				writeStyle(style, true);
				endStyle();
			}

			void setFont(LPCTSTR name)
			{
                Tcs_Utf8 fontName(name);
				genxAddAttribute(m_aFont, u(fontName));
			}

			void setSize(int size)
			{
				genxAddAttribute(m_aSize, u(IntToString(size).c_str()));
			}

			void setFore(COLORREF fore)
			{
				addColourAtt(m_aFore, fore);
			}

			void setBack(COLORREF back)
			{
				addColourAtt(m_aBack, back);
			}

			void setBold(bool on)
			{
				if(on)
				{
					genxAddAttribute(m_aBold, u("true"));
				}
				else
				{
					genxAddAttribute(m_aBold, u("false"));
				}
			}

			void setItalic(bool on)
			{
				if(on)
				{
					genxAddAttribute(m_aItalic, u("true"));
				}
				else
				{
					genxAddAttribute(m_aItalic, u("false"));
				}
			}

			void setUnderline(bool on)
			{
				if(on)
				{
					genxAddAttribute(m_aUnderline, u("true"));
				}
				else
				{
					genxAddAttribute(m_aUnderline, u("false"));
				}
			}

			void setEolFilled(bool on)
			{
				if(on)
				{
					genxAddAttribute(m_aEolFilled, u("true"));
				}
				else
				{
					genxAddAttribute(m_aEolFilled, u("false"));
				}
			}

			void setClass(LPCTSTR theClass)
			{
				Tcs_Utf8 className(theClass);
				genxAddAttribute(m_aClass, className);
			}

			void endStyle()
			{
				pop();
			}

			void endStyleClass()
			{
				pop();
			}

			void writeOverrideColours(EditorColours* colours)
			{
				if(!colours->HasColours())
					return;

				genxStartElement(m_eOvColours);
				writeColours(colours);
				pop();
			}

			void writeColours(EditorColours* colours)
			{
				COLORREF colour;
				if (colours->GetColour(EditorColours::ecSelFore, colour))
				{
					addColourAtt(m_aSelFore, colour);
				}

				if (colours->GetColour(EditorColours::ecSelBack, colour))
				{
					addColourAtt(m_aSelBack, colour);
				}

				if (colours->GetColour(EditorColours::ecCaret, colour))
				{
					addColourAtt(m_aCaret, colour);
				}

				if (colours->GetColour(EditorColours::ecIndentG, colour))
				{
					addColourAtt(m_aIndentGuides, colour);
				}
				
				if (colours->GetColour(EditorColours::ecMarkAll, colour))
				{
					addColourAtt(m_aMarkAll, colour);
				}

				if (colours->GetColour(EditorColours::ecSmartHL, colour))
				{
					addColourAtt(m_aSmartHL, colour);
				}

				if (colours->GetColour(EditorColours::ecTemplateField, colour))
				{
					addColourAtt(m_aTemplateField, colour);
				}
			}

		protected:
			void writeStyle(const StyleDetails& style, bool bIsClass)
			{
				if(style.values & edvFontName)
					setFont(style.FontName.c_str());
				if(style.values & edvFontSize)
					setSize(style.FontSize);
				if(style.values & edvForeColor)
					setFore(style.ForeColor);
				if(style.values & edvBackColor)
					setBack(style.BackColor);
				if(style.values & edvBold)
					setBold(style.Bold);
				if(style.values & edvItalic)
					setItalic(style.Italic);
				if(style.values & edvUnderline)
					setUnderline(style.Underline);
				if(style.values & edvEOLFilled)
					setEolFilled(style.EOLFilled);
				// @todo
				if(style.values & edvClass && !bIsClass)
					setClass(style.classname.c_str());
			}

			void addColourAtt(genxAttribute att, COLORREF colour)
			{
				if(colour == CLR_NONE)
				{
					genxAddAttribute(att, u("-1"));
				}
				else
				{
					char colbuf[12];
					colbuf[11] = NULL;
					_snprintf(colbuf, 11, "%.2x%.2x%.2x", GetRValue(colour), GetGValue(colour), GetBValue(colour));
					genxAddAttribute(att, u(colbuf));
				}
			}

			virtual void initXmlBits()
			{
				genxStatus s;

				m_eScheme = genxDeclareElement(m_writer, NULL, u("scheme"), &s);
				m_eStyle = genxDeclareElement(m_writer, NULL, u("style"), &s);
				m_eStyleClass = genxDeclareElement(m_writer, NULL, u("style-class"), &s);
				m_eKeywords = genxDeclareElement(m_writer, NULL, u("keywords"), &s);
				m_eOvStyles = genxDeclareElement(m_writer, NULL, u("override-styles"), &s);
				m_eOvKeywords = genxDeclareElement(m_writer, NULL, u("override-keywords"), &s);
				m_eOvClasses = genxDeclareElement(m_writer, NULL, u("override-classes"), &s);
				m_eColours = genxDeclareElement(m_writer, NULL, u("colours"), &s);
				m_eOvColours = genxDeclareElement(m_writer, NULL, u("override-colours"), &s);

				PREDECLARE_ATTRIBUTES()
					ATT("key", m_aKey);
					ATT("font", m_aFont);
					ATT("size", m_aSize);
					ATT("fore", m_aFore);
					ATT("back", m_aBack);
					ATT("bold", m_aBold);
					ATT("italic", m_aItalic);
					ATT("underline", m_aUnderline);
					ATT("eolfilled", m_aEolFilled);
					ATT("class", m_aClass);
					ATT("name", m_aName);
					ATT("selFore", m_aSelFore);
					ATT("selBack", m_aSelBack);
					ATT("caret", m_aCaret);
					ATT("indentGuides", m_aIndentGuides);
					ATT("ovtabs", m_aOverrideTabs);
					ATT("usetabs", m_aUseTabs);
					ATT("tabwidth", m_aTabWidth);
					ATT("markAll", m_aMarkAll);
					ATT("smartHighlight", m_aSmartHL);
					ATT("templateField", m_aTemplateField);
				END_ATTRIBUTES();
			}

		protected:
			genxElement m_eScheme;
			genxElement m_eStyle;
			genxElement m_eStyleClass;
			genxElement m_eKeywords;
			genxElement m_eOvStyles;
			genxElement m_eOvKeywords;
			genxElement m_eOvClasses;
			genxElement m_eColours;
			genxElement m_eOvColours;
			
			genxAttribute m_aKey;
			genxAttribute m_aFont;
			genxAttribute m_aSize;
			genxAttribute m_aFore;
			genxAttribute m_aBack;
			genxAttribute m_aBold;
			genxAttribute m_aItalic;
			genxAttribute m_aUnderline;
			genxAttribute m_aEolFilled;
			genxAttribute m_aClass;
			genxAttribute m_aName;
			genxAttribute m_aSelFore;
			genxAttribute m_aSelBack;
			genxAttribute m_aCaret;
			genxAttribute m_aIndentGuides;
			genxAttribute m_aOverrideTabs;
			genxAttribute m_aUseTabs;
			genxAttribute m_aTabWidth;
			genxAttribute m_aSmartHL;
			genxAttribute m_aMarkAll;
			genxAttribute m_aTemplateField;
	};
}

#endif //#ifndef usersettingswriter_h__included