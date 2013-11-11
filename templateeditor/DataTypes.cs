using System;
using System.Collections;
using System.Xml.Serialization;
using System.ComponentModel;

namespace ProjectTemplateEditor
{
	public enum NodeType
	{
		Set,
		Group,
		Category,
		Value
	};

	public enum SetType
	{ 
		[XmlEnum("project")]
		Project, 
		[XmlEnum("folder")]
		Folder, 
		[XmlEnum("file")]
		File
	};

	public enum ValueType
	{
		FolderPath,
		FilePath,
		Text,
		Boolean,
		Integer,
		OptionList
	};

	public interface IDescribed
	{
		string Description
		{
			get;
		}
	}

	public class OptionBase : IDescribed
	{
		string _name = "setme";
		string _desc = "New Option";
		protected string _default = "";
		int _helpid = 0;

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlAttribute("description")]
		public string Description
		{
			get { return _desc; }
			set { _desc = value; }
		}

		[XmlAttribute("default"), Browsable(false)]
		public string DefaultValue
		{
			get { return _default; }
			set { _default = value; }
		}

		[XmlAttribute("helpid")]
		public int HelpID
		{
			get { return _helpid; }
			set { _helpid = value; }
		}
	}

	[XmlRoot("folderPath")]
	public class FolderPath : OptionBase
	{
	}

	[XmlRoot("filePath")]
	public class FilePath : OptionBase
	{
	}

	[XmlRoot("value")]
	public class OptionListValue
	{
		string _desc = "";
		string _value = "";

		[XmlAttribute("description")]
		public string Description
		{
			get { return _desc; }
			set { _desc = value; }
		}

		[XmlAttribute("value")]
		public string Value
		{
			get { return _value; }
			set { _value = value; }
		}

		public override string ToString()
		{
			return Value + " : " + Description;
		}

		public override int GetHashCode()
		{
			return ToString().GetHashCode();
		}

	}

	public class OptionListDesigner : System.ComponentModel.Design.CollectionEditor
	{
		public OptionListDesigner() : base( typeof(ArrayList) )
		{
		}

		protected override Type CreateCollectionItemType()
		{
			return typeof(OptionListValue);
		}

		protected override System.ComponentModel.Design.CollectionEditor.CollectionForm CreateCollectionForm()
		{
			System.ComponentModel.Design.CollectionEditor.CollectionForm form = base.CreateCollectionForm();
			form.Text = "Option List Editor";

			// Make most of the buttons a bit nicer!
			foreach(System.Windows.Forms.Control c in form.Controls)
			{
				System.Windows.Forms.Button b = c as System.Windows.Forms.Button;
				if(b != null)
					if(b.Image == null)
						b.FlatStyle = System.Windows.Forms.FlatStyle.System;
			}

			return form;
		}


	};

	[XmlRoot("optionlist")]
	public class OptionList : OptionBase
	{
		ArrayList _values = new ArrayList();

		[XmlIgnore]
		public string Default
		{
			get { return DefaultValue; }
			set { DefaultValue = value; }
		}

		[XmlElement("value", typeof(OptionListValue)),
		Editor(typeof(OptionListDesigner), typeof(System.Drawing.Design.UITypeEditor))]
		public ArrayList Values
		{
			get { return _values; }
			set { _values = value; }
		}
	}

	[XmlRoot("option")]
	public class Option : OptionBase
	{
		[XmlIgnore]
		public bool Default
		{
			get { if(_default != string.Empty) return System.Xml.XmlConvert.ToBoolean(_default); return false; }
			set { _default = System.Xml.XmlConvert.ToString(value); }
		}
	}

	[XmlRoot("text")]
	public class Text : OptionBase
	{
		[XmlIgnore]
		public string Default
		{
			get { return _default; }
			set { _default = value; }
		}
	}

	[XmlRoot("int")]
	public class IntOption : OptionBase
	{
		[XmlIgnore]
		public int Default
		{
			get { if(_default != string.Empty) return System.Xml.XmlConvert.ToInt32(_default); return 0; }
			set { _default = System.Xml.XmlConvert.ToString(value); }
		}
	}

	public class OptionFactory
	{
		public static OptionBase CreateOption(ValueType vt)
		{
			switch(vt)
			{
				case ValueType.Integer:
					return new IntOption();
				case ValueType.Boolean:
					return new Option();
				case ValueType.FilePath:
					return new FilePath();
				case ValueType.FolderPath:
					return new FolderPath();
				case ValueType.OptionList:
					return new OptionList();
				case ValueType.Text:
					return new Text();
				default:
					throw new ApplicationException("Unexpected ValueType value");
			}
		}

		private OptionFactory()
		{
		}
	}

	[XmlRoot("category")]
	public class Category : IDescribed
	{
		string _name;
		string _desc;
		ArrayList _options = new ArrayList();

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlAttribute("description")]
		public string Description
		{
			get { return _desc; }
			set { _desc = value; }
		}

		[Browsable(false),
		XmlElement("option", typeof(Option)),
		XmlElement("optionlist", typeof(OptionList)),
		XmlElement("int", typeof(IntOption)),
		XmlElement("text", typeof(Text)),
		XmlElement("filePath", typeof(FilePath)),
		XmlElement("folderPath", typeof(FolderPath))]
		public ArrayList Options
		{
			get { return _options; }
			set { _options = value; }
		}
	}

	[XmlRoot("group")]
	public class Group : IDescribed
	{
		string _name;
		string _desc;
		ArrayList _cats = new ArrayList();

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlAttribute("description")]
		public string Description
		{
			get { return _desc; }
			set { _desc = value; }
		}

		[Browsable(false),
		XmlElement("category", typeof(Category))]
		public ArrayList Categories
		{
			get { return _cats; }
			set { _cats = value; }
		}
	}



	[XmlRoot("set")]
	public class Set
	{
		private SetType _type;
		ArrayList _groups = new ArrayList();

		[XmlAttribute("type"), Browsable(false)]
		public SetType Type
		{
			set { _type = value; }
			get { return _type; }
		}

		[Browsable(false),
		XmlElement("group", typeof(Group))]
		public ArrayList Groups
		{
			get { return _groups; }
			set { _groups = value; }
		}

		public Set()
		{
		}

		public Set(SetType type)
		{
			_type = type;
		}
	}

	[XmlRoot("projectConfig")]
	public class ProjectConfig
	{
		string _name;
		string _helpfile;
		string _id;
		string _ns;
		string _icon;
		ArrayList _sets = new ArrayList();

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlAttribute("helpfile")]
		public string HelpFile
		{
			get { return _helpfile; }
			set { _helpfile = value; }
		}

		[XmlAttribute("id")]
		public string GUID
		{
			get { return _id; }
			set { _id = value; }
		}

		[XmlAttribute("icon")]
		public string Icon
		{
			get { return _icon; }
			set { _icon = value; }
		}

		[XmlAttribute("ns")]
		public string XmlNameSpace
		{
			get { return _ns; }
			set { _ns = value; }
		}

		[Browsable(false),
		XmlElement("set", typeof(Set))]
		public ArrayList Sets
		{
			get { return _sets; }
			set { _sets = value; }
		}
	}

	public class FileLoader
	{
		public ProjectConfig Load(string filename)
		{
			XmlSerializer xs = new XmlSerializer(typeof(ProjectConfig));
			// TODO: Exceptions...
			System.Xml.XmlTextReader xtr = new System.Xml.XmlTextReader(filename);
			ProjectConfig pc = (ProjectConfig)xs.Deserialize(xtr);
			xtr.Close();
			return pc;
		}

		public void Save(string filename, ProjectConfig projConf)
		{
			System.Xml.XmlTextWriter xtw = new System.Xml.XmlTextWriter(filename, System.Text.Encoding.UTF8);
			xtw.Formatting = System.Xml.Formatting.Indented;
			xtw.Indentation = 1;
			xtw.IndentChar = '\t';
			XmlSerializerNamespaces xsn = new XmlSerializerNamespaces();
			xsn.Add("", "");
			XmlSerializer xs = new XmlSerializer(typeof(ProjectConfig));
			xs.Serialize(xtw, projConf, xsn);
			xtw.Close();
		}
	}
}
