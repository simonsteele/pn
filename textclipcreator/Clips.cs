using System;
using System.Collections;
using System.Xml.Serialization;

namespace TextClipCreator
{
	/// <summary>
	/// Collection of Text Clips
	/// </summary>
	[XmlRoot("clips")]
	public class Clips
	{
		string _name = "";
		ArrayList _clips = new ArrayList();
		bool _modified = false;

		public Clips()
		{
		}

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlElement("clip", Type=typeof(Clip))]
		public ArrayList TextClips
		{
			get { return _clips; }
			set { _clips = value; }
		}

		public bool Modified
		{
			get { return _modified; }
			set { _modified = value; }
		}

	}

	/// <summary>
	/// One Text Clip
	/// </summary>
	public class Clip
	{
		string _name = "";
		string _content = "";

		public Clip()
		{
		}

		public Clip(string name)
		{
			_name = name;
		}

		[XmlAttribute("name")]
		public string Name
		{
			get { return _name; }
			set { _name = value; }
		}

		[XmlText]
		public string Content
		{
			get { return _content; }
			set { _content = value; }
		}

		public override string ToString()
		{
			return _name;
		}
	}

	/// <summary>
	/// Load and Save TextClip files
	/// </summary>
	public class ClipStorage
	{
		static XmlSerializer s_ser = null;
		System.Xml.Serialization.XmlSerializerNamespaces s_xsn = null;
		
		public ClipStorage()
		{
			if(s_ser == null)
			{
				s_ser = new XmlSerializer(typeof(Clips));
				s_xsn = new XmlSerializerNamespaces();
				s_xsn.Add("", "");
			}
		}

		public Clips Load(string filename)
		{
			System.Xml.XmlTextReader xtw = new System.Xml.XmlTextReader(filename);
			try
			{
				Clips clips = (Clips)s_ser.Deserialize(xtw);
				return clips;
			}
			finally
			{
				xtw.Close();
			}
		}

		public void Save(Clips clips, string filename)
		{
			System.Xml.XmlTextWriter xtw = new System.Xml.XmlTextWriter(filename, System.Text.Encoding.UTF8);
			try
			{
				xtw.Formatting = System.Xml.Formatting.Indented;
				s_ser.Serialize(xtw, clips, s_xsn);
			}
			finally
			{
				xtw.Close();
			}
		}
	}
}
