using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Xml.Linq;
using System.Text.RegularExpressions;
using System.Xml.Serialization;
using System.Xml;

namespace ApiConvert
{
    public class Param
    {
        [XmlAttribute("name")]
        public string Name { get; set; }
    }

    public class Overload
    {
        public Overload()
        {
            this.Params = new List<Param>();
        }

        [XmlAttribute("retVal")]
        public string ReturnValue { get; set; }

        [XmlElement("Param")]
        public List<Param> Params { get; set; }
    }

    public class KeyWord
    {
        private bool func;

        public KeyWord()
        {
            this.Overloads = new List<Overload>();
        }

        [XmlAttribute("name")]
        public string Name { get; set; }

        [XmlIgnore]
        public bool Func 
        {
            get { return this.func; }
            set { this.func = value; this.FuncStrSpecified = this.func; }
        }

        [XmlAttribute("func")]
        public string FuncStr
        {
            get { return this.Func ? "yes" : "no"; }
            set { this.Func = value == "yes" ? true : false; }
        }

        [XmlIgnore]
        public bool FuncStrSpecified { get; set; }

        [XmlElement("Overload")]
        public List<Overload> Overloads { get; set; }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string[] api = File.ReadAllLines(args[0]);

            Dictionary<string, KeyWord> groupedEntries = new Dictionary<string, KeyWord>();

            foreach (string apiEntry in api)
            {
                if (!apiEntry.Contains("("))
                {
                    // Define, ignore until later
                }
                else
                {
                    Regex r = new Regex(@"([^\(]+)\(([^\)]+)\) ([^\(]+)\(([a-zA-Z0-9_-]+)\)");
                    Match m = r.Match(apiEntry);
                    if (m.Success)
                    {
                        // Console.WriteLine("Name: {0}, Params: {1}, Desc: {2}, Ret: {3}", m.Groups[1], m.Groups[2], m.Groups[3], m.Groups[4]);
                        string name = m.Groups[1].Value;
                        string[] paramstr = m.Groups[2].Value.Split(',').Select(s => s.Trim()).ToArray();

                        for (int i = 0; i < paramstr.Length; i++)
                        {
                            if (paramstr[i].EndsWith("["))
                            {
                                paramstr[i] = paramstr[i].Substring(0, paramstr[i].Length - 1).Trim();
                                paramstr[i + 1] = "[" + paramstr[i + 1];
                            }
                        }

                        string desc = m.Groups[3].Value;
                        string ret = m.Groups[4].Value;

                        if (name.Contains("->"))
                        {
                            // Class method, ignore until later
                        }

                        KeyWord kw = GetEntry(groupedEntries, name);
                        kw.Func = true;
                        kw.Overloads.Add(new Overload
                            {
                                ReturnValue = ret,
                                Params = paramstr.Select(p => new Param { Name = p }).ToList()
                            });
                    }
                }
            }

            using (var writer = XmlWriter.Create(args[1], new XmlWriterSettings { Encoding = Encoding.UTF8, Indent = true }))
            {
                writer.WriteStartDocument();
                writer.WriteStartElement("AutoComplete");
                XmlSerializer ser = new XmlSerializer(typeof(KeyWord));
                XmlSerializerNamespaces ns = new XmlSerializerNamespaces();
                ns.Add("", "");
                foreach (var kw in groupedEntries.Values.OrderBy(k => k.Name))
                {
                    ser.Serialize(writer, kw, ns);
                }
                writer.WriteEndDocument();
            }
        }

        private static KeyWord GetEntry(Dictionary<string, KeyWord> all, string apiKey)
        {
            KeyWord ret;
            if (!all.TryGetValue(apiKey, out ret))
            {
                ret = new KeyWord { Name = apiKey };
                all[apiKey] = ret;
            }
            
            return ret;
        }
    }
}
