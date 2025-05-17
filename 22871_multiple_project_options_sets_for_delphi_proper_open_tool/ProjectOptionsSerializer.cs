namespace ProjectOptions
{
    using System.Xml.Serialization;
    using System;
    
    
    /// <summary>Custom serializer for TProjectOptionsSets type.</summary
    public class TProjectOptionsSetsSerializer : System.Xml.Serialization.XmlSerializer
    {
        
        
		TProjectOptionsSetsReader _reader;
		TProjectOptionsSetsWriter _writer;

		/// <summary>Constructs the serializer with default reader and writer instances.</summary>
		public TProjectOptionsSetsSerializer()
		{
		}

		/// <summary>Constructs the serializer with a pre-built reader.</summary>
		public TProjectOptionsSetsSerializer(TProjectOptionsSetsReader reader)
		{
			_reader = reader;
		}

		/// <summary>Constructs the serializer with a pre-writer reader.</summary>
		public TProjectOptionsSetsSerializer(TProjectOptionsSetsWriter writer)
		{
			_writer = writer;
		}

		/// <summary>Constructs the serializer with pre-built reader and writer.</summary>
		public TProjectOptionsSetsSerializer(TProjectOptionsSetsReader reader, TProjectOptionsSetsWriter writer)
		{
			_reader = reader;
			_writer = writer;
		}

		/// <summary>See <see cref="XmlSerializer.CreateReader"/>.</summary>
		protected override XmlSerializationReader CreateReader()
		{
			if (_reader != null) 
				return _reader;
			else
				return new TProjectOptionsSetsReader();
		}

		/// <summary>See <see cref="XmlSerializer.CreateWriter"/>.</summary>
		protected override XmlSerializationWriter CreateWriter()
		{
			if (_writer != null) 
				return _writer;
			else
				return new TProjectOptionsSetsWriter();
		}

		/// <summary>See <see cref="XmlSerializer.Deserialize"/>.</summary>
		protected override object Deserialize(XmlSerializationReader reader)
		{
			if (!(reader is TProjectOptionsSetsReader))
				throw new ArgumentException("reader");

			return ((TProjectOptionsSetsReader)reader).Read();
		}

		/// <summary>See <see cref="XmlSerializer.Serialize"/>.</summary>
		protected override void Serialize(object o, XmlSerializationWriter writer)
		{
			if (!(writer is TProjectOptionsSetsWriter))
				throw new ArgumentException("writer");

			((TProjectOptionsSetsWriter)writer).Write((ProjectOptionsSets.TProjectOptionsSets)o);
		}
        public class Reader : System.Xml.Serialization.XmlSerializationReader {

        /// <remarks/>
        protected virtual ProjectOptionsSets.TProjectOptionsSets Read1_TProjectOptionsSets(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TProjectOptionsSets && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            ProjectOptionsSets.TProjectOptionsSets o = new ProjectOptionsSets.TProjectOptionsSets();
            if ((object)(o.@Item) == null) o.@Item = new System.Collections.ArrayList();
            System.Collections.ArrayList a_1 = (System.Collections.ArrayList)o.@Item;
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id3_LastActivated && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@LastActivated = Reader.ReadElementString();
                        paramsRead[0] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id4_Item && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@Item) == null) o.@Item = new System.Collections.ArrayList();
                            System.Collections.ArrayList a_1_0 = (System.Collections.ArrayList)o.@Item;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id5_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_1_0) == null) Reader.Skip(); else a_1_0.Add(Read3_Object(true, true));
                                        }
                                        else {
                                            UnknownNode(null);
                                        }
                                    }
                                    else {
                                        UnknownNode(null);
                                    }
                                    Reader.MoveToContent();
                                }
                            ReadEndElement();
                            }
                        }
                    }
                    else {
                        UnknownNode((object)o);
                    }
                }
                else {
                    UnknownNode((object)o);
                }
                Reader.MoveToContent();
            }
            ReadEndElement();
            return o;
        }

        /// <remarks/>
        protected virtual Commons.TSettingsObject Read2_TSettingsObject(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id6_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TProjectOptionsSets && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TProjectOptionsSets(isNullable, false);
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            throw CreateAbstractTypeException(@"TSettingsObject", @"");
        }

        /// <remarks/>
        protected virtual System.Object Read3_Object(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null)
                    return ReadTypedPrimitive(new System.Xml.XmlQualifiedName("anyType", "http://www.w3.org/2001/XMLSchema"));
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read5_TProjectOption(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TProjectOptionsSet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read4_TProjectOptionsSet(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id6_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TProjectOptionsSets && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TProjectOptionsSets(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_ArrayOfAnyType && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    System.Collections.ArrayList a = null;
                    if (!ReadNull()) {
                        if ((object)(a) == null) a = new System.Collections.ArrayList();
                        System.Collections.ArrayList z_0_0 = (System.Collections.ArrayList)a;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id5_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        if ((object)(z_0_0) == null) Reader.Skip(); else z_0_0.Add(Read3_Object(true, true));
                                    }
                                    else {
                                        UnknownNode(null);
                                    }
                                }
                                else {
                                    UnknownNode(null);
                                }
                                Reader.MoveToContent();
                            }
                        ReadEndElement();
                        }
                    }
                    return a;
                }
                else
                    return ReadTypedPrimitive((System.Xml.XmlQualifiedName)t);
            }
            System.Object o = new System.Object();
            bool[] paramsRead = new bool[0];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    UnknownNode((object)o);
                }
                else {
                    UnknownNode((object)o);
                }
                Reader.MoveToContent();
            }
            ReadEndElement();
            return o;
        }

        /// <remarks/>
        protected virtual ProjectOptionsSets.TProjectOptionsSet Read4_TProjectOptionsSet(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TProjectOptionsSet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            ProjectOptionsSets.TProjectOptionsSet o = new ProjectOptionsSets.TProjectOptionsSet();
            if ((object)(o.@Values) == null) o.@Values = new System.Collections.ArrayList();
            System.Collections.ArrayList a_1 = (System.Collections.ArrayList)o.@Values;
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id10_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Name = Reader.ReadElementString();
                        paramsRead[0] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id11_Values && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@Values) == null) o.@Values = new System.Collections.ArrayList();
                            System.Collections.ArrayList a_1_0 = (System.Collections.ArrayList)o.@Values;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id5_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_1_0) == null) Reader.Skip(); else a_1_0.Add(Read3_Object(true, true));
                                        }
                                        else {
                                            UnknownNode(null);
                                        }
                                    }
                                    else {
                                        UnknownNode(null);
                                    }
                                    Reader.MoveToContent();
                                }
                            ReadEndElement();
                            }
                        }
                    }
                    else {
                        UnknownNode((object)o);
                    }
                }
                else {
                    UnknownNode((object)o);
                }
                Reader.MoveToContent();
            }
            ReadEndElement();
            return o;
        }

        /// <remarks/>
        protected virtual ProjectOptionsSets.TProjectOption Read5_TProjectOption(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            ProjectOptionsSets.TProjectOption o = new ProjectOptionsSets.TProjectOption();
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id10_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Name = Reader.ReadElementString();
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id12_Value && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Value = Read3_Object(false, true);
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o);
                    }
                }
                else {
                    UnknownNode((object)o);
                }
                Reader.MoveToContent();
            }
            ReadEndElement();
            return o;
        }

        protected override void InitCallbacks() {
        }

        protected object Read7_TProjectOptionsSets() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TProjectOptionsSets && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read1_TProjectOptionsSets(true, true);
                }
                else {
                    throw CreateUnknownNodeException();
                }
            }
            else {
                UnknownNode(null);
            }
            return (object)o;
        }

        System.String id2_Item;
        System.String id9_ArrayOfAnyType;
        System.String id12_Value;
        System.String id11_Values;
        System.String id6_TSettingsObject;
        System.String id8_TProjectOptionsSet;
        System.String id7_TProjectOption;
        System.String id10_Name;
        System.String id4_Item;
        System.String id3_LastActivated;
        System.String id1_TProjectOptionsSets;
        System.String id5_anyType;

        protected override void InitIDs() {
            id2_Item = Reader.NameTable.Add(@"");
            id9_ArrayOfAnyType = Reader.NameTable.Add(@"ArrayOfAnyType");
            id12_Value = Reader.NameTable.Add(@"Value");
            id11_Values = Reader.NameTable.Add(@"Values");
            id6_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
            id8_TProjectOptionsSet = Reader.NameTable.Add(@"TProjectOptionsSet");
            id7_TProjectOption = Reader.NameTable.Add(@"TProjectOption");
            id10_Name = Reader.NameTable.Add(@"Name");
            id4_Item = Reader.NameTable.Add(@"Item");
            id3_LastActivated = Reader.NameTable.Add(@"LastActivated");
            id1_TProjectOptionsSets = Reader.NameTable.Add(@"TProjectOptionsSets");
            id5_anyType = Reader.NameTable.Add(@"anyType");
        }
    }
        public class Writer : System.Xml.Serialization.XmlSerializationWriter {

        void Write1_TProjectOptionsSets(string n, string ns, ProjectOptionsSets.TProjectOptionsSets o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(ProjectOptionsSets.TProjectOptionsSets))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TProjectOptionsSets", @"");
            WriteElementString(@"LastActivated", @"", ((System.String)o.@LastActivated));
            {
                System.Collections.ArrayList a = (System.Collections.ArrayList)((System.Collections.ArrayList)o.@Item);
                if (a != null){
                    WriteStartElement(@"Item", @"");
                    for (int ia = 0; ia < a.Count; ia++) {
                        Write3_Object(@"anyType", @"", ((System.Object)a[ia]), true, false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        void Write2_TSettingsObject(string n, string ns, Commons.TSettingsObject o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Commons.TSettingsObject))
                    ;
                else if (t == typeof(ProjectOptionsSets.TProjectOptionsSets)) {
                    Write1_TProjectOptionsSets(n, ns, (ProjectOptionsSets.TProjectOptionsSets)o, isNullable, true);
                    return;
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
        }

        void Write3_Object(string n, string ns, System.Object o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(System.Object))
                    ;
                else if (t == typeof(ProjectOptionsSets.TProjectOption)) {
                    Write5_TProjectOption(n, ns, (ProjectOptionsSets.TProjectOption)o, isNullable, true);
                    return;
                }
                else if (t == typeof(ProjectOptionsSets.TProjectOptionsSet)) {
                    Write4_TProjectOptionsSet(n, ns, (ProjectOptionsSets.TProjectOptionsSet)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns, (Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(ProjectOptionsSets.TProjectOptionsSets)) {
                    Write1_TProjectOptionsSets(n, ns, (ProjectOptionsSets.TProjectOptionsSets)o, isNullable, true);
                    return;
                }
                else if (t == typeof(System.Collections.ArrayList)) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfAnyType", @"");
                    {
                        System.Collections.ArrayList a = (System.Collections.ArrayList)o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Count; ia++) {
                                Write3_Object(@"anyType", @"", ((System.Object)a[ia]), true, false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else {
                    WriteTypedPrimitive(n, ns, o, true);
                    return;
                }
            }
            WriteStartElement(n, ns, o);
            WriteEndElement(o);
        }

        void Write4_TProjectOptionsSet(string n, string ns, ProjectOptionsSets.TProjectOptionsSet o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(ProjectOptionsSets.TProjectOptionsSet))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TProjectOptionsSet", @"");
            WriteElementString(@"Name", @"", ((System.String)o.@Name));
            {
                System.Collections.ArrayList a = (System.Collections.ArrayList)((System.Collections.ArrayList)o.@Values);
                if (a != null){
                    WriteStartElement(@"Values", @"");
                    for (int ia = 0; ia < a.Count; ia++) {
                        Write3_Object(@"anyType", @"", ((System.Object)a[ia]), true, false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        void Write5_TProjectOption(string n, string ns, ProjectOptionsSets.TProjectOption o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(ProjectOptionsSets.TProjectOption))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TProjectOption", @"");
            WriteElementString(@"Name", @"", ((System.String)o.@Name));
            Write3_Object(@"Value", @"", ((System.Object)o.@Value), false, false);
            WriteEndElement(o);
        }

        protected override void InitCallbacks() {
        }

        protected void Write6_TProjectOptionsSets(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TProjectOptionsSets", @"");
                return;
            }
            TopLevelElement();
            Write1_TProjectOptionsSets(@"TProjectOptionsSets", @"", ((ProjectOptionsSets.TProjectOptionsSets)o), true, false);
        }
    }

    /// <remarks/>
	}
    
    public class TProjectOptionsSetsReader : TProjectOptionsSetsSerializer.Reader
    {
        
        
		/// <remarks/>
		protected override ProjectOptionsSets.TProjectOptionsSets Read1_TProjectOptionsSets(bool isNullable, bool checkType)
		{
			ProjectOptionsSets.TProjectOptionsSets obj = base.Read1_TProjectOptionsSets(isNullable, checkType);
			TProjectOptionsSetsDeserializedHandler handler = TProjectOptionsSetsDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}
        
		/// <remarks/>
		protected override Commons.TSettingsObject Read2_TSettingsObject(bool isNullable, bool checkType)
		{
			Commons.TSettingsObject obj = base.Read2_TSettingsObject(isNullable, checkType);
			TSettingsObjectDeserializedHandler handler = TSettingsObjectDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}
        
		/// <remarks/>
		protected override ProjectOptionsSets.TProjectOptionsSet Read4_TProjectOptionsSet(bool isNullable, bool checkType)
		{
			ProjectOptionsSets.TProjectOptionsSet obj = base.Read4_TProjectOptionsSet(isNullable, checkType);
			TProjectOptionsSetDeserializedHandler handler = TProjectOptionsSetDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}
        
		/// <remarks/>
		protected override ProjectOptionsSets.TProjectOption Read5_TProjectOption(bool isNullable, bool checkType)
		{
			ProjectOptionsSets.TProjectOption obj = base.Read5_TProjectOption(isNullable, checkType);
			TProjectOptionDeserializedHandler handler = TProjectOptionDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}
        
		/// <summary>Reads an object of type ProjectOptionsSets.TProjectOptionsSets.</summary>
		internal ProjectOptionsSets.TProjectOptionsSets Read()
		{
			return (ProjectOptionsSets.TProjectOptionsSets) Read7_TProjectOptionsSets();
		}
        public event TProjectOptionsSetsDeserializedHandler TProjectOptionsSetsDeserialized;
        
        public event TSettingsObjectDeserializedHandler TSettingsObjectDeserialized;
        
        public event TProjectOptionsSetDeserializedHandler TProjectOptionsSetDeserialized;
        
        public event TProjectOptionDeserializedHandler TProjectOptionDeserialized;
    }
    
    public delegate void TProjectOptionsSetsDeserializedHandler(ProjectOptionsSets.TProjectOptionsSets tprojectoptionssets);
    
    public delegate void TSettingsObjectDeserializedHandler(Commons.TSettingsObject tsettingsobject);
    
    public delegate void TProjectOptionsSetDeserializedHandler(ProjectOptionsSets.TProjectOptionsSet tprojectoptionsset);
    
    public delegate void TProjectOptionDeserializedHandler(ProjectOptionsSets.TProjectOption tprojectoption);
    
    public class TProjectOptionsSetsWriter : TProjectOptionsSetsSerializer.Writer
    {
        
        
		/// <summary>Writes an object of type ProjectOptionsSets.TProjectOptionsSets.</summary>
		internal void Write(ProjectOptionsSets.TProjectOptionsSets obj)
		{
			Write6_TProjectOptionsSets(obj);
		}}
}
