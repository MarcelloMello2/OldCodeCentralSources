namespace Options
{
    using System.Xml.Serialization;
    using System;
    
    
    /// <summary>Custom serializer for TOptions type.</summary
    public class TOptionsSerializer : System.Xml.Serialization.XmlSerializer
    {
        
        
		TOptionsReader _reader;
		TOptionsWriter _writer;

		/// <summary>Constructs the serializer with default reader and writer instances.</summary>
		public TOptionsSerializer()
		{
		}

		/// <summary>Constructs the serializer with a pre-built reader.</summary>
		public TOptionsSerializer(TOptionsReader reader)
		{
			_reader = reader;
		}

		/// <summary>Constructs the serializer with a pre-writer reader.</summary>
		public TOptionsSerializer(TOptionsWriter writer)
		{
			_writer = writer;
		}

		/// <summary>Constructs the serializer with pre-built reader and writer.</summary>
		public TOptionsSerializer(TOptionsReader reader, TOptionsWriter writer)
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
				return new TOptionsReader();
		}

		/// <summary>See <see cref="XmlSerializer.CreateWriter"/>.</summary>
		protected override XmlSerializationWriter CreateWriter()
		{
			if (_writer != null) 
				return _writer;
			else
				return new TOptionsWriter();
		}

		/// <summary>See <see cref="XmlSerializer.Deserialize"/>.</summary>
		protected override object Deserialize(XmlSerializationReader reader)
		{
			if (!(reader is TOptionsReader))
				throw new ArgumentException("reader");

			return ((TOptionsReader)reader).Read();
		}

		/// <summary>See <see cref="XmlSerializer.Serialize"/>.</summary>
		protected override void Serialize(object o, XmlSerializationWriter writer)
		{
			if (!(writer is TOptionsWriter))
				throw new ArgumentException("writer");

			((TOptionsWriter)writer).Write((Options.TOptions)o);
		}
        public class Reader : System.Xml.Serialization.XmlSerializationReader {

        /// <remarks/>
        protected virtual Options.TOptions Read1_TOptions(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            Options.TOptions o = new Options.TOptions();
            if ((object)(o.@IgnoreOptions) == null) o.@IgnoreOptions = new System.Collections.ArrayList();
            System.Collections.ArrayList a_0 = (System.Collections.ArrayList)o.@IgnoreOptions;
            if ((object)(o.@ReleaseValues) == null) o.@ReleaseValues = new System.Collections.ArrayList();
            System.Collections.ArrayList a_2 = (System.Collections.ArrayList)o.@ReleaseValues;
            bool[] paramsRead = new bool[3];
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
                    if (((object) Reader.LocalName == (object)id3_IgnoreOptions && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@IgnoreOptions) == null) o.@IgnoreOptions = new System.Collections.ArrayList();
                            System.Collections.ArrayList a_0_0 = (System.Collections.ArrayList)o.@IgnoreOptions;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_0_0) == null) Reader.Skip(); else a_0_0.Add(Read3_Object(true, true));
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
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id5_TrayAreaInjector && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@TrayAreaInjector = System.Xml.XmlConvert.ToBoolean(Reader.ReadElementString());
                        paramsRead[1] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id6_ReleaseValues && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@ReleaseValues) == null) o.@ReleaseValues = new System.Collections.ArrayList();
                            System.Collections.ArrayList a_2_0 = (System.Collections.ArrayList)o.@ReleaseValues;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_2_0) == null) Reader.Skip(); else a_2_0.Add(Read3_Object(true, true));
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
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TOptions(isNullable, false);
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
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read5_TProjectOption(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_TReleaseProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read4_TReleaseProjectOption(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TOptions(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id10_ArrayOfAnyType && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
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
                                    if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
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
        protected virtual Options.TReleaseProjectOption Read4_TReleaseProjectOption(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_TReleaseProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            Options.TReleaseProjectOption o = new Options.TReleaseProjectOption();
            if ((object)(o.@Conditions) == null) o.@Conditions = new System.Collections.ArrayList();
            System.Collections.ArrayList a_2 = (System.Collections.ArrayList)o.@Conditions;
            bool[] paramsRead = new bool[3];
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id11_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Name = Reader.ReadElementString();
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id12_Value && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Value = Read3_Object(false, true);
                        paramsRead[1] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id13_Conditions && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@Conditions) == null) o.@Conditions = new System.Collections.ArrayList();
                            System.Collections.ArrayList a_2_0 = (System.Collections.ArrayList)o.@Conditions;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_2_0) == null) Reader.Skip(); else a_2_0.Add(Read3_Object(true, true));
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
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TProjectOption && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id11_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
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

        protected object Read7_TOptions() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TOptions && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read1_TOptions(true, true);
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
        System.String id10_ArrayOfAnyType;
        System.String id6_ReleaseValues;
        System.String id12_Value;
        System.String id5_TrayAreaInjector;
        System.String id11_Name;
        System.String id1_TOptions;
        System.String id13_Conditions;
        System.String id8_TProjectOption;
        System.String id7_TSettingsObject;
        System.String id9_TReleaseProjectOption;
        System.String id3_IgnoreOptions;
        System.String id4_anyType;

        protected override void InitIDs() {
            id2_Item = Reader.NameTable.Add(@"");
            id10_ArrayOfAnyType = Reader.NameTable.Add(@"ArrayOfAnyType");
            id6_ReleaseValues = Reader.NameTable.Add(@"ReleaseValues");
            id12_Value = Reader.NameTable.Add(@"Value");
            id5_TrayAreaInjector = Reader.NameTable.Add(@"TrayAreaInjector");
            id11_Name = Reader.NameTable.Add(@"Name");
            id1_TOptions = Reader.NameTable.Add(@"TOptions");
            id13_Conditions = Reader.NameTable.Add(@"Conditions");
            id8_TProjectOption = Reader.NameTable.Add(@"TProjectOption");
            id7_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
            id9_TReleaseProjectOption = Reader.NameTable.Add(@"TReleaseProjectOption");
            id3_IgnoreOptions = Reader.NameTable.Add(@"IgnoreOptions");
            id4_anyType = Reader.NameTable.Add(@"anyType");
        }
    }
        public class Writer : System.Xml.Serialization.XmlSerializationWriter {

        void Write1_TOptions(string n, string ns, Options.TOptions o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Options.TOptions))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TOptions", @"");
            {
                System.Collections.ArrayList a = (System.Collections.ArrayList)((System.Collections.ArrayList)o.@IgnoreOptions);
                if (a != null){
                    WriteStartElement(@"IgnoreOptions", @"");
                    for (int ia = 0; ia < a.Count; ia++) {
                        Write3_Object(@"anyType", @"", ((System.Object)a[ia]), true, false);
                    }
                    WriteEndElement();
                }
            }
            WriteElementStringRaw(@"TrayAreaInjector", @"", System.Xml.XmlConvert.ToString((System.Boolean)((System.Boolean)o.@TrayAreaInjector)));
            {
                System.Collections.ArrayList a = (System.Collections.ArrayList)((System.Collections.ArrayList)o.@ReleaseValues);
                if (a != null){
                    WriteStartElement(@"ReleaseValues", @"");
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
                else if (t == typeof(Options.TOptions)) {
                    Write1_TOptions(n, ns, (Options.TOptions)o, isNullable, true);
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
                else if (t == typeof(Options.TReleaseProjectOption)) {
                    Write4_TReleaseProjectOption(n, ns, (Options.TReleaseProjectOption)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns, (Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Options.TOptions)) {
                    Write1_TOptions(n, ns, (Options.TOptions)o, isNullable, true);
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

        void Write4_TReleaseProjectOption(string n, string ns, Options.TReleaseProjectOption o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Options.TReleaseProjectOption))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TReleaseProjectOption", @"");
            WriteElementString(@"Name", @"", ((System.String)o.@Name));
            Write3_Object(@"Value", @"", ((System.Object)o.@Value), false, false);
            {
                System.Collections.ArrayList a = (System.Collections.ArrayList)((System.Collections.ArrayList)o.@Conditions);
                if (a != null){
                    WriteStartElement(@"Conditions", @"");
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

        protected void Write6_TOptions(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TOptions", @"");
                return;
            }
            TopLevelElement();
            Write1_TOptions(@"TOptions", @"", ((Options.TOptions)o), true, false);
        }
    }

    /// <remarks/>
	}
    
    public class TOptionsReader : TOptionsSerializer.Reader
    {
        
        
		/// <remarks/>
		protected override Options.TOptions Read1_TOptions(bool isNullable, bool checkType)
		{
			Options.TOptions obj = base.Read1_TOptions(isNullable, checkType);
			TOptionsDeserializedHandler handler = TOptionsDeserialized;
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
		protected override Options.TReleaseProjectOption Read4_TReleaseProjectOption(bool isNullable, bool checkType)
		{
			Options.TReleaseProjectOption obj = base.Read4_TReleaseProjectOption(isNullable, checkType);
			TReleaseProjectOptionDeserializedHandler handler = TReleaseProjectOptionDeserialized;
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
        
		/// <summary>Reads an object of type Options.TOptions.</summary>
		internal Options.TOptions Read()
		{
			return (Options.TOptions) Read7_TOptions();
		}
        public event TOptionsDeserializedHandler TOptionsDeserialized;
        
        public event TSettingsObjectDeserializedHandler TSettingsObjectDeserialized;
        
        public event TReleaseProjectOptionDeserializedHandler TReleaseProjectOptionDeserialized;
        
        public event TProjectOptionDeserializedHandler TProjectOptionDeserialized;
    }
    
    public delegate void TOptionsDeserializedHandler(Options.TOptions toptions);
    
    public delegate void TSettingsObjectDeserializedHandler(Commons.TSettingsObject tsettingsobject);
    
    public delegate void TReleaseProjectOptionDeserializedHandler(Options.TReleaseProjectOption treleaseprojectoption);
    
    public delegate void TProjectOptionDeserializedHandler(ProjectOptionsSets.TProjectOption tprojectoption);
    
    public class TOptionsWriter : TOptionsSerializer.Writer
    {
        
        
		/// <summary>Writes an object of type Options.TOptions.</summary>
		internal void Write(Options.TOptions obj)
		{
			Write6_TOptions(obj);
		}}
}
