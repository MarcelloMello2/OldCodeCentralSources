namespace ApplicationSts
{
    using System.Xml.Serialization;
    using System;


    /// <summary>Custom serializer for TApplicationSts type.</summary
    public class TApplicationStsSerializer : System.Xml.Serialization.XmlSerializer
    {


		TApplicationStsReader _reader;
		TApplicationStsWriter _writer;

		/// <summary>Constructs the serializer with default reader and writer instances.</summary>
		public TApplicationStsSerializer()
		{
		}

		/// <summary>Constructs the serializer with a pre-built reader.</summary>
		public TApplicationStsSerializer(TApplicationStsReader reader)
		{
			_reader = reader;
		}

		/// <summary>Constructs the serializer with a pre-writer reader.</summary>
		public TApplicationStsSerializer(TApplicationStsWriter writer)
		{
			_writer = writer;
		}

		/// <summary>Constructs the serializer with pre-built reader and writer.</summary>
		public TApplicationStsSerializer(TApplicationStsReader reader, TApplicationStsWriter writer)
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
				return new TApplicationStsReader();
		}

		/// <summary>See <see cref="XmlSerializer.CreateWriter"/>.</summary>
		protected override XmlSerializationWriter CreateWriter()
		{
			if (_writer != null)
				return _writer;
			else
				return new TApplicationStsWriter();
		}

		/// <summary>See <see cref="XmlSerializer.Deserialize"/>.</summary>
		protected override object Deserialize(XmlSerializationReader reader)
		{
			if (!(reader is TApplicationStsReader))
				throw new ArgumentException("reader");

			return ((TApplicationStsReader)reader).Read();
		}

		/// <summary>See <see cref="XmlSerializer.Serialize"/>.</summary>
		protected override void Serialize(object o, XmlSerializationWriter writer)
		{
			if (!(writer is TApplicationStsWriter))
				throw new ArgumentException("writer");

			((TApplicationStsWriter)writer).Write((Options.TApplicationSts)o);
		}
        public class Reader : System.Xml.Serialization.XmlSerializationReader {

        /// <remarks/>
        protected virtual Options.TApplicationSts Read1_TApplicationSts(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TApplicationSts && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            Options.TApplicationSts o = new Options.TApplicationSts();
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id3_OpenDialog && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@OpenDialog = Read4_TOpenDialog(false, true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id4_AddNewDialog && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@AddNewDialog = Read7_TAddNewDialog(false, true);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id5_SearchPath && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@SearchPath = Reader.ReadElementString();
                        paramsRead[2] = true;
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
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TApplicationSts && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TApplicationSts(isNullable, false);
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
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TAddNewDialog && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read7_TAddNewDialog(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TOpenDialog && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read4_TOpenDialog(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_Point && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read6_Point(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id10_Size && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read5_Size(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id6_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TApplicationSts && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TApplicationSts(isNullable, false);
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
        protected virtual Options.TOpenDialog Read4_TOpenDialog(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TOpenDialog && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            Options.TOpenDialog o = new Options.TOpenDialog();
            bool[] paramsRead = new bool[4];
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id10_Size && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Size = Read5_Size(true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id11_Location && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Location = Read6_Point(true);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id12_ColumnFileWidth && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@ColumnFileWidth = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        paramsRead[2] = true;
                    }
                    else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id13_Sorted && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Sorted = System.Xml.XmlConvert.ToBoolean(Reader.ReadElementString());
                        paramsRead[3] = true;
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
        protected virtual System.Drawing.Size Read5_Size(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id10_Size && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            System.Drawing.Size o;
            try {
                o = (System.Drawing.Size)System.Activator.CreateInstance(typeof(System.Drawing.Size));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"Size");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"Size");
            }
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id14_Width && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Width = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id15_Height && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Height = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
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

        /// <remarks/>
        protected virtual System.Drawing.Point Read6_Point(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_Point && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            System.Drawing.Point o;
            try {
                o = (System.Drawing.Point)System.Activator.CreateInstance(typeof(System.Drawing.Point));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"Point");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"Point");
            }
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id16_X && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@X = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id17_Y && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Y = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
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

        /// <remarks/>
        protected virtual Options.TAddNewDialog Read7_TAddNewDialog(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TAddNewDialog && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            Options.TAddNewDialog o = new Options.TAddNewDialog();
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id10_Size && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Size = Read5_Size(true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id11_Location && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Location = Read6_Point(true);
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

        protected object Read9_TApplicationSts() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TApplicationSts && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read1_TApplicationSts(true, true);
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

        System.String id14_Width;
        System.String id7_TAddNewDialog;
        System.String id8_TOpenDialog;
        System.String id17_Y;
        System.String id6_TSettingsObject;
        System.String id12_ColumnFileWidth;
        System.String id2_Item;
        System.String id3_OpenDialog;
        System.String id10_Size;
        System.String id13_Sorted;
        System.String id11_Location;
        System.String id9_Point;
        System.String id15_Height;
        System.String id1_TApplicationSts;
        System.String id4_AddNewDialog;
        System.String id16_X;
        System.String id5_SearchPath;

        protected override void InitIDs() {
            id14_Width = Reader.NameTable.Add(@"Width");
            id7_TAddNewDialog = Reader.NameTable.Add(@"TAddNewDialog");
            id8_TOpenDialog = Reader.NameTable.Add(@"TOpenDialog");
            id17_Y = Reader.NameTable.Add(@"Y");
            id6_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
            id12_ColumnFileWidth = Reader.NameTable.Add(@"ColumnFileWidth");
            id2_Item = Reader.NameTable.Add(@"");
            id3_OpenDialog = Reader.NameTable.Add(@"OpenDialog");
            id10_Size = Reader.NameTable.Add(@"Size");
            id13_Sorted = Reader.NameTable.Add(@"Sorted");
            id11_Location = Reader.NameTable.Add(@"Location");
            id9_Point = Reader.NameTable.Add(@"Point");
            id15_Height = Reader.NameTable.Add(@"Height");
            id1_TApplicationSts = Reader.NameTable.Add(@"TApplicationSts");
            id4_AddNewDialog = Reader.NameTable.Add(@"AddNewDialog");
            id16_X = Reader.NameTable.Add(@"X");
            id5_SearchPath = Reader.NameTable.Add(@"SearchPath");
        }
    }
        public class Writer : System.Xml.Serialization.XmlSerializationWriter {

        void Write1_TApplicationSts(string n, string ns, Options.TApplicationSts o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Options.TApplicationSts))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TApplicationSts", @"");
            Write4_TOpenDialog(@"OpenDialog", @"", ((Options.TOpenDialog)o.@OpenDialog), false, false);
            Write7_TAddNewDialog(@"AddNewDialog", @"", ((Options.TAddNewDialog)o.@AddNewDialog), false, false);
            WriteElementString(@"SearchPath", @"", ((System.String)o.@SearchPath));
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
                else if (t == typeof(Options.TApplicationSts)) {
                    Write1_TApplicationSts(n, ns, (Options.TApplicationSts)o, isNullable, true);
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
                else if (t == typeof(Options.TAddNewDialog)) {
                    Write7_TAddNewDialog(n, ns, (Options.TAddNewDialog)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Options.TOpenDialog)) {
                    Write4_TOpenDialog(n, ns, (Options.TOpenDialog)o, isNullable, true);
                    return;
                }
                else if (t == typeof(System.Drawing.Point)) {
                    Write6_Point(n, ns, (System.Drawing.Point)o, true);
                    return;
                }
                else if (t == typeof(System.Drawing.Size)) {
                    Write5_Size(n, ns, (System.Drawing.Size)o, true);
                    return;
                }
                else if (t == typeof(Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns, (Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Options.TApplicationSts)) {
                    Write1_TApplicationSts(n, ns, (Options.TApplicationSts)o, isNullable, true);
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

        void Write4_TOpenDialog(string n, string ns, Options.TOpenDialog o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Options.TOpenDialog))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TOpenDialog", @"");
            Write5_Size(@"Size", @"", ((System.Drawing.Size)o.@Size), false);
            Write6_Point(@"Location", @"", ((System.Drawing.Point)o.@Location), false);
            WriteElementStringRaw(@"ColumnFileWidth", @"", System.Xml.XmlConvert.ToString((System.Int32)((System.Int32)o.@ColumnFileWidth)));
            WriteElementStringRaw(@"Sorted", @"", System.Xml.XmlConvert.ToString((System.Boolean)((System.Boolean)o.@Sorted)));
            WriteEndElement(o);
        }

        void Write5_Size(string n, string ns, System.Drawing.Size o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(System.Drawing.Size))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"Size", @"");
            WriteElementStringRaw(@"Width", @"", System.Xml.XmlConvert.ToString((System.Int32)((System.Int32)o.@Width)));
            WriteElementStringRaw(@"Height", @"", System.Xml.XmlConvert.ToString((System.Int32)((System.Int32)o.@Height)));
            WriteEndElement(o);
        }

        void Write6_Point(string n, string ns, System.Drawing.Point o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(System.Drawing.Point))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"Point", @"");
            WriteElementStringRaw(@"X", @"", System.Xml.XmlConvert.ToString((System.Int32)((System.Int32)o.@X)));
            WriteElementStringRaw(@"Y", @"", System.Xml.XmlConvert.ToString((System.Int32)((System.Int32)o.@Y)));
            WriteEndElement(o);
        }

        void Write7_TAddNewDialog(string n, string ns, Options.TAddNewDialog o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(Options.TAddNewDialog))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TAddNewDialog", @"");
            Write5_Size(@"Size", @"", ((System.Drawing.Size)o.@Size), false);
            Write6_Point(@"Location", @"", ((System.Drawing.Point)o.@Location), false);
            WriteEndElement(o);
        }

        protected override void InitCallbacks() {
        }

        protected void Write8_TApplicationSts(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TApplicationSts", @"");
                return;
            }
            TopLevelElement();
            Write1_TApplicationSts(@"TApplicationSts", @"", ((Options.TApplicationSts)o), true, false);
        }
    }

    /// <remarks/>
	}

    public class TApplicationStsReader : TApplicationStsSerializer.Reader
    {


		/// <remarks/>
		protected override Options.TApplicationSts Read1_TApplicationSts(bool isNullable, bool checkType)
		{
			Options.TApplicationSts obj = base.Read1_TApplicationSts(isNullable, checkType);
			TApplicationStsDeserializedHandler handler = TApplicationStsDeserialized;
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
		protected override Options.TOpenDialog Read4_TOpenDialog(bool isNullable, bool checkType)
		{
			Options.TOpenDialog obj = base.Read4_TOpenDialog(isNullable, checkType);
			TOpenDialogDeserializedHandler handler = TOpenDialogDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override System.Drawing.Size Read5_Size(bool checkType)
		{
			System.Drawing.Size obj = base.Read5_Size(checkType);
			SizeDeserializedHandler handler = SizeDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override System.Drawing.Point Read6_Point(bool checkType)
		{
			System.Drawing.Point obj = base.Read6_Point(checkType);
			PointDeserializedHandler handler = PointDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override Options.TAddNewDialog Read7_TAddNewDialog(bool isNullable, bool checkType)
		{
			Options.TAddNewDialog obj = base.Read7_TAddNewDialog(isNullable, checkType);
			TAddNewDialogDeserializedHandler handler = TAddNewDialogDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <summary>Reads an object of type Options.TApplicationSts.</summary>
		internal Options.TApplicationSts Read()
		{
			return (Options.TApplicationSts) Read9_TApplicationSts();
		}
        public event TApplicationStsDeserializedHandler TApplicationStsDeserialized;

        public event TSettingsObjectDeserializedHandler TSettingsObjectDeserialized;

        public event TOpenDialogDeserializedHandler TOpenDialogDeserialized;

        public event SizeDeserializedHandler SizeDeserialized;

        public event PointDeserializedHandler PointDeserialized;

        public event TAddNewDialogDeserializedHandler TAddNewDialogDeserialized;
    }

    public delegate void TApplicationStsDeserializedHandler(Options.TApplicationSts tapplicationsts);

    public delegate void TSettingsObjectDeserializedHandler(Commons.TSettingsObject tsettingsobject);

    public delegate void TOpenDialogDeserializedHandler(Options.TOpenDialog topendialog);

    public delegate void SizeDeserializedHandler(System.Drawing.Size size);

    public delegate void PointDeserializedHandler(System.Drawing.Point point);

    public delegate void TAddNewDialogDeserializedHandler(Options.TAddNewDialog taddnewdialog);

    public class TApplicationStsWriter : TApplicationStsSerializer.Writer
    {


		/// <summary>Writes an object of type Options.TApplicationSts.</summary>
		internal void Write(Options.TApplicationSts obj)
		{
			Write8_TApplicationSts(obj);
		}}
}
