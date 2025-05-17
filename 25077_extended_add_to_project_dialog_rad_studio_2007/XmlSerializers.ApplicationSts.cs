#if _DYNAMIC_XMLSERIALIZER_COMPILATION
[assembly:System.Security.AllowPartiallyTrustedCallers()]
[assembly:System.Security.SecurityTransparent()]
#endif
[assembly:System.Reflection.AssemblyVersionAttribute("5.0.0.0")]
[assembly:System.Xml.Serialization.XmlSerializerVersionAttribute(ParentAssemblyId=@"2cd18d27-7f28-4313-8886-6a28f7c60ab7,", Version=@"2.0.0.0")]
namespace XmlSerializers.ApplicationSts {

    public class XmlSerializationWriterTApplicationSts : System.Xml.Serialization.XmlSerializationWriter {

        public void Write8_TApplicationSts(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TApplicationSts", @"");
                return;
            }
            TopLevelElement();
            Write7_TApplicationSts(@"TApplicationSts", @"", ((global::Options.TApplicationSts)o), true, false);
        }

        void Write7_TApplicationSts(string n, string ns, global::Options.TApplicationSts o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::Options.TApplicationSts)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TApplicationSts", @"");
            Write5_TOpenDialog(@"OpenDialog", @"", ((global::Options.TOpenDialog)o.@OpenDialog), false, false);
            Write6_TAddNewDialog(@"AddNewDialog", @"", ((global::Options.TAddNewDialog)o.@AddNewDialog), false, false);
            WriteElementString(@"SearchPath", @"", ((global::System.String)o.@SearchPath));
            WriteEndElement(o);
        }

        void Write6_TAddNewDialog(string n, string ns, global::Options.TAddNewDialog o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::Options.TAddNewDialog)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TAddNewDialog", @"");
            Write3_Size(@"Size", @"", ((global::System.Drawing.Size)o.@Size), false);
            Write4_Point(@"Location", @"", ((global::System.Drawing.Point)o.@Location), false);
            WriteEndElement(o);
        }

        void Write4_Point(string n, string ns, global::System.Drawing.Point o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::System.Drawing.Point)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"Point", @"");
            WriteElementStringRaw(@"X", @"", System.Xml.XmlConvert.ToString((global::System.Int32)((global::System.Int32)o.@X)));
            WriteElementStringRaw(@"Y", @"", System.Xml.XmlConvert.ToString((global::System.Int32)((global::System.Int32)o.@Y)));
            WriteEndElement(o);
        }

        void Write3_Size(string n, string ns, global::System.Drawing.Size o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::System.Drawing.Size)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"Size", @"");
            WriteElementStringRaw(@"Width", @"", System.Xml.XmlConvert.ToString((global::System.Int32)((global::System.Int32)o.@Width)));
            WriteElementStringRaw(@"Height", @"", System.Xml.XmlConvert.ToString((global::System.Int32)((global::System.Int32)o.@Height)));
            WriteEndElement(o);
        }

        void Write5_TOpenDialog(string n, string ns, global::Options.TOpenDialog o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::Options.TOpenDialog)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TOpenDialog", @"");
            Write3_Size(@"Size", @"", ((global::System.Drawing.Size)o.@Size), false);
            Write4_Point(@"Location", @"", ((global::System.Drawing.Point)o.@Location), false);
            WriteElementStringRaw(@"ColumnFileWidth", @"", System.Xml.XmlConvert.ToString((global::System.Int32)((global::System.Int32)o.@ColumnFileWidth)));
            WriteElementStringRaw(@"Sorted", @"", System.Xml.XmlConvert.ToString((global::System.Boolean)((global::System.Boolean)o.@Sorted)));
            WriteEndElement(o);
        }

        protected override void InitCallbacks() {
        }
    }

    public class XmlSerializationReaderTApplicationSts : System.Xml.Serialization.XmlSerializationReader {

        public object Read8_TApplicationSts() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TApplicationSts && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read7_TApplicationSts(true, true);
                }
                else {
                    throw CreateUnknownNodeException();
                }
            }
            else {
                UnknownNode(null, @":TApplicationSts");
            }
            return (object)o;
        }

        global::Options.TApplicationSts Read7_TApplicationSts(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id1_TApplicationSts && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::Options.TApplicationSts o;
            o = new global::Options.TApplicationSts();
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
            int whileIterations0 = 0;
            int readerCount0 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id3_OpenDialog && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@OpenDialog = Read5_TOpenDialog(false, true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id4_AddNewDialog && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@AddNewDialog = Read6_TAddNewDialog(false, true);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id5_SearchPath && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@SearchPath = Reader.ReadElementString();
                        }
                        paramsRead[2] = true;
                    }
                    else {
                        UnknownNode((object)o, @":OpenDialog, :AddNewDialog, :SearchPath");
                    }
                }
                else {
                    UnknownNode((object)o, @":OpenDialog, :AddNewDialog, :SearchPath");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations0, ref readerCount0);
            }
            ReadEndElement();
            return o;
        }

        global::Options.TAddNewDialog Read6_TAddNewDialog(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id6_TAddNewDialog && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::Options.TAddNewDialog o;
            o = new global::Options.TAddNewDialog();
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
            int whileIterations1 = 0;
            int readerCount1 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id7_Size && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Size = Read3_Size(true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id8_Location && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Location = Read4_Point(true);
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o, @":Size, :Location");
                    }
                }
                else {
                    UnknownNode((object)o, @":Size, :Location");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations1, ref readerCount1);
            }
            ReadEndElement();
            return o;
        }

        global::System.Drawing.Point Read4_Point(bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id9_Point && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            global::System.Drawing.Point o;
            try {
                o = (global::System.Drawing.Point)System.Activator.CreateInstance(typeof(global::System.Drawing.Point), System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.CreateInstance | System.Reflection.BindingFlags.NonPublic, null, new object[0], null);
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"global::System.Drawing.Point");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"global::System.Drawing.Point");
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
            int whileIterations2 = 0;
            int readerCount2 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id10_X && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@X = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id11_Y && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Y = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        }
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o, @":X, :Y");
                    }
                }
                else {
                    UnknownNode((object)o, @":X, :Y");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations2, ref readerCount2);
            }
            ReadEndElement();
            return o;
        }

        global::System.Drawing.Size Read3_Size(bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id7_Size && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            global::System.Drawing.Size o;
            try {
                o = (global::System.Drawing.Size)System.Activator.CreateInstance(typeof(global::System.Drawing.Size), System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.CreateInstance | System.Reflection.BindingFlags.NonPublic, null, new object[0], null);
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"global::System.Drawing.Size");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"global::System.Drawing.Size");
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
            int whileIterations3 = 0;
            int readerCount3 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id12_Width && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Width = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id13_Height && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Height = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        }
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o, @":Width, :Height");
                    }
                }
                else {
                    UnknownNode((object)o, @":Width, :Height");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations3, ref readerCount3);
            }
            ReadEndElement();
            return o;
        }

        global::Options.TOpenDialog Read5_TOpenDialog(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id14_TOpenDialog && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::Options.TOpenDialog o;
            o = new global::Options.TOpenDialog();
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
            int whileIterations4 = 0;
            int readerCount4 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id7_Size && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Size = Read3_Size(true);
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id8_Location && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Location = Read4_Point(true);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id15_ColumnFileWidth && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@ColumnFileWidth = System.Xml.XmlConvert.ToInt32(Reader.ReadElementString());
                        }
                        paramsRead[2] = true;
                    }
                    else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id16_Sorted && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Sorted = System.Xml.XmlConvert.ToBoolean(Reader.ReadElementString());
                        }
                        paramsRead[3] = true;
                    }
                    else {
                        UnknownNode((object)o, @":Size, :Location, :ColumnFileWidth, :Sorted");
                    }
                }
                else {
                    UnknownNode((object)o, @":Size, :Location, :ColumnFileWidth, :Sorted");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations4, ref readerCount4);
            }
            ReadEndElement();
            return o;
        }

        protected override void InitCallbacks() {
        }

        string id6_TAddNewDialog;
        string id1_TApplicationSts;
        string id5_SearchPath;
        string id16_Sorted;
        string id13_Height;
        string id7_Size;
        string id9_Point;
        string id3_OpenDialog;
        string id4_AddNewDialog;
        string id2_Item;
        string id8_Location;
        string id14_TOpenDialog;
        string id15_ColumnFileWidth;
        string id10_X;
        string id11_Y;
        string id12_Width;

        protected override void InitIDs() {
            id6_TAddNewDialog = Reader.NameTable.Add(@"TAddNewDialog");
            id1_TApplicationSts = Reader.NameTable.Add(@"TApplicationSts");
            id5_SearchPath = Reader.NameTable.Add(@"SearchPath");
            id16_Sorted = Reader.NameTable.Add(@"Sorted");
            id13_Height = Reader.NameTable.Add(@"Height");
            id7_Size = Reader.NameTable.Add(@"Size");
            id9_Point = Reader.NameTable.Add(@"Point");
            id3_OpenDialog = Reader.NameTable.Add(@"OpenDialog");
            id4_AddNewDialog = Reader.NameTable.Add(@"AddNewDialog");
            id2_Item = Reader.NameTable.Add(@"");
            id8_Location = Reader.NameTable.Add(@"Location");
            id14_TOpenDialog = Reader.NameTable.Add(@"TOpenDialog");
            id15_ColumnFileWidth = Reader.NameTable.Add(@"ColumnFileWidth");
            id10_X = Reader.NameTable.Add(@"X");
            id11_Y = Reader.NameTable.Add(@"Y");
            id12_Width = Reader.NameTable.Add(@"Width");
        }
    }

    public abstract class XmlSerializer1 : System.Xml.Serialization.XmlSerializer {
        protected override System.Xml.Serialization.XmlSerializationReader CreateReader() {
            return new XmlSerializationReaderTApplicationSts();
        }
        protected override System.Xml.Serialization.XmlSerializationWriter CreateWriter() {
            return new XmlSerializationWriterTApplicationSts();
        }
    }

    public sealed class TApplicationStsSerializer : XmlSerializer1 {

        public override System.Boolean CanDeserialize(System.Xml.XmlReader xmlReader) {
            return xmlReader.IsStartElement(@"TApplicationSts", @"");
        }

        protected override void Serialize(object objectToSerialize, System.Xml.Serialization.XmlSerializationWriter writer) {
            ((XmlSerializationWriterTApplicationSts)writer).Write8_TApplicationSts(objectToSerialize);
        }

        protected override object Deserialize(System.Xml.Serialization.XmlSerializationReader reader) {
            return ((XmlSerializationReaderTApplicationSts)reader).Read8_TApplicationSts();
        }
    }

    public class XmlSerializerContract : global::System.Xml.Serialization.XmlSerializerImplementation {
        public override global::System.Xml.Serialization.XmlSerializationReader Reader { get { return new XmlSerializationReaderTApplicationSts(); } }
        public override global::System.Xml.Serialization.XmlSerializationWriter Writer { get { return new XmlSerializationWriterTApplicationSts(); } }
        System.Collections.Hashtable readMethods = null;
        public override System.Collections.Hashtable ReadMethods {
            get {
                if (readMethods == null) {
                    System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                    _tmp[@"Options.TApplicationSts::"] = @"Read8_TApplicationSts";
                    if (readMethods == null) readMethods = _tmp;
                }
                return readMethods;
            }
        }
        System.Collections.Hashtable writeMethods = null;
        public override System.Collections.Hashtable WriteMethods {
            get {
                if (writeMethods == null) {
                    System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                    _tmp[@"Options.TApplicationSts::"] = @"Write8_TApplicationSts";
                    if (writeMethods == null) writeMethods = _tmp;
                }
                return writeMethods;
            }
        }
        System.Collections.Hashtable typedSerializers = null;
        public override System.Collections.Hashtable TypedSerializers {
            get {
                if (typedSerializers == null) {
                    System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                    _tmp.Add(@"Options.TApplicationSts::", new TApplicationStsSerializer());
                    if (typedSerializers == null) typedSerializers = _tmp;
                }
                return typedSerializers;
            }
        }
        public override System.Boolean CanSerialize(System.Type type) {
            if (type == typeof(global::Options.TApplicationSts)) return true;
            return false;
        }
        public override System.Xml.Serialization.XmlSerializer GetSerializer(System.Type type) {
            if (type == typeof(global::Options.TApplicationSts)) return new TApplicationStsSerializer();
            return null;
        }
    }
}
