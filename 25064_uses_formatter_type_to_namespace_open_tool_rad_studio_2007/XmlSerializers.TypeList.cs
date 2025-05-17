#if _DYNAMIC_XMLSERIALIZER_COMPILATION
[assembly:System.Security.AllowPartiallyTrustedCallers()]
[assembly:System.Security.SecurityTransparent()]
#endif
[assembly:System.Reflection.AssemblyVersionAttribute("6.0.0.0")]
[assembly:System.Xml.Serialization.XmlSerializerVersionAttribute(ParentAssemblyId=@"50f533ac-abae-4783-ba8f-d875bcf9b088,", Version=@"2.0.0.0")]
namespace XmlSerializers.TypeList {

    public class XmlSerializationWriterTTypeList : System.Xml.Serialization.XmlSerializationWriter {

        public void Write4_TTypeList(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TTypeList", @"");
                return;
            }
            TopLevelElement();
            Write3_TTypeList(@"TTypeList", @"", ((global::TypeList.TTypeList)o), true, false);
        }

        void Write3_TTypeList(string n, string ns, global::TypeList.TTypeList o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::TypeList.TTypeList)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TTypeList", @"");
            {
                global::System.Collections.ArrayList a = (global::System.Collections.ArrayList)((global::System.Collections.ArrayList)o.@KnownTypes);
                if (a != null){
                    WriteStartElement(@"KnownTypes", @"", null, false);
                    for (int ia = 0; ia < ((System.Collections.ICollection)a).Count; ia++) {
                        Write1_Object(@"anyType", @"", ((global::System.Object)a[ia]), true, false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        void Write1_Object(string n, string ns, global::System.Object o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::System.Object)) {
                }
                else if (t == typeof(global::Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns,(global::Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(global::TypeList.TTypeList)) {
                    Write3_TTypeList(n, ns,(global::TypeList.TTypeList)o, isNullable, true);
                    return;
                }
                else if (t == typeof(global::System.Collections.ArrayList)) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfAnyType", @"");
                    {
                        global::System.Collections.ArrayList a = (global::System.Collections.ArrayList)o;
                        if (a != null) {
                            for (int ia = 0; ia < ((System.Collections.ICollection)a).Count; ia++) {
                                Write1_Object(@"anyType", @"", ((global::System.Object)a[ia]), true, false);
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
            WriteStartElement(n, ns, o, false, null);
            WriteEndElement(o);
        }

        void Write2_TSettingsObject(string n, string ns, global::Commons.TSettingsObject o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::Commons.TSettingsObject)) {
                }
                else if (t == typeof(global::TypeList.TTypeList)) {
                    Write3_TTypeList(n, ns,(global::TypeList.TTypeList)o, isNullable, true);
                    return;
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
        }

        protected override void InitCallbacks() {
        }
    }

    public class XmlSerializationReaderTTypeList : System.Xml.Serialization.XmlSerializationReader {

        public object Read4_TTypeList() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TTypeList && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read3_TTypeList(true, true);
                }
                else {
                    throw CreateUnknownNodeException();
                }
            }
            else {
                UnknownNode(null, @":TTypeList");
            }
            return (object)o;
        }

        global::TypeList.TTypeList Read3_TTypeList(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id1_TTypeList && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::TypeList.TTypeList o;
            o = new global::TypeList.TTypeList();
            if ((object)(o.@KnownTypes) == null) o.@KnownTypes = new global::System.Collections.ArrayList();
            global::System.Collections.ArrayList a_0 = (global::System.Collections.ArrayList)o.@KnownTypes;
            bool[] paramsRead = new bool[1];
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
                    if (((object) Reader.LocalName == (object)id3_KnownTypes && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            if ((object)(o.@KnownTypes) == null) o.@KnownTypes = new global::System.Collections.ArrayList();
                            global::System.Collections.ArrayList a_0_0 = (global::System.Collections.ArrayList)o.@KnownTypes;
                            if ((Reader.IsEmptyElement)) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                int whileIterations1 = 0;
                                int readerCount1 = ReaderCount;
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            if ((object)(a_0_0) == null) Reader.Skip(); else a_0_0.Add(Read1_Object(true, true));
                                        }
                                        else {
                                            UnknownNode(null, @":anyType");
                                        }
                                    }
                                    else {
                                        UnknownNode(null, @":anyType");
                                    }
                                    Reader.MoveToContent();
                                    CheckReaderCount(ref whileIterations1, ref readerCount1);
                                }
                            ReadEndElement();
                            }
                        }
                    }
                    else {
                        UnknownNode((object)o, @":KnownTypes");
                    }
                }
                else {
                    UnknownNode((object)o, @":KnownTypes");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations0, ref readerCount0);
            }
            ReadEndElement();
            return o;
        }

        global::System.Object Read1_Object(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
                if (isNull) {
                    if (xsiType != null) return (global::System.Object)ReadTypedNull(xsiType);
                    else return null;
                }
                if (xsiType == null) {
                    return ReadTypedPrimitive(new System.Xml.XmlQualifiedName("anyType", "http://www.w3.org/2001/XMLSchema"));
                }
                else if (((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id5_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id1_TTypeList && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item))
                    return Read3_TTypeList(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id6_ArrayOfAnyType && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
                    global::System.Collections.ArrayList a = null;
                    if (!ReadNull()) {
                        if ((object)(a) == null) a = new global::System.Collections.ArrayList();
                        global::System.Collections.ArrayList z_0_0 = (global::System.Collections.ArrayList)a;
                        if ((Reader.IsEmptyElement)) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            int whileIterations2 = 0;
                            int readerCount2 = ReaderCount;
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id4_anyType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        if ((object)(z_0_0) == null) Reader.Skip(); else z_0_0.Add(Read1_Object(true, true));
                                    }
                                    else {
                                        UnknownNode(null, @":anyType");
                                    }
                                }
                                else {
                                    UnknownNode(null, @":anyType");
                                }
                                Reader.MoveToContent();
                                CheckReaderCount(ref whileIterations2, ref readerCount2);
                            }
                        ReadEndElement();
                        }
                    }
                    return a;
                }
                else
                    return ReadTypedPrimitive((System.Xml.XmlQualifiedName)xsiType);
                }
                if (isNull) return null;
                global::System.Object o;
                o = new global::System.Object();
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
                int whileIterations3 = 0;
                int readerCount3 = ReaderCount;
                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                        UnknownNode((object)o, @"");
                    }
                    else {
                        UnknownNode((object)o, @"");
                    }
                    Reader.MoveToContent();
                    CheckReaderCount(ref whileIterations3, ref readerCount3);
                }
                ReadEndElement();
                return o;
            }

            global::Commons.TSettingsObject Read2_TSettingsObject(bool isNullable, bool checkType) {
                System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
                bool isNull = false;
                if (isNullable) isNull = ReadNull();
                if (checkType) {
                if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id5_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
                }
                else if (((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id1_TTypeList && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item))
                    return Read3_TTypeList(isNullable, false);
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
                }
                if (isNull) return null;
                throw CreateAbstractTypeException(@"TSettingsObject", @"");
            }

            protected override void InitCallbacks() {
            }

            string id1_TTypeList;
            string id3_KnownTypes;
            string id4_anyType;
            string id2_Item;
            string id5_TSettingsObject;
            string id6_ArrayOfAnyType;

            protected override void InitIDs() {
                id1_TTypeList = Reader.NameTable.Add(@"TTypeList");
                id3_KnownTypes = Reader.NameTable.Add(@"KnownTypes");
                id4_anyType = Reader.NameTable.Add(@"anyType");
                id2_Item = Reader.NameTable.Add(@"");
                id5_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
                id6_ArrayOfAnyType = Reader.NameTable.Add(@"ArrayOfAnyType");
            }
        }

        public abstract class XmlSerializer1 : System.Xml.Serialization.XmlSerializer {
            protected override System.Xml.Serialization.XmlSerializationReader CreateReader() {
                return new XmlSerializationReaderTTypeList();
            }
            protected override System.Xml.Serialization.XmlSerializationWriter CreateWriter() {
                return new XmlSerializationWriterTTypeList();
            }
        }

        public sealed class TTypeListSerializer : XmlSerializer1 {

            public override System.Boolean CanDeserialize(System.Xml.XmlReader xmlReader) {
                return xmlReader.IsStartElement(@"TTypeList", @"");
            }

            protected override void Serialize(object objectToSerialize, System.Xml.Serialization.XmlSerializationWriter writer) {
                ((XmlSerializationWriterTTypeList)writer).Write4_TTypeList(objectToSerialize);
            }

            protected override object Deserialize(System.Xml.Serialization.XmlSerializationReader reader) {
                return ((XmlSerializationReaderTTypeList)reader).Read4_TTypeList();
            }
        }

        public class XmlSerializerContract : global::System.Xml.Serialization.XmlSerializerImplementation {
            public override global::System.Xml.Serialization.XmlSerializationReader Reader { get { return new XmlSerializationReaderTTypeList(); } }
            public override global::System.Xml.Serialization.XmlSerializationWriter Writer { get { return new XmlSerializationWriterTTypeList(); } }
            System.Collections.Hashtable readMethods = null;
            public override System.Collections.Hashtable ReadMethods {
                get {
                    if (readMethods == null) {
                        System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                        _tmp[@"TypeList.TTypeList::"] = @"Read4_TTypeList";
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
                        _tmp[@"TypeList.TTypeList::"] = @"Write4_TTypeList";
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
                        _tmp.Add(@"TypeList.TTypeList::", new TTypeListSerializer());
                        if (typedSerializers == null) typedSerializers = _tmp;
                    }
                    return typedSerializers;
                }
            }
            public override System.Boolean CanSerialize(System.Type type) {
                if (type == typeof(global::TypeList.TTypeList)) return true;
                return false;
            }
            public override System.Xml.Serialization.XmlSerializer GetSerializer(System.Type type) {
                if (type == typeof(global::TypeList.TTypeList)) return new TTypeListSerializer();
                return null;
            }
        }
    }
