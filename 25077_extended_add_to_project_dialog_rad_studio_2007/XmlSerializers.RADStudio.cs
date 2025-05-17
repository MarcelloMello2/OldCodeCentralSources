#if _DYNAMIC_XMLSERIALIZER_COMPILATION
[assembly:System.Security.AllowPartiallyTrustedCallers()]
[assembly:System.Security.SecurityTransparent()]
#endif
[assembly:System.Reflection.AssemblyVersionAttribute("5.0.0.0")]
[assembly:System.Xml.Serialization.XmlSerializerVersionAttribute(ParentAssemblyId=@"2cd18d27-7f28-4313-8886-6a28f7c60ab7,", Version=@"2.0.0.0")]
namespace XmlSerializers.RADStudio {

    public class XmlSerializationWriterTRADStudioProject : System.Xml.Serialization.XmlSerializationWriter {

        public void Write12_Project(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"Project", @"http://schemas.microsoft.com/developer/msbuild/2003");
                return;
            }
            TopLevelElement();
            Write11_TRADStudioProject(@"Project", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.TRADStudioProject)o), true, false);
        }

        void Write11_TRADStudioProject(string n, string ns, global::RADStudioProject.TRADStudioProject o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.TRADStudioProject)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TRADStudioProject", @"http://schemas.microsoft.com/developer/msbuild/2003");
            {
                global::RADStudioProject.ProjectPropertyGroup[] a = (global::RADStudioProject.ProjectPropertyGroup[])o.@PropertyGroup;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write4_ProjectPropertyGroup(@"PropertyGroup", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectPropertyGroup)a[ia]), false, false);
                    }
                }
            }
            if ((o.@ProjectExtensions) is System.Xml.XmlNode || o.@ProjectExtensions == null) {
                WriteElementLiteral((System.Xml.XmlNode)o.@ProjectExtensions, @"ProjectExtensions", @"http://schemas.microsoft.com/developer/msbuild/2003", false, true);
            }
            else {
                throw CreateInvalidAnyTypeException(o.@ProjectExtensions);
            }
            if ((o.@Import) is System.Xml.XmlNode || o.@Import == null) {
                WriteElementLiteral((System.Xml.XmlNode)o.@Import, @"Import", @"http://schemas.microsoft.com/developer/msbuild/2003", false, true);
            }
            else {
                throw CreateInvalidAnyTypeException(o.@Import);
            }
            {
                global::RADStudioProject.ProjectItemGroup[] a = (global::RADStudioProject.ProjectItemGroup[])o.@ItemGroup;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write10_ProjectItemGroup(@"ItemGroup", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroup)a[ia]), false, false);
                    }
                }
            }
            WriteEndElement(o);
        }

        void Write10_ProjectItemGroup(string n, string ns, global::RADStudioProject.ProjectItemGroup o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroup)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroup", @"http://schemas.microsoft.com/developer/msbuild/2003");
            Write5_ProjectItemGroupDelphiCompile(@"DelphiCompile", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroupDelphiCompile)o.@DelphiCompile), false, false);
            {
                global::RADStudioProject.ProjectItemGroupEmbeddedResource[] a = (global::RADStudioProject.ProjectItemGroupEmbeddedResource[])o.@EmbeddedResource;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write6_Item(@"EmbeddedResource", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroupEmbeddedResource)a[ia]), false, false);
                    }
                }
            }
            {
                global::RADStudioProject.ProjectItemGroupDCCReference[] a = (global::RADStudioProject.ProjectItemGroupDCCReference[])o.@DCCReference;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write7_ProjectItemGroupDCCReference(@"DCCReference", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroupDCCReference)a[ia]), false, false);
                    }
                }
            }
            {
                global::RADStudioProject.ProjectItemGroupCompile[] a = (global::RADStudioProject.ProjectItemGroupCompile[])o.@Compile;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write8_ProjectItemGroupCompile(@"Compile", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroupCompile)a[ia]), false, false);
                    }
                }
            }
            {
                global::RADStudioProject.ProjectItemGroupReference[] a = (global::RADStudioProject.ProjectItemGroupReference[])o.@Reference;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write9_ProjectItemGroupReference(@"Reference", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectItemGroupReference)a[ia]), false, false);
                    }
                }
            }
            WriteEndElement(o);
        }

        void Write9_ProjectItemGroupReference(string n, string ns, global::RADStudioProject.ProjectItemGroupReference o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroupReference)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroupReference", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Include", @"", ((global::System.String)o.@Include));
            WriteElementString(@"AssemblyName", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@AssemblyName));
            WriteElementString(@"CopyLocal", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@CopyLocal));
            WriteElementString(@"Version", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@Version));
            WriteElementString(@"HintPath", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@HintPath));
            WriteElementString(@"AssemblyTag", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@AssemblyTag));
            WriteElementString(@"LinkUnits", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@LinkUnits));
            WriteEndElement(o);
        }

        void Write8_ProjectItemGroupCompile(string n, string ns, global::RADStudioProject.ProjectItemGroupCompile o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroupCompile)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroupCompile", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Include", @"", ((global::System.String)o.@Include));
            WriteElementString(@"DependentUpon", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@DependentUpon));
            WriteElementString(@"AutoGen", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@AutoGen));
            WriteElementString(@"DesignTime", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@DesignTime));
            WriteElementString(@"Link", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@Link));
            WriteElementString(@"SubType", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@SubType));
            WriteEndElement(o);
        }

        void Write7_ProjectItemGroupDCCReference(string n, string ns, global::RADStudioProject.ProjectItemGroupDCCReference o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroupDCCReference)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroupDCCReference", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Include", @"", ((global::System.String)o.@Include));
            WriteElementString(@"Form", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@Form));
            WriteElementString(@"DesignClass", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@DesignClass));
            WriteEndElement(o);
        }

        void Write6_Item(string n, string ns, global::RADStudioProject.ProjectItemGroupEmbeddedResource o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroupEmbeddedResource)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroupEmbeddedResource", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Include", @"", ((global::System.String)o.@Include));
            WriteElementString(@"DependentUpon", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@DependentUpon));
            WriteEndElement(o);
        }

        void Write5_ProjectItemGroupDelphiCompile(string n, string ns, global::RADStudioProject.ProjectItemGroupDelphiCompile o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectItemGroupDelphiCompile)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectItemGroupDelphiCompile", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Include", @"", ((global::System.String)o.@Include));
            WriteElementString(@"MainSource", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@MainSource));
            WriteEndElement(o);
        }

        void Write4_ProjectPropertyGroup(string n, string ns, global::RADStudioProject.ProjectPropertyGroup o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectPropertyGroup)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectPropertyGroup", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Condition", @"", ((global::System.String)o.@Condition));
            WriteElementString(@"OutputType", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@OutputType));
            WriteElementString(@"OutputPath", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@OutputPath));
            WriteElementString(@"TargetName", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@TargetName));
            Write3_Item(@"Configuration", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::RADStudioProject.ProjectPropertyGroupConfiguration)o.@Configuration), false, false);
            WriteElementString(@"DCC_DependencyCheckOutputName", @"http://schemas.microsoft.com/developer/msbuild/2003", ((global::System.String)o.@DCC_DependencyCheckOutputName));
            WriteEndElement(o);
        }

        void Write3_Item(string n, string ns, global::RADStudioProject.ProjectPropertyGroupConfiguration o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::RADStudioProject.ProjectPropertyGroupConfiguration)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"ProjectPropertyGroupConfiguration", @"http://schemas.microsoft.com/developer/msbuild/2003");
            WriteAttribute(@"Condition", @"", ((global::System.String)o.@Condition));
            {
                WriteValue(((global::System.String)o.@Value));
            }
            WriteEndElement(o);
        }

        protected override void InitCallbacks() {
        }
    }

    public class XmlSerializationReaderTRADStudioProject : System.Xml.Serialization.XmlSerializationReader {

        public object Read12_Project() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_Project && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read11_TRADStudioProject(true, true);
                }
                else {
                    throw CreateUnknownNodeException();
                }
            }
            else {
                UnknownNode(null, @"http://schemas.microsoft.com/developer/msbuild/2003:Project");
            }
            return (object)o;
        }

        global::RADStudioProject.TRADStudioProject Read11_TRADStudioProject(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id3_TRADStudioProject && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.TRADStudioProject o;
            o = new global::RADStudioProject.TRADStudioProject();
            global::RADStudioProject.ProjectPropertyGroup[] a_0 = null;
            int ca_0 = 0;
            global::RADStudioProject.ProjectItemGroup[] a_3 = null;
            int ca_3 = 0;
            bool[] paramsRead = new bool[4];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                o.@PropertyGroup = (global::RADStudioProject.ProjectPropertyGroup[])ShrinkArray(a_0, ca_0, typeof(global::RADStudioProject.ProjectPropertyGroup), true);
                o.@ItemGroup = (global::RADStudioProject.ProjectItemGroup[])ShrinkArray(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroup), true);
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations0 = 0;
            int readerCount0 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (((object) Reader.LocalName == (object)id4_PropertyGroup && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_0 = (global::RADStudioProject.ProjectPropertyGroup[])EnsureArrayIndex(a_0, ca_0, typeof(global::RADStudioProject.ProjectPropertyGroup));a_0[ca_0++] = Read4_ProjectPropertyGroup(false, true);
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id5_ProjectExtensions && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@ProjectExtensions = (global::System.Xml.XmlElement)ReadXmlNode(false);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id6_Import && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Import = (global::System.Xml.XmlElement)ReadXmlNode(false);
                        paramsRead[2] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id7_ItemGroup && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_3 = (global::RADStudioProject.ProjectItemGroup[])EnsureArrayIndex(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroup));a_3[ca_3++] = Read10_ProjectItemGroup(false, true);
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:PropertyGroup, http://schemas.microsoft.com/developer/msbuild/2003:ProjectExtensions, http://schemas.microsoft.com/developer/msbuild/2003:Import, http://schemas.microsoft.com/developer/msbuild/2003:ItemGroup");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:PropertyGroup, http://schemas.microsoft.com/developer/msbuild/2003:ProjectExtensions, http://schemas.microsoft.com/developer/msbuild/2003:Import, http://schemas.microsoft.com/developer/msbuild/2003:ItemGroup");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations0, ref readerCount0);
            }
            o.@PropertyGroup = (global::RADStudioProject.ProjectPropertyGroup[])ShrinkArray(a_0, ca_0, typeof(global::RADStudioProject.ProjectPropertyGroup), true);
            o.@ItemGroup = (global::RADStudioProject.ProjectItemGroup[])ShrinkArray(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroup), true);
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroup Read10_ProjectItemGroup(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id8_ProjectItemGroup && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroup o;
            o = new global::RADStudioProject.ProjectItemGroup();
            global::RADStudioProject.ProjectItemGroupEmbeddedResource[] a_1 = null;
            int ca_1 = 0;
            global::RADStudioProject.ProjectItemGroupDCCReference[] a_2 = null;
            int ca_2 = 0;
            global::RADStudioProject.ProjectItemGroupCompile[] a_3 = null;
            int ca_3 = 0;
            global::RADStudioProject.ProjectItemGroupReference[] a_4 = null;
            int ca_4 = 0;
            bool[] paramsRead = new bool[5];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                o.@EmbeddedResource = (global::RADStudioProject.ProjectItemGroupEmbeddedResource[])ShrinkArray(a_1, ca_1, typeof(global::RADStudioProject.ProjectItemGroupEmbeddedResource), true);
                o.@DCCReference = (global::RADStudioProject.ProjectItemGroupDCCReference[])ShrinkArray(a_2, ca_2, typeof(global::RADStudioProject.ProjectItemGroupDCCReference), true);
                o.@Compile = (global::RADStudioProject.ProjectItemGroupCompile[])ShrinkArray(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroupCompile), true);
                o.@Reference = (global::RADStudioProject.ProjectItemGroupReference[])ShrinkArray(a_4, ca_4, typeof(global::RADStudioProject.ProjectItemGroupReference), true);
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations1 = 0;
            int readerCount1 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id9_DelphiCompile && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@DelphiCompile = Read5_ProjectItemGroupDelphiCompile(false, true);
                        paramsRead[0] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id10_EmbeddedResource && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_1 = (global::RADStudioProject.ProjectItemGroupEmbeddedResource[])EnsureArrayIndex(a_1, ca_1, typeof(global::RADStudioProject.ProjectItemGroupEmbeddedResource));a_1[ca_1++] = Read6_Item(false, true);
                    }
                    else if (((object) Reader.LocalName == (object)id11_DCCReference && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_2 = (global::RADStudioProject.ProjectItemGroupDCCReference[])EnsureArrayIndex(a_2, ca_2, typeof(global::RADStudioProject.ProjectItemGroupDCCReference));a_2[ca_2++] = Read7_ProjectItemGroupDCCReference(false, true);
                    }
                    else if (((object) Reader.LocalName == (object)id12_Compile && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_3 = (global::RADStudioProject.ProjectItemGroupCompile[])EnsureArrayIndex(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroupCompile));a_3[ca_3++] = Read8_ProjectItemGroupCompile(false, true);
                    }
                    else if (((object) Reader.LocalName == (object)id13_Reference && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_4 = (global::RADStudioProject.ProjectItemGroupReference[])EnsureArrayIndex(a_4, ca_4, typeof(global::RADStudioProject.ProjectItemGroupReference));a_4[ca_4++] = Read9_ProjectItemGroupReference(false, true);
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DelphiCompile, http://schemas.microsoft.com/developer/msbuild/2003:EmbeddedResource, http://schemas.microsoft.com/developer/msbuild/2003:DCCReference, http://schemas.microsoft.com/developer/msbuild/2003:Compile, http://schemas.microsoft.com/developer/msbuild/2003:Reference");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DelphiCompile, http://schemas.microsoft.com/developer/msbuild/2003:EmbeddedResource, http://schemas.microsoft.com/developer/msbuild/2003:DCCReference, http://schemas.microsoft.com/developer/msbuild/2003:Compile, http://schemas.microsoft.com/developer/msbuild/2003:Reference");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations1, ref readerCount1);
            }
            o.@EmbeddedResource = (global::RADStudioProject.ProjectItemGroupEmbeddedResource[])ShrinkArray(a_1, ca_1, typeof(global::RADStudioProject.ProjectItemGroupEmbeddedResource), true);
            o.@DCCReference = (global::RADStudioProject.ProjectItemGroupDCCReference[])ShrinkArray(a_2, ca_2, typeof(global::RADStudioProject.ProjectItemGroupDCCReference), true);
            o.@Compile = (global::RADStudioProject.ProjectItemGroupCompile[])ShrinkArray(a_3, ca_3, typeof(global::RADStudioProject.ProjectItemGroupCompile), true);
            o.@Reference = (global::RADStudioProject.ProjectItemGroupReference[])ShrinkArray(a_4, ca_4, typeof(global::RADStudioProject.ProjectItemGroupReference), true);
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroupReference Read9_ProjectItemGroupReference(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id14_ProjectItemGroupReference && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroupReference o;
            o = new global::RADStudioProject.ProjectItemGroupReference();
            bool[] paramsRead = new bool[7];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[6] && ((object) Reader.LocalName == (object)id15_Include && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Include = Reader.Value;
                    paramsRead[6] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Include");
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id17_AssemblyName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@AssemblyName = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id18_CopyLocal && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@CopyLocal = Reader.ReadElementString();
                        }
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id19_Version && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Version = Reader.ReadElementString();
                        }
                        paramsRead[2] = true;
                    }
                    else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id20_HintPath && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@HintPath = Reader.ReadElementString();
                        }
                        paramsRead[3] = true;
                    }
                    else if (!paramsRead[4] && ((object) Reader.LocalName == (object)id21_AssemblyTag && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@AssemblyTag = Reader.ReadElementString();
                        }
                        paramsRead[4] = true;
                    }
                    else if (!paramsRead[5] && ((object) Reader.LocalName == (object)id22_LinkUnits && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@LinkUnits = Reader.ReadElementString();
                        }
                        paramsRead[5] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:AssemblyName, http://schemas.microsoft.com/developer/msbuild/2003:CopyLocal, http://schemas.microsoft.com/developer/msbuild/2003:Version, http://schemas.microsoft.com/developer/msbuild/2003:HintPath, http://schemas.microsoft.com/developer/msbuild/2003:AssemblyTag, http://schemas.microsoft.com/developer/msbuild/2003:LinkUnits");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:AssemblyName, http://schemas.microsoft.com/developer/msbuild/2003:CopyLocal, http://schemas.microsoft.com/developer/msbuild/2003:Version, http://schemas.microsoft.com/developer/msbuild/2003:HintPath, http://schemas.microsoft.com/developer/msbuild/2003:AssemblyTag, http://schemas.microsoft.com/developer/msbuild/2003:LinkUnits");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations2, ref readerCount2);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroupCompile Read8_ProjectItemGroupCompile(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id23_ProjectItemGroupCompile && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroupCompile o;
            o = new global::RADStudioProject.ProjectItemGroupCompile();
            bool[] paramsRead = new bool[6];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[5] && ((object) Reader.LocalName == (object)id15_Include && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Include = Reader.Value;
                    paramsRead[5] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Include");
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id24_DependentUpon && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@DependentUpon = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id25_AutoGen && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@AutoGen = Reader.ReadElementString();
                        }
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id26_DesignTime && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@DesignTime = Reader.ReadElementString();
                        }
                        paramsRead[2] = true;
                    }
                    else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id27_Link && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Link = Reader.ReadElementString();
                        }
                        paramsRead[3] = true;
                    }
                    else if (!paramsRead[4] && ((object) Reader.LocalName == (object)id28_SubType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@SubType = Reader.ReadElementString();
                        }
                        paramsRead[4] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DependentUpon, http://schemas.microsoft.com/developer/msbuild/2003:AutoGen, http://schemas.microsoft.com/developer/msbuild/2003:DesignTime, http://schemas.microsoft.com/developer/msbuild/2003:Link, http://schemas.microsoft.com/developer/msbuild/2003:SubType");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DependentUpon, http://schemas.microsoft.com/developer/msbuild/2003:AutoGen, http://schemas.microsoft.com/developer/msbuild/2003:DesignTime, http://schemas.microsoft.com/developer/msbuild/2003:Link, http://schemas.microsoft.com/developer/msbuild/2003:SubType");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations3, ref readerCount3);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroupDCCReference Read7_ProjectItemGroupDCCReference(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id29_ProjectItemGroupDCCReference && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroupDCCReference o;
            o = new global::RADStudioProject.ProjectItemGroupDCCReference();
            bool[] paramsRead = new bool[3];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[2] && ((object) Reader.LocalName == (object)id15_Include && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Include = Reader.Value;
                    paramsRead[2] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Include");
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id30_Form && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@Form = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id31_DesignClass && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@DesignClass = Reader.ReadElementString();
                        }
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:Form, http://schemas.microsoft.com/developer/msbuild/2003:DesignClass");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:Form, http://schemas.microsoft.com/developer/msbuild/2003:DesignClass");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations4, ref readerCount4);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroupEmbeddedResource Read6_Item(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id32_Item && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroupEmbeddedResource o;
            o = new global::RADStudioProject.ProjectItemGroupEmbeddedResource();
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[1] && ((object) Reader.LocalName == (object)id15_Include && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Include = Reader.Value;
                    paramsRead[1] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Include");
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations5 = 0;
            int readerCount5 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id24_DependentUpon && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@DependentUpon = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DependentUpon");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:DependentUpon");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations5, ref readerCount5);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectItemGroupDelphiCompile Read5_ProjectItemGroupDelphiCompile(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id33_ProjectItemGroupDelphiCompile && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectItemGroupDelphiCompile o;
            o = new global::RADStudioProject.ProjectItemGroupDelphiCompile();
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[1] && ((object) Reader.LocalName == (object)id15_Include && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Include = Reader.Value;
                    paramsRead[1] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Include");
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations6 = 0;
            int readerCount6 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id34_MainSource && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@MainSource = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:MainSource");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:MainSource");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations6, ref readerCount6);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectPropertyGroup Read4_ProjectPropertyGroup(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id35_ProjectPropertyGroup && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectPropertyGroup o;
            o = new global::RADStudioProject.ProjectPropertyGroup();
            bool[] paramsRead = new bool[6];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[5] && ((object) Reader.LocalName == (object)id36_Condition && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Condition = Reader.Value;
                    paramsRead[5] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Condition");
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations7 = 0;
            int readerCount7 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id37_OutputType && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@OutputType = Reader.ReadElementString();
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id38_OutputPath && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@OutputPath = Reader.ReadElementString();
                        }
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id39_TargetName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@TargetName = Reader.ReadElementString();
                        }
                        paramsRead[2] = true;
                    }
                    else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id40_Configuration && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Configuration = Read3_Item(false, true);
                        paramsRead[3] = true;
                    }
                    else if (!paramsRead[4] && ((object) Reader.LocalName == (object)id41_DCC_DependencyCheckOutputName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@DCC_DependencyCheckOutputName = Reader.ReadElementString();
                        }
                        paramsRead[4] = true;
                    }
                    else {
                        UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:OutputType, http://schemas.microsoft.com/developer/msbuild/2003:OutputPath, http://schemas.microsoft.com/developer/msbuild/2003:TargetName, http://schemas.microsoft.com/developer/msbuild/2003:Configuration, http://schemas.microsoft.com/developer/msbuild/2003:DCC_DependencyCheckOutputName");
                    }
                }
                else {
                    UnknownNode((object)o, @"http://schemas.microsoft.com/developer/msbuild/2003:OutputType, http://schemas.microsoft.com/developer/msbuild/2003:OutputPath, http://schemas.microsoft.com/developer/msbuild/2003:TargetName, http://schemas.microsoft.com/developer/msbuild/2003:Configuration, http://schemas.microsoft.com/developer/msbuild/2003:DCC_DependencyCheckOutputName");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations7, ref readerCount7);
            }
            ReadEndElement();
            return o;
        }

        global::RADStudioProject.ProjectPropertyGroupConfiguration Read3_Item(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id42_Item && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::RADStudioProject.ProjectPropertyGroupConfiguration o;
            o = new global::RADStudioProject.ProjectPropertyGroupConfiguration();
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[0] && ((object) Reader.LocalName == (object)id36_Condition && (object) Reader.NamespaceURI == (object)id16_Item)) {
                    o.@Condition = Reader.Value;
                    paramsRead[0] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o, @":Condition");
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            int whileIterations8 = 0;
            int readerCount8 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                string tmp = null;
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    UnknownNode((object)o, @"");
                }
                else if (Reader.NodeType == System.Xml.XmlNodeType.Text ||
                Reader.NodeType == System.Xml.XmlNodeType.CDATA ||
                Reader.NodeType == System.Xml.XmlNodeType.Whitespace ||
                Reader.NodeType == System.Xml.XmlNodeType.SignificantWhitespace) {
                    tmp = ReadString(tmp, false);
                    o.@Value = tmp;
                }
                else {
                    UnknownNode((object)o, @"");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations8, ref readerCount8);
            }
            ReadEndElement();
            return o;
        }

        protected override void InitCallbacks() {
        }

        string id27_Link;
        string id4_PropertyGroup;
        string id16_Item;
        string id32_Item;
        string id40_Configuration;
        string id17_AssemblyName;
        string id39_TargetName;
        string id30_Form;
        string id37_OutputType;
        string id12_Compile;
        string id3_TRADStudioProject;
        string id14_ProjectItemGroupReference;
        string id8_ProjectItemGroup;
        string id29_ProjectItemGroupDCCReference;
        string id15_Include;
        string id36_Condition;
        string id20_HintPath;
        string id25_AutoGen;
        string id2_Item;
        string id34_MainSource;
        string id18_CopyLocal;
        string id19_Version;
        string id10_EmbeddedResource;
        string id28_SubType;
        string id35_ProjectPropertyGroup;
        string id26_DesignTime;
        string id33_ProjectItemGroupDelphiCompile;
        string id24_DependentUpon;
        string id1_Project;
        string id6_Import;
        string id5_ProjectExtensions;
        string id22_LinkUnits;
        string id11_DCCReference;
        string id42_Item;
        string id31_DesignClass;
        string id21_AssemblyTag;
        string id9_DelphiCompile;
        string id13_Reference;
        string id23_ProjectItemGroupCompile;
        string id38_OutputPath;
        string id7_ItemGroup;
        string id41_DCC_DependencyCheckOutputName;

        protected override void InitIDs() {
            id27_Link = Reader.NameTable.Add(@"Link");
            id4_PropertyGroup = Reader.NameTable.Add(@"PropertyGroup");
            id16_Item = Reader.NameTable.Add(@"");
            id32_Item = Reader.NameTable.Add(@"ProjectItemGroupEmbeddedResource");
            id40_Configuration = Reader.NameTable.Add(@"Configuration");
            id17_AssemblyName = Reader.NameTable.Add(@"AssemblyName");
            id39_TargetName = Reader.NameTable.Add(@"TargetName");
            id30_Form = Reader.NameTable.Add(@"Form");
            id37_OutputType = Reader.NameTable.Add(@"OutputType");
            id12_Compile = Reader.NameTable.Add(@"Compile");
            id3_TRADStudioProject = Reader.NameTable.Add(@"TRADStudioProject");
            id14_ProjectItemGroupReference = Reader.NameTable.Add(@"ProjectItemGroupReference");
            id8_ProjectItemGroup = Reader.NameTable.Add(@"ProjectItemGroup");
            id29_ProjectItemGroupDCCReference = Reader.NameTable.Add(@"ProjectItemGroupDCCReference");
            id15_Include = Reader.NameTable.Add(@"Include");
            id36_Condition = Reader.NameTable.Add(@"Condition");
            id20_HintPath = Reader.NameTable.Add(@"HintPath");
            id25_AutoGen = Reader.NameTable.Add(@"AutoGen");
            id2_Item = Reader.NameTable.Add(@"http://schemas.microsoft.com/developer/msbuild/2003");
            id34_MainSource = Reader.NameTable.Add(@"MainSource");
            id18_CopyLocal = Reader.NameTable.Add(@"CopyLocal");
            id19_Version = Reader.NameTable.Add(@"Version");
            id10_EmbeddedResource = Reader.NameTable.Add(@"EmbeddedResource");
            id28_SubType = Reader.NameTable.Add(@"SubType");
            id35_ProjectPropertyGroup = Reader.NameTable.Add(@"ProjectPropertyGroup");
            id26_DesignTime = Reader.NameTable.Add(@"DesignTime");
            id33_ProjectItemGroupDelphiCompile = Reader.NameTable.Add(@"ProjectItemGroupDelphiCompile");
            id24_DependentUpon = Reader.NameTable.Add(@"DependentUpon");
            id1_Project = Reader.NameTable.Add(@"Project");
            id6_Import = Reader.NameTable.Add(@"Import");
            id5_ProjectExtensions = Reader.NameTable.Add(@"ProjectExtensions");
            id22_LinkUnits = Reader.NameTable.Add(@"LinkUnits");
            id11_DCCReference = Reader.NameTable.Add(@"DCCReference");
            id42_Item = Reader.NameTable.Add(@"ProjectPropertyGroupConfiguration");
            id31_DesignClass = Reader.NameTable.Add(@"DesignClass");
            id21_AssemblyTag = Reader.NameTable.Add(@"AssemblyTag");
            id9_DelphiCompile = Reader.NameTable.Add(@"DelphiCompile");
            id13_Reference = Reader.NameTable.Add(@"Reference");
            id23_ProjectItemGroupCompile = Reader.NameTable.Add(@"ProjectItemGroupCompile");
            id38_OutputPath = Reader.NameTable.Add(@"OutputPath");
            id7_ItemGroup = Reader.NameTable.Add(@"ItemGroup");
            id41_DCC_DependencyCheckOutputName = Reader.NameTable.Add(@"DCC_DependencyCheckOutputName");
        }
    }

    public abstract class XmlSerializer1 : System.Xml.Serialization.XmlSerializer {
        protected override System.Xml.Serialization.XmlSerializationReader CreateReader() {
            return new XmlSerializationReaderTRADStudioProject();
        }
        protected override System.Xml.Serialization.XmlSerializationWriter CreateWriter() {
            return new XmlSerializationWriterTRADStudioProject();
        }
    }

    public sealed class TRADStudioProjectSerializer : XmlSerializer1 {

        public override System.Boolean CanDeserialize(System.Xml.XmlReader xmlReader) {
            return xmlReader.IsStartElement(@"Project", @"http://schemas.microsoft.com/developer/msbuild/2003");
        }

        protected override void Serialize(object objectToSerialize, System.Xml.Serialization.XmlSerializationWriter writer) {
            ((XmlSerializationWriterTRADStudioProject)writer).Write12_Project(objectToSerialize);
        }

        protected override object Deserialize(System.Xml.Serialization.XmlSerializationReader reader) {
            return ((XmlSerializationReaderTRADStudioProject)reader).Read12_Project();
        }
    }

    public class XmlSerializerContract : global::System.Xml.Serialization.XmlSerializerImplementation {
        public override global::System.Xml.Serialization.XmlSerializationReader Reader { get { return new XmlSerializationReaderTRADStudioProject(); } }
        public override global::System.Xml.Serialization.XmlSerializationWriter Writer { get { return new XmlSerializationWriterTRADStudioProject(); } }
        System.Collections.Hashtable readMethods = null;
        public override System.Collections.Hashtable ReadMethods {
            get {
                if (readMethods == null) {
                    System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                    _tmp[@"RADStudioProject.TRADStudioProject:http://schemas.microsoft.com/developer/msbuild/2003:Project:True:"] = @"Read12_Project";
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
                    _tmp[@"RADStudioProject.TRADStudioProject:http://schemas.microsoft.com/developer/msbuild/2003:Project:True:"] = @"Write12_Project";
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
                    _tmp.Add(@"RADStudioProject.TRADStudioProject:http://schemas.microsoft.com/developer/msbuild/2003:Project:True:", new TRADStudioProjectSerializer());
                    if (typedSerializers == null) typedSerializers = _tmp;
                }
                return typedSerializers;
            }
        }
        public override System.Boolean CanSerialize(System.Type type) {
            if (type == typeof(global::RADStudioProject.TRADStudioProject)) return true;
            return false;
        }
        public override System.Xml.Serialization.XmlSerializer GetSerializer(System.Type type) {
            if (type == typeof(global::RADStudioProject.TRADStudioProject)) return new TRADStudioProjectSerializer();
            return null;
        }
    }
}
