namespace BorlandProjectSettings
{
    using System.Xml.Serialization;
    using System;


    /// <summary>Custom serializer for TBorlandProject type.</summary
    public class TBorlandProjectSerializer : System.Xml.Serialization.XmlSerializer
    {


		BorlandProjectReader _reader;
		BorlandProjectWriter _writer;

		/// <summary>Constructs the serializer with default reader and writer instances.</summary>
		public TBorlandProjectSerializer()
		{
		}

		/// <summary>Constructs the serializer with a pre-built reader.</summary>
		public TBorlandProjectSerializer(BorlandProjectReader reader)
		{
			_reader = reader;
		}

		/// <summary>Constructs the serializer with a pre-writer reader.</summary>
		public TBorlandProjectSerializer(BorlandProjectWriter writer)
		{
			_writer = writer;
		}

		/// <summary>Constructs the serializer with pre-built reader and writer.</summary>
		public TBorlandProjectSerializer(BorlandProjectReader reader, BorlandProjectWriter writer)
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
				return new BorlandProjectReader();
		}

		/// <summary>See <see cref="XmlSerializer.CreateWriter"/>.</summary>
		protected override XmlSerializationWriter CreateWriter()
		{
			if (_writer != null)
				return _writer;
			else
				return new BorlandProjectWriter();
		}

		/// <summary>See <see cref="XmlSerializer.Deserialize"/>.</summary>
		protected override object Deserialize(XmlSerializationReader reader)
		{
			if (!(reader is BorlandProjectReader))
				throw new ArgumentException("reader");

			return ((BorlandProjectReader)reader).Read();
		}

		/// <summary>See <see cref="XmlSerializer.Serialize"/>.</summary>
		protected override void Serialize(object o, XmlSerializationWriter writer)
		{
			if (!(writer is BorlandProjectWriter))
				throw new ArgumentException("writer");

			((BorlandProjectWriter)writer).Write((BorlandProjectSettings.TBorlandProject)o);
		}
        public class Reader : System.Xml.Serialization.XmlSerializationReader {

        /// <remarks/>
        protected virtual BorlandProjectSettings.TBorlandProject Read1_TBorlandProject(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TBorlandProject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TBorlandProject o = new BorlandProjectSettings.TBorlandProject();
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
                    if (!paramsRead[1] && ((object) Reader.LocalName == (object)id3_DelphiDotNetPersonality && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@DelphiDotNet = Read4_TDelphiDotNet(false, true);
                        paramsRead[1] = true;
                    }
                    else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id4_CSharpPersonality && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@CSharp = Read7_TCSharp(false, true);
                        paramsRead[2] = true;
                    }
                    else {
                        o.@More = (System.Xml.XmlElement)ReadXmlNode(false);
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
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id5_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TBorlandProject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TBorlandProject(isNullable, false);
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
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id6_TCSharp && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read7_TCSharp(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TCSharpOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read8_TCSharpOptions(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TOptionsSet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read9_TOptionsSet(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_TDelphiDotNet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read4_TDelphiDotNet(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id10_TFileInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read6_TFileInfo(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id11_TOptionInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read5_TOptionInfo(false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id5_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TBorlandProject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TBorlandProject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id12_ArrayOfTOptionInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id13_Source && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id14_ArrayOfTOptionInfo1 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id15_FileVersion && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id16_ArrayOfTOptionInfo2 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id17_Compiler && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id18_ArrayOfTOptionInfo3 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id19_Linker && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id20_ArrayOfTOptionInfo4 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id21_Directories && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id22_ArrayOfTOptionInfo5 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id23_Parameters && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id24_ArrayOfTOptionInfo6 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id25_Language && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id26_ArrayOfTOptionInfo7 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id27_VersionInfo && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id28_ArrayOfTOptionInfo8 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id29_VersionInfoKeys && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id30_ArrayOfTFileInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TFileInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TFileInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id31_File && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TFileInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TFileInfo));z_0_0[cz_0_0++] = Read6_TFileInfo(true);
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
                        a = (BorlandProjectSettings.TFileInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TFileInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id32_ArrayOfTOptionInfo9 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id33_SelectedOptionSet && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                    }
                    return a;
                }
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id34_ArrayOfTOptionInfo10 && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    BorlandProjectSettings.TOptionInfo[] a = null;
                    if (!ReadNull()) {
                        BorlandProjectSettings.TOptionInfo[] z_0_0 = null;
                        int cz_0_0 = 0;
                        if (Reader.IsEmptyElement) {
                            Reader.Skip();
                        }
                        else {
                            Reader.ReadStartElement();
                            Reader.MoveToContent();
                            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                    if (((object) Reader.LocalName == (object)id35_Options && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                        z_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo));z_0_0[cz_0_0++] = Read5_TOptionInfo(true);
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
                        a = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(z_0_0, cz_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
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
        protected virtual BorlandProjectSettings.TDelphiDotNet Read4_TDelphiDotNet(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id9_TDelphiDotNet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TDelphiDotNet o = new BorlandProjectSettings.TDelphiDotNet();
            BorlandProjectSettings.TOptionInfo[] a_0 = null;
            int ca_0 = 0;
            BorlandProjectSettings.TOptionInfo[] a_1 = null;
            int ca_1 = 0;
            BorlandProjectSettings.TOptionInfo[] a_2 = null;
            int ca_2 = 0;
            BorlandProjectSettings.TOptionInfo[] a_3 = null;
            int ca_3 = 0;
            BorlandProjectSettings.TOptionInfo[] a_4 = null;
            int ca_4 = 0;
            BorlandProjectSettings.TOptionInfo[] a_5 = null;
            int ca_5 = 0;
            BorlandProjectSettings.TOptionInfo[] a_6 = null;
            int ca_6 = 0;
            BorlandProjectSettings.TOptionInfo[] a_7 = null;
            int ca_7 = 0;
            BorlandProjectSettings.TOptionInfo[] a_8 = null;
            int ca_8 = 0;
            BorlandProjectSettings.TFileInfo[] a_9 = null;
            int ca_9 = 0;
            bool[] paramsRead = new bool[10];
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
                    if (((object) Reader.LocalName == (object)id13_Source && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_0_0 = null;
                            int ca_0_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id13_Source && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_0_0, ca_0_0, typeof(BorlandProjectSettings.TOptionInfo));a_0_0[ca_0_0++] = Read5_TOptionInfo(true);
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
                            o.@Source = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_0_0, ca_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id15_FileVersion && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_1_0 = null;
                            int ca_1_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id15_FileVersion && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_1_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TOptionInfo));a_1_0[ca_1_0++] = Read5_TOptionInfo(true);
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
                            o.@FileVersion = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id17_Compiler && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_2_0 = null;
                            int ca_2_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id17_Compiler && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_2_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_2_0, ca_2_0, typeof(BorlandProjectSettings.TOptionInfo));a_2_0[ca_2_0++] = Read5_TOptionInfo(true);
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
                            o.@Compiler = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_2_0, ca_2_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id19_Linker && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_3_0 = null;
                            int ca_3_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id19_Linker && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_3_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_3_0, ca_3_0, typeof(BorlandProjectSettings.TOptionInfo));a_3_0[ca_3_0++] = Read5_TOptionInfo(true);
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
                            o.@Linker = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_3_0, ca_3_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id21_Directories && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_4_0 = null;
                            int ca_4_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id21_Directories && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_4_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_4_0, ca_4_0, typeof(BorlandProjectSettings.TOptionInfo));a_4_0[ca_4_0++] = Read5_TOptionInfo(true);
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
                            o.@Directories = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_4_0, ca_4_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id23_Parameters && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_5_0 = null;
                            int ca_5_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id23_Parameters && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_5_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_5_0, ca_5_0, typeof(BorlandProjectSettings.TOptionInfo));a_5_0[ca_5_0++] = Read5_TOptionInfo(true);
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
                            o.@Parameters = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_5_0, ca_5_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id25_Language && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_6_0 = null;
                            int ca_6_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id25_Language && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_6_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_6_0, ca_6_0, typeof(BorlandProjectSettings.TOptionInfo));a_6_0[ca_6_0++] = Read5_TOptionInfo(true);
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
                            o.@Language = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_6_0, ca_6_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id27_VersionInfo && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_7_0 = null;
                            int ca_7_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id27_VersionInfo && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_7_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_7_0, ca_7_0, typeof(BorlandProjectSettings.TOptionInfo));a_7_0[ca_7_0++] = Read5_TOptionInfo(true);
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
                            o.@VersionInfo = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_7_0, ca_7_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id29_VersionInfoKeys && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_8_0 = null;
                            int ca_8_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id29_VersionInfoKeys && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_8_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_8_0, ca_8_0, typeof(BorlandProjectSettings.TOptionInfo));a_8_0[ca_8_0++] = Read5_TOptionInfo(true);
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
                            o.@VersionInfoKeys = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_8_0, ca_8_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id36_FileList && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TFileInfo[] a_9_0 = null;
                            int ca_9_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id31_File && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_9_0 = (BorlandProjectSettings.TFileInfo[])EnsureArrayIndex(a_9_0, ca_9_0, typeof(BorlandProjectSettings.TFileInfo));a_9_0[ca_9_0++] = Read6_TFileInfo(true);
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
                            o.@FileList = (BorlandProjectSettings.TFileInfo[])ShrinkArray(a_9_0, ca_9_0, typeof(BorlandProjectSettings.TFileInfo), false);
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
        protected virtual BorlandProjectSettings.TOptionInfo Read5_TOptionInfo(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id11_TOptionInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TOptionInfo o;
            try {
                o = (BorlandProjectSettings.TOptionInfo)System.Activator.CreateInstance(typeof(BorlandProjectSettings.TOptionInfo));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"TOptionInfo");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"TOptionInfo");
            }
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[0] && ((object) Reader.LocalName == (object)id37_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@Name = Reader.Value;
                    paramsRead[0] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
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
                string t = null;
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    UnknownNode((object)o);
                }
                else if (Reader.NodeType == System.Xml.XmlNodeType.Text ||
                Reader.NodeType == System.Xml.XmlNodeType.CDATA ||
                Reader.NodeType == System.Xml.XmlNodeType.Whitespace ||
                Reader.NodeType == System.Xml.XmlNodeType.SignificantWhitespace) {
                    t = ReadString(t);
                    o.@Value = t;
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
        protected virtual BorlandProjectSettings.TFileInfo Read6_TFileInfo(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id10_TFileInfo && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TFileInfo o;
            try {
                o = (BorlandProjectSettings.TFileInfo)System.Activator.CreateInstance(typeof(BorlandProjectSettings.TFileInfo));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"TFileInfo");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"TFileInfo");
            }
            bool[] paramsRead = new bool[8];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[0] && ((object) Reader.LocalName == (object)id38_FileName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@FileName = Reader.Value;
                    paramsRead[0] = true;
                }
                else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id39_ContainerId && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@ContainerId = Reader.Value;
                    paramsRead[1] = true;
                }
                else if (!paramsRead[2] && ((object) Reader.LocalName == (object)id40_ModuleName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@ModuleName = Reader.Value;
                    paramsRead[2] = true;
                }
                else if (!paramsRead[3] && ((object) Reader.LocalName == (object)id41_AssemblyName && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@AssemblyName = Reader.Value;
                    paramsRead[3] = true;
                }
                else if (!paramsRead[4] && ((object) Reader.LocalName == (object)id42_Version && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@Version = Reader.Value;
                    paramsRead[4] = true;
                }
                else if (!paramsRead[5] && ((object) Reader.LocalName == (object)id43_CopyLocal && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@CopyLocal = Reader.Value;
                    paramsRead[5] = true;
                }
                else if (!paramsRead[6] && ((object) Reader.LocalName == (object)id44_LinkUnits && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@LinkUnits = Reader.Value;
                    paramsRead[6] = true;
                }
                else if (!paramsRead[7] && ((object) Reader.LocalName == (object)id45_Parent && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@Parent = Reader.Value;
                    paramsRead[7] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
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
        protected virtual BorlandProjectSettings.TCSharp Read7_TCSharp(bool isNullable, bool checkType) {
            if (isNullable && ReadNull()) return null;
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id6_TCSharp && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TCSharp o = new BorlandProjectSettings.TCSharp();
            BorlandProjectSettings.TFileInfo[] a_1 = null;
            int ca_1 = 0;
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
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id35_Options && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@Options = Read8_TCSharpOptions(true);
                        paramsRead[0] = true;
                    }
                    else if (((object) Reader.LocalName == (object)id36_FileList && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TFileInfo[] a_1_0 = null;
                            int ca_1_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id31_File && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_1_0 = (BorlandProjectSettings.TFileInfo[])EnsureArrayIndex(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TFileInfo));a_1_0[ca_1_0++] = Read6_TFileInfo(true);
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
                            o.@FileList = (BorlandProjectSettings.TFileInfo[])ShrinkArray(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TFileInfo), false);
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
        protected virtual BorlandProjectSettings.TCSharpOptions Read8_TCSharpOptions(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id7_TCSharpOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TCSharpOptions o;
            try {
                o = (BorlandProjectSettings.TCSharpOptions)System.Activator.CreateInstance(typeof(BorlandProjectSettings.TCSharpOptions));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"TCSharpOptions");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"TCSharpOptions");
            }
            BorlandProjectSettings.TOptionInfo[] a_0 = null;
            int ca_0 = 0;
            BorlandProjectSettings.TOptionsSet[] a_1 = null;
            int ca_1 = 0;
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!IsXmlnsAttribute(Reader.Name)) {
                    UnknownNode((object)o);
                }
            }
            Reader.MoveToElement();
            if (Reader.IsEmptyElement) {
                Reader.Skip();
                o.@OptionsSet = (BorlandProjectSettings.TOptionsSet[])ShrinkArray(a_1, ca_1, typeof(BorlandProjectSettings.TOptionsSet), true);
                return o;
            }
            Reader.ReadStartElement();
            Reader.MoveToContent();
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (((object) Reader.LocalName == (object)id33_SelectedOptionSet && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_0_0 = null;
                            int ca_0_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id33_SelectedOptionSet && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_0_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_0_0, ca_0_0, typeof(BorlandProjectSettings.TOptionInfo));a_0_0[ca_0_0++] = Read5_TOptionInfo(true);
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
                            o.@SelectedOptionSet = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_0_0, ca_0_0, typeof(BorlandProjectSettings.TOptionInfo), false);
                        }
                    }
                    else if (((object) Reader.LocalName == (object)id46_OptionsSet && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        a_1 = (BorlandProjectSettings.TOptionsSet[])EnsureArrayIndex(a_1, ca_1, typeof(BorlandProjectSettings.TOptionsSet));a_1[ca_1++] = Read9_TOptionsSet(true);
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
            o.@OptionsSet = (BorlandProjectSettings.TOptionsSet[])ShrinkArray(a_1, ca_1, typeof(BorlandProjectSettings.TOptionsSet), true);
            ReadEndElement();
            return o;
        }

        /// <remarks/>
        protected virtual BorlandProjectSettings.TOptionsSet Read9_TOptionsSet(bool checkType) {
            if (checkType) {
                System.Xml.XmlQualifiedName t = GetXsiType();
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id8_TOptionsSet && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    ;
                else
                    throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)t);
            }
            BorlandProjectSettings.TOptionsSet o;
            try {
                o = (BorlandProjectSettings.TOptionsSet)System.Activator.CreateInstance(typeof(BorlandProjectSettings.TOptionsSet));
            }
            catch (System.MissingMethodException) {
                throw CreateInaccessibleConstructorException(@"TOptionsSet");
            }
            catch (System.Security.SecurityException) {
                throw CreateCtorHasSecurityException(@"TOptionsSet");
            }
            BorlandProjectSettings.TOptionInfo[] a_1 = null;
            int ca_1 = 0;
            bool[] paramsRead = new bool[2];
            while (Reader.MoveToNextAttribute()) {
                if (!paramsRead[0] && ((object) Reader.LocalName == (object)id37_Name && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o.@Name = Reader.Value;
                    paramsRead[0] = true;
                }
                else if (!IsXmlnsAttribute(Reader.Name)) {
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
                    if (((object) Reader.LocalName == (object)id35_Options && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        if (!ReadNull()) {
                            BorlandProjectSettings.TOptionInfo[] a_1_0 = null;
                            int ca_1_0 = 0;
                            if (Reader.IsEmptyElement) {
                                Reader.Skip();
                            }
                            else {
                                Reader.ReadStartElement();
                                Reader.MoveToContent();
                                while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                                    if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                                        if (((object) Reader.LocalName == (object)id35_Options && (object) Reader.NamespaceURI == (object)id2_Item)) {
                                            a_1_0 = (BorlandProjectSettings.TOptionInfo[])EnsureArrayIndex(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TOptionInfo));a_1_0[ca_1_0++] = Read5_TOptionInfo(true);
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
                            o.@Options = (BorlandProjectSettings.TOptionInfo[])ShrinkArray(a_1_0, ca_1_0, typeof(BorlandProjectSettings.TOptionInfo), false);
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

        protected override void InitCallbacks() {
        }

        protected object Read11_BorlandProject() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id47_BorlandProject && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read1_TBorlandProject(true, true);
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

        System.String id32_ArrayOfTOptionInfo9;
        System.String id17_Compiler;
        System.String id29_VersionInfoKeys;
        System.String id7_TCSharpOptions;
        System.String id10_TFileInfo;
        System.String id39_ContainerId;
        System.String id31_File;
        System.String id30_ArrayOfTFileInfo;
        System.String id27_VersionInfo;
        System.String id3_DelphiDotNetPersonality;
        System.String id40_ModuleName;
        System.String id8_TOptionsSet;
        System.String id19_Linker;
        System.String id35_Options;
        System.String id44_LinkUnits;
        System.String id9_TDelphiDotNet;
        System.String id12_ArrayOfTOptionInfo;
        System.String id36_FileList;
        System.String id33_SelectedOptionSet;
        System.String id13_Source;
        System.String id46_OptionsSet;
        System.String id21_Directories;
        System.String id38_FileName;
        System.String id45_Parent;
        System.String id41_AssemblyName;
        System.String id5_TSettingsObject;
        System.String id42_Version;
        System.String id25_Language;
        System.String id1_TBorlandProject;
        System.String id4_CSharpPersonality;
        System.String id2_Item;
        System.String id47_BorlandProject;
        System.String id37_Name;
        System.String id34_ArrayOfTOptionInfo10;
        System.String id43_CopyLocal;
        System.String id23_Parameters;
        System.String id15_FileVersion;
        System.String id28_ArrayOfTOptionInfo8;
        System.String id6_TCSharp;
        System.String id11_TOptionInfo;
        System.String id18_ArrayOfTOptionInfo3;
        System.String id16_ArrayOfTOptionInfo2;
        System.String id14_ArrayOfTOptionInfo1;
        System.String id26_ArrayOfTOptionInfo7;
        System.String id24_ArrayOfTOptionInfo6;
        System.String id22_ArrayOfTOptionInfo5;
        System.String id20_ArrayOfTOptionInfo4;

        protected override void InitIDs() {
            id32_ArrayOfTOptionInfo9 = Reader.NameTable.Add(@"ArrayOfTOptionInfo9");
            id17_Compiler = Reader.NameTable.Add(@"Compiler");
            id29_VersionInfoKeys = Reader.NameTable.Add(@"VersionInfoKeys");
            id7_TCSharpOptions = Reader.NameTable.Add(@"TCSharpOptions");
            id10_TFileInfo = Reader.NameTable.Add(@"TFileInfo");
            id39_ContainerId = Reader.NameTable.Add(@"ContainerId");
            id31_File = Reader.NameTable.Add(@"File");
            id30_ArrayOfTFileInfo = Reader.NameTable.Add(@"ArrayOfTFileInfo");
            id27_VersionInfo = Reader.NameTable.Add(@"VersionInfo");
            id3_DelphiDotNetPersonality = Reader.NameTable.Add(@"DelphiDotNet.Personality");
            id40_ModuleName = Reader.NameTable.Add(@"ModuleName");
            id8_TOptionsSet = Reader.NameTable.Add(@"TOptionsSet");
            id19_Linker = Reader.NameTable.Add(@"Linker");
            id35_Options = Reader.NameTable.Add(@"Options");
            id44_LinkUnits = Reader.NameTable.Add(@"LinkUnits");
            id9_TDelphiDotNet = Reader.NameTable.Add(@"TDelphiDotNet");
            id12_ArrayOfTOptionInfo = Reader.NameTable.Add(@"ArrayOfTOptionInfo");
            id36_FileList = Reader.NameTable.Add(@"FileList");
            id33_SelectedOptionSet = Reader.NameTable.Add(@"SelectedOptionSet");
            id13_Source = Reader.NameTable.Add(@"Source");
            id46_OptionsSet = Reader.NameTable.Add(@"OptionsSet");
            id21_Directories = Reader.NameTable.Add(@"Directories");
            id38_FileName = Reader.NameTable.Add(@"FileName");
            id45_Parent = Reader.NameTable.Add(@"Parent");
            id41_AssemblyName = Reader.NameTable.Add(@"AssemblyName");
            id5_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
            id42_Version = Reader.NameTable.Add(@"Version");
            id25_Language = Reader.NameTable.Add(@"Language");
            id1_TBorlandProject = Reader.NameTable.Add(@"TBorlandProject");
            id4_CSharpPersonality = Reader.NameTable.Add(@"CSharp.Personality");
            id2_Item = Reader.NameTable.Add(@"");
            id47_BorlandProject = Reader.NameTable.Add(@"BorlandProject");
            id37_Name = Reader.NameTable.Add(@"Name");
            id34_ArrayOfTOptionInfo10 = Reader.NameTable.Add(@"ArrayOfTOptionInfo10");
            id43_CopyLocal = Reader.NameTable.Add(@"CopyLocal");
            id23_Parameters = Reader.NameTable.Add(@"Parameters");
            id15_FileVersion = Reader.NameTable.Add(@"FileVersion");
            id28_ArrayOfTOptionInfo8 = Reader.NameTable.Add(@"ArrayOfTOptionInfo8");
            id6_TCSharp = Reader.NameTable.Add(@"TCSharp");
            id11_TOptionInfo = Reader.NameTable.Add(@"TOptionInfo");
            id18_ArrayOfTOptionInfo3 = Reader.NameTable.Add(@"ArrayOfTOptionInfo3");
            id16_ArrayOfTOptionInfo2 = Reader.NameTable.Add(@"ArrayOfTOptionInfo2");
            id14_ArrayOfTOptionInfo1 = Reader.NameTable.Add(@"ArrayOfTOptionInfo1");
            id26_ArrayOfTOptionInfo7 = Reader.NameTable.Add(@"ArrayOfTOptionInfo7");
            id24_ArrayOfTOptionInfo6 = Reader.NameTable.Add(@"ArrayOfTOptionInfo6");
            id22_ArrayOfTOptionInfo5 = Reader.NameTable.Add(@"ArrayOfTOptionInfo5");
            id20_ArrayOfTOptionInfo4 = Reader.NameTable.Add(@"ArrayOfTOptionInfo4");
        }
    }
        public class Writer : System.Xml.Serialization.XmlSerializationWriter {

        void Write1_TBorlandProject(string n, string ns, BorlandProjectSettings.TBorlandProject o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TBorlandProject))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TBorlandProject", @"");
            WriteElementLiteral(((System.Xml.XmlElement)o.@More), @"", "", false, true);
            Write4_TDelphiDotNet(@"DelphiDotNet.Personality", @"", ((BorlandProjectSettings.TDelphiDotNet)o.@DelphiDotNet), false, false);
            Write7_TCSharp(@"CSharp.Personality", @"", ((BorlandProjectSettings.TCSharp)o.@CSharp), false, false);
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
                else if (t == typeof(BorlandProjectSettings.TBorlandProject)) {
                    Write1_TBorlandProject(n, ns, (BorlandProjectSettings.TBorlandProject)o, isNullable, true);
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
                else if (t == typeof(BorlandProjectSettings.TCSharp)) {
                    Write7_TCSharp(n, ns, (BorlandProjectSettings.TCSharp)o, isNullable, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TCSharpOptions)) {
                    Write8_TCSharpOptions(n, ns, (BorlandProjectSettings.TCSharpOptions)o, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionsSet)) {
                    Write9_TOptionsSet(n, ns, (BorlandProjectSettings.TOptionsSet)o, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TDelphiDotNet)) {
                    Write4_TDelphiDotNet(n, ns, (BorlandProjectSettings.TDelphiDotNet)o, isNullable, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TFileInfo)) {
                    Write6_TFileInfo(n, ns, (BorlandProjectSettings.TFileInfo)o, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo)) {
                    Write5_TOptionInfo(n, ns, (BorlandProjectSettings.TOptionInfo)o, true);
                    return;
                }
                else if (t == typeof(Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns, (Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TBorlandProject)) {
                    Write1_TBorlandProject(n, ns, (BorlandProjectSettings.TBorlandProject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Source", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo1", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"FileVersion", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo2", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Compiler", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo3", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Linker", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo4", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Directories", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo5", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Parameters", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo6", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Language", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo7", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"VersionInfo", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo8", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"VersionInfoKeys", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TFileInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTFileInfo", @"");
                    {
                        BorlandProjectSettings.TFileInfo[] a = (BorlandProjectSettings.TFileInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write6_TFileInfo(@"File", @"", ((BorlandProjectSettings.TFileInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo9", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"SelectedOptionSet", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                            }
                        }
                    }
                    Writer.WriteEndElement();
                    return;
                }
                else if (t == typeof(BorlandProjectSettings.TOptionInfo[])) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"ArrayOfTOptionInfo10", @"");
                    {
                        BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])o;
                        if (a != null) {
                            for (int ia = 0; ia < a.Length; ia++) {
                                Write5_TOptionInfo(@"Options", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
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

        void Write4_TDelphiDotNet(string n, string ns, BorlandProjectSettings.TDelphiDotNet o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TDelphiDotNet))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TDelphiDotNet", @"");
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Source);
                if (a != null){
                    WriteStartElement(@"Source", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Source", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@FileVersion);
                if (a != null){
                    WriteStartElement(@"FileVersion", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"FileVersion", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Compiler);
                if (a != null){
                    WriteStartElement(@"Compiler", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Compiler", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Linker);
                if (a != null){
                    WriteStartElement(@"Linker", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Linker", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Directories);
                if (a != null){
                    WriteStartElement(@"Directories", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Directories", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Parameters);
                if (a != null){
                    WriteStartElement(@"Parameters", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Parameters", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Language);
                if (a != null){
                    WriteStartElement(@"Language", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Language", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@VersionInfo);
                if (a != null){
                    WriteStartElement(@"VersionInfo", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"VersionInfo", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@VersionInfoKeys);
                if (a != null){
                    WriteStartElement(@"VersionInfoKeys", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"VersionInfoKeys", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TFileInfo[] a = (BorlandProjectSettings.TFileInfo[])((BorlandProjectSettings.TFileInfo[])o.@FileList);
                if (a != null){
                    WriteStartElement(@"FileList", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write6_TFileInfo(@"File", @"", ((BorlandProjectSettings.TFileInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        void Write5_TOptionInfo(string n, string ns, BorlandProjectSettings.TOptionInfo o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TOptionInfo))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TOptionInfo", @"");
            WriteAttribute(@"Name", @"", (System.String)o.@Name);
            {
                WriteValue(((System.String)o.@Value));
            }
            WriteEndElement(o);
        }

        void Write6_TFileInfo(string n, string ns, BorlandProjectSettings.TFileInfo o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TFileInfo))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TFileInfo", @"");
            WriteAttribute(@"FileName", @"", (System.String)o.@FileName);
            WriteAttribute(@"ContainerId", @"", (System.String)o.@ContainerId);
            WriteAttribute(@"ModuleName", @"", (System.String)o.@ModuleName);
            WriteAttribute(@"AssemblyName", @"", (System.String)o.@AssemblyName);
            WriteAttribute(@"Version", @"", (System.String)o.@Version);
            WriteAttribute(@"CopyLocal", @"", (System.String)o.@CopyLocal);
            WriteAttribute(@"LinkUnits", @"", (System.String)o.@LinkUnits);
            WriteAttribute(@"Parent", @"", (System.String)o.@Parent);
            WriteEndElement(o);
        }

        void Write7_TCSharp(string n, string ns, BorlandProjectSettings.TCSharp o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TCSharp))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TCSharp", @"");
            Write8_TCSharpOptions(@"Options", @"", ((BorlandProjectSettings.TCSharpOptions)o.@Options), false);
            {
                BorlandProjectSettings.TFileInfo[] a = (BorlandProjectSettings.TFileInfo[])((BorlandProjectSettings.TFileInfo[])o.@FileList);
                if (a != null){
                    WriteStartElement(@"FileList", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write6_TFileInfo(@"File", @"", ((BorlandProjectSettings.TFileInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        void Write8_TCSharpOptions(string n, string ns, BorlandProjectSettings.TCSharpOptions o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TCSharpOptions))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TCSharpOptions", @"");
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@SelectedOptionSet);
                if (a != null){
                    WriteStartElement(@"SelectedOptionSet", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"SelectedOptionSet", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            {
                BorlandProjectSettings.TOptionsSet[] a = (BorlandProjectSettings.TOptionsSet[])o.@OptionsSet;
                if (a != null) {
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write9_TOptionsSet(@"OptionsSet", @"", ((BorlandProjectSettings.TOptionsSet)a[ia]), false);
                    }
                }
            }
            WriteEndElement(o);
        }

        void Write9_TOptionsSet(string n, string ns, BorlandProjectSettings.TOptionsSet o, bool needType) {
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(BorlandProjectSettings.TOptionsSet))
                    ;
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o);
            if (needType) WriteXsiType(@"TOptionsSet", @"");
            WriteAttribute(@"Name", @"", (System.String)o.@Name);
            {
                BorlandProjectSettings.TOptionInfo[] a = (BorlandProjectSettings.TOptionInfo[])((BorlandProjectSettings.TOptionInfo[])o.@Options);
                if (a != null){
                    WriteStartElement(@"Options", @"");
                    for (int ia = 0; ia < a.Length; ia++) {
                        Write5_TOptionInfo(@"Options", @"", ((BorlandProjectSettings.TOptionInfo)a[ia]), false);
                    }
                    WriteEndElement();
                }
            }
            WriteEndElement(o);
        }

        protected override void InitCallbacks() {
        }

        protected void Write10_BorlandProject(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"BorlandProject", @"");
                return;
            }
            TopLevelElement();
            Write1_TBorlandProject(@"BorlandProject", @"", ((BorlandProjectSettings.TBorlandProject)o), true, false);
        }
    }

    /// <remarks/>
	}

    public class BorlandProjectReader : TBorlandProjectSerializer.Reader
    {


		/// <remarks/>
		protected override BorlandProjectSettings.TBorlandProject Read1_TBorlandProject(bool isNullable, bool checkType)
		{
			BorlandProjectSettings.TBorlandProject obj = base.Read1_TBorlandProject(isNullable, checkType);
			TBorlandProjectDeserializedHandler handler = TBorlandProjectDeserialized;
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
		protected override BorlandProjectSettings.TDelphiDotNet Read4_TDelphiDotNet(bool isNullable, bool checkType)
		{
			BorlandProjectSettings.TDelphiDotNet obj = base.Read4_TDelphiDotNet(isNullable, checkType);
			TDelphiDotNetDeserializedHandler handler = TDelphiDotNetDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override BorlandProjectSettings.TOptionInfo Read5_TOptionInfo(bool checkType)
		{
			BorlandProjectSettings.TOptionInfo obj = base.Read5_TOptionInfo(checkType);
			TOptionInfoDeserializedHandler handler = TOptionInfoDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override BorlandProjectSettings.TFileInfo Read6_TFileInfo(bool checkType)
		{
			BorlandProjectSettings.TFileInfo obj = base.Read6_TFileInfo(checkType);
			TFileInfoDeserializedHandler handler = TFileInfoDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override BorlandProjectSettings.TCSharp Read7_TCSharp(bool isNullable, bool checkType)
		{
			BorlandProjectSettings.TCSharp obj = base.Read7_TCSharp(isNullable, checkType);
			TCSharpDeserializedHandler handler = TCSharpDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override BorlandProjectSettings.TCSharpOptions Read8_TCSharpOptions(bool checkType)
		{
			BorlandProjectSettings.TCSharpOptions obj = base.Read8_TCSharpOptions(checkType);
			TCSharpOptionsDeserializedHandler handler = TCSharpOptionsDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <remarks/>
		protected override BorlandProjectSettings.TOptionsSet Read9_TOptionsSet(bool checkType)
		{
			BorlandProjectSettings.TOptionsSet obj = base.Read9_TOptionsSet(checkType);
			TOptionsSetDeserializedHandler handler = TOptionsSetDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <summary>Reads an object of type BorlandProjectSettings.TBorlandProject.</summary>
		internal BorlandProjectSettings.TBorlandProject Read()
		{
			return (BorlandProjectSettings.TBorlandProject) Read11_BorlandProject();
		}
        public event TBorlandProjectDeserializedHandler TBorlandProjectDeserialized;

        public event TSettingsObjectDeserializedHandler TSettingsObjectDeserialized;

        public event TDelphiDotNetDeserializedHandler TDelphiDotNetDeserialized;

        public event TOptionInfoDeserializedHandler TOptionInfoDeserialized;

        public event TFileInfoDeserializedHandler TFileInfoDeserialized;

        public event TCSharpDeserializedHandler TCSharpDeserialized;

        public event TCSharpOptionsDeserializedHandler TCSharpOptionsDeserialized;

        public event TOptionsSetDeserializedHandler TOptionsSetDeserialized;
    }

    public delegate void TBorlandProjectDeserializedHandler(BorlandProjectSettings.TBorlandProject tborlandproject);

    public delegate void TSettingsObjectDeserializedHandler(Commons.TSettingsObject tsettingsobject);

    public delegate void TDelphiDotNetDeserializedHandler(BorlandProjectSettings.TDelphiDotNet tdelphidotnet);

    public delegate void TOptionInfoDeserializedHandler(BorlandProjectSettings.TOptionInfo toptioninfo);

    public delegate void TFileInfoDeserializedHandler(BorlandProjectSettings.TFileInfo tfileinfo);

    public delegate void TCSharpDeserializedHandler(BorlandProjectSettings.TCSharp tcsharp);

    public delegate void TCSharpOptionsDeserializedHandler(BorlandProjectSettings.TCSharpOptions tcsharpoptions);

    public delegate void TOptionsSetDeserializedHandler(BorlandProjectSettings.TOptionsSet toptionsset);

    public class BorlandProjectWriter : TBorlandProjectSerializer.Writer
    {


		/// <summary>Writes an object of type BorlandProjectSettings.TBorlandProject.</summary>
		internal void Write(BorlandProjectSettings.TBorlandProject obj)
		{
			Write10_BorlandProject(obj);
		}}
}
