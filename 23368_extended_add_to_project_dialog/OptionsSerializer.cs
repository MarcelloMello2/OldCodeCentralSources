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
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id3_OpenFilesShortcut && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        o.@OpenFilesShortcut = Read4_Keys(Reader.ReadElementString());
                        paramsRead[0] = true;
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
                if (t == null || ((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id4_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
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
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id4_TSettingsObject && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read2_TSettingsObject(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id1_TOptions && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item))
                    return Read1_TOptions(isNullable, false);
                else if (((object) ((System.Xml.XmlQualifiedName)t).Name == (object)id5_Keys && (object) ((System.Xml.XmlQualifiedName)t).Namespace == (object)id2_Item)) {
                    Reader.ReadStartElement();
                    object e = Read4_Keys(Reader.ReadString());
                    ReadEndElement();
                    return e;
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

        System.Collections.Hashtable _KeysValues;

        internal System.Collections.Hashtable KeysValues {
            get {
                if ((object)_KeysValues == null) {
                    System.Collections.Hashtable h = new System.Collections.Hashtable();
                    h.Add(@"KeyCode", (System.Int64)System.Windows.Forms.Keys.@KeyCode);
                    h.Add(@"Modifiers", (System.Int64)System.Windows.Forms.Keys.@Modifiers);
                    h.Add(@"None", (System.Int64)System.Windows.Forms.Keys.@None);
                    h.Add(@"LButton", (System.Int64)System.Windows.Forms.Keys.@LButton);
                    h.Add(@"RButton", (System.Int64)System.Windows.Forms.Keys.@RButton);
                    h.Add(@"Cancel", (System.Int64)System.Windows.Forms.Keys.@Cancel);
                    h.Add(@"MButton", (System.Int64)System.Windows.Forms.Keys.@MButton);
                    h.Add(@"XButton1", (System.Int64)System.Windows.Forms.Keys.@XButton1);
                    h.Add(@"XButton2", (System.Int64)System.Windows.Forms.Keys.@XButton2);
                    h.Add(@"Back", (System.Int64)System.Windows.Forms.Keys.@Back);
                    h.Add(@"Tab", (System.Int64)System.Windows.Forms.Keys.@Tab);
                    h.Add(@"LineFeed", (System.Int64)System.Windows.Forms.Keys.@LineFeed);
                    h.Add(@"Clear", (System.Int64)System.Windows.Forms.Keys.@Clear);
                    h.Add(@"Return", (System.Int64)System.Windows.Forms.Keys.@Return);
                    h.Add(@"Enter", (System.Int64)System.Windows.Forms.Keys.@Enter);
                    h.Add(@"ShiftKey", (System.Int64)System.Windows.Forms.Keys.@ShiftKey);
                    h.Add(@"ControlKey", (System.Int64)System.Windows.Forms.Keys.@ControlKey);
                    h.Add(@"Menu", (System.Int64)System.Windows.Forms.Keys.@Menu);
                    h.Add(@"Pause", (System.Int64)System.Windows.Forms.Keys.@Pause);
                    h.Add(@"Capital", (System.Int64)System.Windows.Forms.Keys.@Capital);
                    h.Add(@"CapsLock", (System.Int64)System.Windows.Forms.Keys.@CapsLock);
                    h.Add(@"Escape", (System.Int64)System.Windows.Forms.Keys.@Escape);
                    h.Add(@"Space", (System.Int64)System.Windows.Forms.Keys.@Space);
                    h.Add(@"Prior", (System.Int64)System.Windows.Forms.Keys.@Prior);
                    h.Add(@"PageUp", (System.Int64)System.Windows.Forms.Keys.@PageUp);
                    h.Add(@"Next", (System.Int64)System.Windows.Forms.Keys.@Next);
                    h.Add(@"PageDown", (System.Int64)System.Windows.Forms.Keys.@PageDown);
                    h.Add(@"End", (System.Int64)System.Windows.Forms.Keys.@End);
                    h.Add(@"Home", (System.Int64)System.Windows.Forms.Keys.@Home);
                    h.Add(@"Left", (System.Int64)System.Windows.Forms.Keys.@Left);
                    h.Add(@"Up", (System.Int64)System.Windows.Forms.Keys.@Up);
                    h.Add(@"Right", (System.Int64)System.Windows.Forms.Keys.@Right);
                    h.Add(@"Down", (System.Int64)System.Windows.Forms.Keys.@Down);
                    h.Add(@"Select", (System.Int64)System.Windows.Forms.Keys.@Select);
                    h.Add(@"Print", (System.Int64)System.Windows.Forms.Keys.@Print);
                    h.Add(@"Execute", (System.Int64)System.Windows.Forms.Keys.@Execute);
                    h.Add(@"Snapshot", (System.Int64)System.Windows.Forms.Keys.@Snapshot);
                    h.Add(@"PrintScreen", (System.Int64)System.Windows.Forms.Keys.@PrintScreen);
                    h.Add(@"Insert", (System.Int64)System.Windows.Forms.Keys.@Insert);
                    h.Add(@"Delete", (System.Int64)System.Windows.Forms.Keys.@Delete);
                    h.Add(@"Help", (System.Int64)System.Windows.Forms.Keys.@Help);
                    h.Add(@"D0", (System.Int64)System.Windows.Forms.Keys.@D0);
                    h.Add(@"D1", (System.Int64)System.Windows.Forms.Keys.@D1);
                    h.Add(@"D2", (System.Int64)System.Windows.Forms.Keys.@D2);
                    h.Add(@"D3", (System.Int64)System.Windows.Forms.Keys.@D3);
                    h.Add(@"D4", (System.Int64)System.Windows.Forms.Keys.@D4);
                    h.Add(@"D5", (System.Int64)System.Windows.Forms.Keys.@D5);
                    h.Add(@"D6", (System.Int64)System.Windows.Forms.Keys.@D6);
                    h.Add(@"D7", (System.Int64)System.Windows.Forms.Keys.@D7);
                    h.Add(@"D8", (System.Int64)System.Windows.Forms.Keys.@D8);
                    h.Add(@"D9", (System.Int64)System.Windows.Forms.Keys.@D9);
                    h.Add(@"A", (System.Int64)System.Windows.Forms.Keys.@A);
                    h.Add(@"B", (System.Int64)System.Windows.Forms.Keys.@B);
                    h.Add(@"C", (System.Int64)System.Windows.Forms.Keys.@C);
                    h.Add(@"D", (System.Int64)System.Windows.Forms.Keys.@D);
                    h.Add(@"E", (System.Int64)System.Windows.Forms.Keys.@E);
                    h.Add(@"F", (System.Int64)System.Windows.Forms.Keys.@F);
                    h.Add(@"G", (System.Int64)System.Windows.Forms.Keys.@G);
                    h.Add(@"H", (System.Int64)System.Windows.Forms.Keys.@H);
                    h.Add(@"I", (System.Int64)System.Windows.Forms.Keys.@I);
                    h.Add(@"J", (System.Int64)System.Windows.Forms.Keys.@J);
                    h.Add(@"K", (System.Int64)System.Windows.Forms.Keys.@K);
                    h.Add(@"L", (System.Int64)System.Windows.Forms.Keys.@L);
                    h.Add(@"M", (System.Int64)System.Windows.Forms.Keys.@M);
                    h.Add(@"N", (System.Int64)System.Windows.Forms.Keys.@N);
                    h.Add(@"O", (System.Int64)System.Windows.Forms.Keys.@O);
                    h.Add(@"P", (System.Int64)System.Windows.Forms.Keys.@P);
                    h.Add(@"Q", (System.Int64)System.Windows.Forms.Keys.@Q);
                    h.Add(@"R", (System.Int64)System.Windows.Forms.Keys.@R);
                    h.Add(@"S", (System.Int64)System.Windows.Forms.Keys.@S);
                    h.Add(@"T", (System.Int64)System.Windows.Forms.Keys.@T);
                    h.Add(@"U", (System.Int64)System.Windows.Forms.Keys.@U);
                    h.Add(@"V", (System.Int64)System.Windows.Forms.Keys.@V);
                    h.Add(@"W", (System.Int64)System.Windows.Forms.Keys.@W);
                    h.Add(@"X", (System.Int64)System.Windows.Forms.Keys.@X);
                    h.Add(@"Y", (System.Int64)System.Windows.Forms.Keys.@Y);
                    h.Add(@"Z", (System.Int64)System.Windows.Forms.Keys.@Z);
                    h.Add(@"LWin", (System.Int64)System.Windows.Forms.Keys.@LWin);
                    h.Add(@"RWin", (System.Int64)System.Windows.Forms.Keys.@RWin);
                    h.Add(@"Apps", (System.Int64)System.Windows.Forms.Keys.@Apps);
                    h.Add(@"NumPad0", (System.Int64)System.Windows.Forms.Keys.@NumPad0);
                    h.Add(@"NumPad1", (System.Int64)System.Windows.Forms.Keys.@NumPad1);
                    h.Add(@"NumPad2", (System.Int64)System.Windows.Forms.Keys.@NumPad2);
                    h.Add(@"NumPad3", (System.Int64)System.Windows.Forms.Keys.@NumPad3);
                    h.Add(@"NumPad4", (System.Int64)System.Windows.Forms.Keys.@NumPad4);
                    h.Add(@"NumPad5", (System.Int64)System.Windows.Forms.Keys.@NumPad5);
                    h.Add(@"NumPad6", (System.Int64)System.Windows.Forms.Keys.@NumPad6);
                    h.Add(@"NumPad7", (System.Int64)System.Windows.Forms.Keys.@NumPad7);
                    h.Add(@"NumPad8", (System.Int64)System.Windows.Forms.Keys.@NumPad8);
                    h.Add(@"NumPad9", (System.Int64)System.Windows.Forms.Keys.@NumPad9);
                    h.Add(@"Multiply", (System.Int64)System.Windows.Forms.Keys.@Multiply);
                    h.Add(@"Add", (System.Int64)System.Windows.Forms.Keys.@Add);
                    h.Add(@"Separator", (System.Int64)System.Windows.Forms.Keys.@Separator);
                    h.Add(@"Subtract", (System.Int64)System.Windows.Forms.Keys.@Subtract);
                    h.Add(@"Decimal", (System.Int64)System.Windows.Forms.Keys.@Decimal);
                    h.Add(@"Divide", (System.Int64)System.Windows.Forms.Keys.@Divide);
                    h.Add(@"F1", (System.Int64)System.Windows.Forms.Keys.@F1);
                    h.Add(@"F2", (System.Int64)System.Windows.Forms.Keys.@F2);
                    h.Add(@"F3", (System.Int64)System.Windows.Forms.Keys.@F3);
                    h.Add(@"F4", (System.Int64)System.Windows.Forms.Keys.@F4);
                    h.Add(@"F5", (System.Int64)System.Windows.Forms.Keys.@F5);
                    h.Add(@"F6", (System.Int64)System.Windows.Forms.Keys.@F6);
                    h.Add(@"F7", (System.Int64)System.Windows.Forms.Keys.@F7);
                    h.Add(@"F8", (System.Int64)System.Windows.Forms.Keys.@F8);
                    h.Add(@"F9", (System.Int64)System.Windows.Forms.Keys.@F9);
                    h.Add(@"F10", (System.Int64)System.Windows.Forms.Keys.@F10);
                    h.Add(@"F11", (System.Int64)System.Windows.Forms.Keys.@F11);
                    h.Add(@"F12", (System.Int64)System.Windows.Forms.Keys.@F12);
                    h.Add(@"F13", (System.Int64)System.Windows.Forms.Keys.@F13);
                    h.Add(@"F14", (System.Int64)System.Windows.Forms.Keys.@F14);
                    h.Add(@"F15", (System.Int64)System.Windows.Forms.Keys.@F15);
                    h.Add(@"F16", (System.Int64)System.Windows.Forms.Keys.@F16);
                    h.Add(@"F17", (System.Int64)System.Windows.Forms.Keys.@F17);
                    h.Add(@"F18", (System.Int64)System.Windows.Forms.Keys.@F18);
                    h.Add(@"F19", (System.Int64)System.Windows.Forms.Keys.@F19);
                    h.Add(@"F20", (System.Int64)System.Windows.Forms.Keys.@F20);
                    h.Add(@"F21", (System.Int64)System.Windows.Forms.Keys.@F21);
                    h.Add(@"F22", (System.Int64)System.Windows.Forms.Keys.@F22);
                    h.Add(@"F23", (System.Int64)System.Windows.Forms.Keys.@F23);
                    h.Add(@"F24", (System.Int64)System.Windows.Forms.Keys.@F24);
                    h.Add(@"NumLock", (System.Int64)System.Windows.Forms.Keys.@NumLock);
                    h.Add(@"Scroll", (System.Int64)System.Windows.Forms.Keys.@Scroll);
                    h.Add(@"LShiftKey", (System.Int64)System.Windows.Forms.Keys.@LShiftKey);
                    h.Add(@"RShiftKey", (System.Int64)System.Windows.Forms.Keys.@RShiftKey);
                    h.Add(@"LControlKey", (System.Int64)System.Windows.Forms.Keys.@LControlKey);
                    h.Add(@"RControlKey", (System.Int64)System.Windows.Forms.Keys.@RControlKey);
                    h.Add(@"LMenu", (System.Int64)System.Windows.Forms.Keys.@LMenu);
                    h.Add(@"RMenu", (System.Int64)System.Windows.Forms.Keys.@RMenu);
                    h.Add(@"ProcessKey", (System.Int64)System.Windows.Forms.Keys.@ProcessKey);
                    h.Add(@"Attn", (System.Int64)System.Windows.Forms.Keys.@Attn);
                    h.Add(@"Crsel", (System.Int64)System.Windows.Forms.Keys.@Crsel);
                    h.Add(@"Exsel", (System.Int64)System.Windows.Forms.Keys.@Exsel);
                    h.Add(@"EraseEof", (System.Int64)System.Windows.Forms.Keys.@EraseEof);
                    h.Add(@"Play", (System.Int64)System.Windows.Forms.Keys.@Play);
                    h.Add(@"Zoom", (System.Int64)System.Windows.Forms.Keys.@Zoom);
                    h.Add(@"NoName", (System.Int64)System.Windows.Forms.Keys.@NoName);
                    h.Add(@"Pa1", (System.Int64)System.Windows.Forms.Keys.@Pa1);
                    h.Add(@"OemClear", (System.Int64)System.Windows.Forms.Keys.@OemClear);
                    h.Add(@"KanaMode", (System.Int64)System.Windows.Forms.Keys.@KanaMode);
                    h.Add(@"HanguelMode", (System.Int64)System.Windows.Forms.Keys.@HanguelMode);
                    h.Add(@"HangulMode", (System.Int64)System.Windows.Forms.Keys.@HangulMode);
                    h.Add(@"JunjaMode", (System.Int64)System.Windows.Forms.Keys.@JunjaMode);
                    h.Add(@"FinalMode", (System.Int64)System.Windows.Forms.Keys.@FinalMode);
                    h.Add(@"HanjaMode", (System.Int64)System.Windows.Forms.Keys.@HanjaMode);
                    h.Add(@"KanjiMode", (System.Int64)System.Windows.Forms.Keys.@KanjiMode);
                    h.Add(@"IMEConvert", (System.Int64)System.Windows.Forms.Keys.@IMEConvert);
                    h.Add(@"IMENonconvert", (System.Int64)System.Windows.Forms.Keys.@IMENonconvert);
                    h.Add(@"IMEAceept", (System.Int64)System.Windows.Forms.Keys.@IMEAceept);
                    h.Add(@"IMEModeChange", (System.Int64)System.Windows.Forms.Keys.@IMEModeChange);
                    h.Add(@"BrowserBack", (System.Int64)System.Windows.Forms.Keys.@BrowserBack);
                    h.Add(@"BrowserForward", (System.Int64)System.Windows.Forms.Keys.@BrowserForward);
                    h.Add(@"BrowserRefresh", (System.Int64)System.Windows.Forms.Keys.@BrowserRefresh);
                    h.Add(@"BrowserStop", (System.Int64)System.Windows.Forms.Keys.@BrowserStop);
                    h.Add(@"BrowserSearch", (System.Int64)System.Windows.Forms.Keys.@BrowserSearch);
                    h.Add(@"BrowserFavorites", (System.Int64)System.Windows.Forms.Keys.@BrowserFavorites);
                    h.Add(@"BrowserHome", (System.Int64)System.Windows.Forms.Keys.@BrowserHome);
                    h.Add(@"VolumeMute", (System.Int64)System.Windows.Forms.Keys.@VolumeMute);
                    h.Add(@"VolumeDown", (System.Int64)System.Windows.Forms.Keys.@VolumeDown);
                    h.Add(@"VolumeUp", (System.Int64)System.Windows.Forms.Keys.@VolumeUp);
                    h.Add(@"MediaNextTrack", (System.Int64)System.Windows.Forms.Keys.@MediaNextTrack);
                    h.Add(@"MediaPreviousTrack", (System.Int64)System.Windows.Forms.Keys.@MediaPreviousTrack);
                    h.Add(@"MediaStop", (System.Int64)System.Windows.Forms.Keys.@MediaStop);
                    h.Add(@"MediaPlayPause", (System.Int64)System.Windows.Forms.Keys.@MediaPlayPause);
                    h.Add(@"LaunchMail", (System.Int64)System.Windows.Forms.Keys.@LaunchMail);
                    h.Add(@"SelectMedia", (System.Int64)System.Windows.Forms.Keys.@SelectMedia);
                    h.Add(@"LaunchApplication1", (System.Int64)System.Windows.Forms.Keys.@LaunchApplication1);
                    h.Add(@"LaunchApplication2", (System.Int64)System.Windows.Forms.Keys.@LaunchApplication2);
                    h.Add(@"OemSemicolon", (System.Int64)System.Windows.Forms.Keys.@OemSemicolon);
                    h.Add(@"Oemplus", (System.Int64)System.Windows.Forms.Keys.@Oemplus);
                    h.Add(@"Oemcomma", (System.Int64)System.Windows.Forms.Keys.@Oemcomma);
                    h.Add(@"OemMinus", (System.Int64)System.Windows.Forms.Keys.@OemMinus);
                    h.Add(@"OemPeriod", (System.Int64)System.Windows.Forms.Keys.@OemPeriod);
                    h.Add(@"OemQuestion", (System.Int64)System.Windows.Forms.Keys.@OemQuestion);
                    h.Add(@"Oemtilde", (System.Int64)System.Windows.Forms.Keys.@Oemtilde);
                    h.Add(@"OemOpenBrackets", (System.Int64)System.Windows.Forms.Keys.@OemOpenBrackets);
                    h.Add(@"OemPipe", (System.Int64)System.Windows.Forms.Keys.@OemPipe);
                    h.Add(@"OemCloseBrackets", (System.Int64)System.Windows.Forms.Keys.@OemCloseBrackets);
                    h.Add(@"OemQuotes", (System.Int64)System.Windows.Forms.Keys.@OemQuotes);
                    h.Add(@"Oem8", (System.Int64)System.Windows.Forms.Keys.@Oem8);
                    h.Add(@"OemBackslash", (System.Int64)System.Windows.Forms.Keys.@OemBackslash);
                    h.Add(@"Shift", (System.Int64)System.Windows.Forms.Keys.@Shift);
                    h.Add(@"Control", (System.Int64)System.Windows.Forms.Keys.@Control);
                    h.Add(@"Alt", (System.Int64)System.Windows.Forms.Keys.@Alt);
                    _KeysValues = h;
                }
                return _KeysValues;
            }
        }

        /// <remarks/>
        protected virtual System.Windows.Forms.Keys Read4_Keys(string s) {
            return (System.Windows.Forms.Keys)ToEnum(s, KeysValues, @"System.Windows.Forms.Keys");
        }

        protected override void InitCallbacks() {
        }

        protected object Read6_TOptions() {
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

        System.String id4_TSettingsObject;
        System.String id5_Keys;
        System.String id2_Item;
        System.String id1_TOptions;
        System.String id3_OpenFilesShortcut;

        protected override void InitIDs() {
            id4_TSettingsObject = Reader.NameTable.Add(@"TSettingsObject");
            id5_Keys = Reader.NameTable.Add(@"Keys");
            id2_Item = Reader.NameTable.Add(@"");
            id1_TOptions = Reader.NameTable.Add(@"TOptions");
            id3_OpenFilesShortcut = Reader.NameTable.Add(@"OpenFilesShortcut");
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
            WriteElementString(@"OpenFilesShortcut", @"", Write4_Keys(((System.Windows.Forms.Keys)o.@OpenFilesShortcut)));
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
                else if (t == typeof(Commons.TSettingsObject)) {
                    Write2_TSettingsObject(n, ns, (Commons.TSettingsObject)o, isNullable, true);
                    return;
                }
                else if (t == typeof(Options.TOptions)) {
                    Write1_TOptions(n, ns, (Options.TOptions)o, isNullable, true);
                    return;
                }
                else if (t == typeof(System.Windows.Forms.Keys)) {
                    Writer.WriteStartElement(n, ns);
                    WriteXsiType(@"Keys", @"");
                    Writer.WriteString(Write4_Keys((System.Windows.Forms.Keys)o));
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

        string Write4_Keys(System.Windows.Forms.Keys v) {
            string s = null;
            switch (v) {
                case System.Windows.Forms.Keys.@KeyCode: s = @"KeyCode"; break;
                case System.Windows.Forms.Keys.@Modifiers: s = @"Modifiers"; break;
                case System.Windows.Forms.Keys.@None: s = @"None"; break;
                case System.Windows.Forms.Keys.@LButton: s = @"LButton"; break;
                case System.Windows.Forms.Keys.@RButton: s = @"RButton"; break;
                case System.Windows.Forms.Keys.@Cancel: s = @"Cancel"; break;
                case System.Windows.Forms.Keys.@MButton: s = @"MButton"; break;
                case System.Windows.Forms.Keys.@XButton1: s = @"XButton1"; break;
                case System.Windows.Forms.Keys.@XButton2: s = @"XButton2"; break;
                case System.Windows.Forms.Keys.@Back: s = @"Back"; break;
                case System.Windows.Forms.Keys.@Tab: s = @"Tab"; break;
                case System.Windows.Forms.Keys.@LineFeed: s = @"LineFeed"; break;
                case System.Windows.Forms.Keys.@Clear: s = @"Clear"; break;
                case System.Windows.Forms.Keys.@Return: s = @"Return"; break;
                case System.Windows.Forms.Keys.@ShiftKey: s = @"ShiftKey"; break;
                case System.Windows.Forms.Keys.@ControlKey: s = @"ControlKey"; break;
                case System.Windows.Forms.Keys.@Menu: s = @"Menu"; break;
                case System.Windows.Forms.Keys.@Pause: s = @"Pause"; break;
                case System.Windows.Forms.Keys.@Capital: s = @"Capital"; break;
                case System.Windows.Forms.Keys.@Escape: s = @"Escape"; break;
                case System.Windows.Forms.Keys.@Space: s = @"Space"; break;
                case System.Windows.Forms.Keys.@Prior: s = @"Prior"; break;
                case System.Windows.Forms.Keys.@Next: s = @"Next"; break;
                case System.Windows.Forms.Keys.@End: s = @"End"; break;
                case System.Windows.Forms.Keys.@Home: s = @"Home"; break;
                case System.Windows.Forms.Keys.@Left: s = @"Left"; break;
                case System.Windows.Forms.Keys.@Up: s = @"Up"; break;
                case System.Windows.Forms.Keys.@Right: s = @"Right"; break;
                case System.Windows.Forms.Keys.@Down: s = @"Down"; break;
                case System.Windows.Forms.Keys.@Select: s = @"Select"; break;
                case System.Windows.Forms.Keys.@Print: s = @"Print"; break;
                case System.Windows.Forms.Keys.@Execute: s = @"Execute"; break;
                case System.Windows.Forms.Keys.@Snapshot: s = @"Snapshot"; break;
                case System.Windows.Forms.Keys.@Insert: s = @"Insert"; break;
                case System.Windows.Forms.Keys.@Delete: s = @"Delete"; break;
                case System.Windows.Forms.Keys.@Help: s = @"Help"; break;
                case System.Windows.Forms.Keys.@D0: s = @"D0"; break;
                case System.Windows.Forms.Keys.@D1: s = @"D1"; break;
                case System.Windows.Forms.Keys.@D2: s = @"D2"; break;
                case System.Windows.Forms.Keys.@D3: s = @"D3"; break;
                case System.Windows.Forms.Keys.@D4: s = @"D4"; break;
                case System.Windows.Forms.Keys.@D5: s = @"D5"; break;
                case System.Windows.Forms.Keys.@D6: s = @"D6"; break;
                case System.Windows.Forms.Keys.@D7: s = @"D7"; break;
                case System.Windows.Forms.Keys.@D8: s = @"D8"; break;
                case System.Windows.Forms.Keys.@D9: s = @"D9"; break;
                case System.Windows.Forms.Keys.@A: s = @"A"; break;
                case System.Windows.Forms.Keys.@B: s = @"B"; break;
                case System.Windows.Forms.Keys.@C: s = @"C"; break;
                case System.Windows.Forms.Keys.@D: s = @"D"; break;
                case System.Windows.Forms.Keys.@E: s = @"E"; break;
                case System.Windows.Forms.Keys.@F: s = @"F"; break;
                case System.Windows.Forms.Keys.@G: s = @"G"; break;
                case System.Windows.Forms.Keys.@H: s = @"H"; break;
                case System.Windows.Forms.Keys.@I: s = @"I"; break;
                case System.Windows.Forms.Keys.@J: s = @"J"; break;
                case System.Windows.Forms.Keys.@K: s = @"K"; break;
                case System.Windows.Forms.Keys.@L: s = @"L"; break;
                case System.Windows.Forms.Keys.@M: s = @"M"; break;
                case System.Windows.Forms.Keys.@N: s = @"N"; break;
                case System.Windows.Forms.Keys.@O: s = @"O"; break;
                case System.Windows.Forms.Keys.@P: s = @"P"; break;
                case System.Windows.Forms.Keys.@Q: s = @"Q"; break;
                case System.Windows.Forms.Keys.@R: s = @"R"; break;
                case System.Windows.Forms.Keys.@S: s = @"S"; break;
                case System.Windows.Forms.Keys.@T: s = @"T"; break;
                case System.Windows.Forms.Keys.@U: s = @"U"; break;
                case System.Windows.Forms.Keys.@V: s = @"V"; break;
                case System.Windows.Forms.Keys.@W: s = @"W"; break;
                case System.Windows.Forms.Keys.@X: s = @"X"; break;
                case System.Windows.Forms.Keys.@Y: s = @"Y"; break;
                case System.Windows.Forms.Keys.@Z: s = @"Z"; break;
                case System.Windows.Forms.Keys.@LWin: s = @"LWin"; break;
                case System.Windows.Forms.Keys.@RWin: s = @"RWin"; break;
                case System.Windows.Forms.Keys.@Apps: s = @"Apps"; break;
                case System.Windows.Forms.Keys.@NumPad0: s = @"NumPad0"; break;
                case System.Windows.Forms.Keys.@NumPad1: s = @"NumPad1"; break;
                case System.Windows.Forms.Keys.@NumPad2: s = @"NumPad2"; break;
                case System.Windows.Forms.Keys.@NumPad3: s = @"NumPad3"; break;
                case System.Windows.Forms.Keys.@NumPad4: s = @"NumPad4"; break;
                case System.Windows.Forms.Keys.@NumPad5: s = @"NumPad5"; break;
                case System.Windows.Forms.Keys.@NumPad6: s = @"NumPad6"; break;
                case System.Windows.Forms.Keys.@NumPad7: s = @"NumPad7"; break;
                case System.Windows.Forms.Keys.@NumPad8: s = @"NumPad8"; break;
                case System.Windows.Forms.Keys.@NumPad9: s = @"NumPad9"; break;
                case System.Windows.Forms.Keys.@Multiply: s = @"Multiply"; break;
                case System.Windows.Forms.Keys.@Add: s = @"Add"; break;
                case System.Windows.Forms.Keys.@Separator: s = @"Separator"; break;
                case System.Windows.Forms.Keys.@Subtract: s = @"Subtract"; break;
                case System.Windows.Forms.Keys.@Decimal: s = @"Decimal"; break;
                case System.Windows.Forms.Keys.@Divide: s = @"Divide"; break;
                case System.Windows.Forms.Keys.@F1: s = @"F1"; break;
                case System.Windows.Forms.Keys.@F2: s = @"F2"; break;
                case System.Windows.Forms.Keys.@F3: s = @"F3"; break;
                case System.Windows.Forms.Keys.@F4: s = @"F4"; break;
                case System.Windows.Forms.Keys.@F5: s = @"F5"; break;
                case System.Windows.Forms.Keys.@F6: s = @"F6"; break;
                case System.Windows.Forms.Keys.@F7: s = @"F7"; break;
                case System.Windows.Forms.Keys.@F8: s = @"F8"; break;
                case System.Windows.Forms.Keys.@F9: s = @"F9"; break;
                case System.Windows.Forms.Keys.@F10: s = @"F10"; break;
                case System.Windows.Forms.Keys.@F11: s = @"F11"; break;
                case System.Windows.Forms.Keys.@F12: s = @"F12"; break;
                case System.Windows.Forms.Keys.@F13: s = @"F13"; break;
                case System.Windows.Forms.Keys.@F14: s = @"F14"; break;
                case System.Windows.Forms.Keys.@F15: s = @"F15"; break;
                case System.Windows.Forms.Keys.@F16: s = @"F16"; break;
                case System.Windows.Forms.Keys.@F17: s = @"F17"; break;
                case System.Windows.Forms.Keys.@F18: s = @"F18"; break;
                case System.Windows.Forms.Keys.@F19: s = @"F19"; break;
                case System.Windows.Forms.Keys.@F20: s = @"F20"; break;
                case System.Windows.Forms.Keys.@F21: s = @"F21"; break;
                case System.Windows.Forms.Keys.@F22: s = @"F22"; break;
                case System.Windows.Forms.Keys.@F23: s = @"F23"; break;
                case System.Windows.Forms.Keys.@F24: s = @"F24"; break;
                case System.Windows.Forms.Keys.@NumLock: s = @"NumLock"; break;
                case System.Windows.Forms.Keys.@Scroll: s = @"Scroll"; break;
                case System.Windows.Forms.Keys.@LShiftKey: s = @"LShiftKey"; break;
                case System.Windows.Forms.Keys.@RShiftKey: s = @"RShiftKey"; break;
                case System.Windows.Forms.Keys.@LControlKey: s = @"LControlKey"; break;
                case System.Windows.Forms.Keys.@RControlKey: s = @"RControlKey"; break;
                case System.Windows.Forms.Keys.@LMenu: s = @"LMenu"; break;
                case System.Windows.Forms.Keys.@RMenu: s = @"RMenu"; break;
                case System.Windows.Forms.Keys.@ProcessKey: s = @"ProcessKey"; break;
                case System.Windows.Forms.Keys.@Attn: s = @"Attn"; break;
                case System.Windows.Forms.Keys.@Crsel: s = @"Crsel"; break;
                case System.Windows.Forms.Keys.@Exsel: s = @"Exsel"; break;
                case System.Windows.Forms.Keys.@EraseEof: s = @"EraseEof"; break;
                case System.Windows.Forms.Keys.@Play: s = @"Play"; break;
                case System.Windows.Forms.Keys.@Zoom: s = @"Zoom"; break;
                case System.Windows.Forms.Keys.@NoName: s = @"NoName"; break;
                case System.Windows.Forms.Keys.@Pa1: s = @"Pa1"; break;
                case System.Windows.Forms.Keys.@OemClear: s = @"OemClear"; break;
                case System.Windows.Forms.Keys.@KanaMode: s = @"KanaMode"; break;
                case System.Windows.Forms.Keys.@JunjaMode: s = @"JunjaMode"; break;
                case System.Windows.Forms.Keys.@FinalMode: s = @"FinalMode"; break;
                case System.Windows.Forms.Keys.@HanjaMode: s = @"HanjaMode"; break;
                case System.Windows.Forms.Keys.@IMEConvert: s = @"IMEConvert"; break;
                case System.Windows.Forms.Keys.@IMENonconvert: s = @"IMENonconvert"; break;
                case System.Windows.Forms.Keys.@IMEAceept: s = @"IMEAceept"; break;
                case System.Windows.Forms.Keys.@IMEModeChange: s = @"IMEModeChange"; break;
                case System.Windows.Forms.Keys.@BrowserBack: s = @"BrowserBack"; break;
                case System.Windows.Forms.Keys.@BrowserForward: s = @"BrowserForward"; break;
                case System.Windows.Forms.Keys.@BrowserRefresh: s = @"BrowserRefresh"; break;
                case System.Windows.Forms.Keys.@BrowserStop: s = @"BrowserStop"; break;
                case System.Windows.Forms.Keys.@BrowserSearch: s = @"BrowserSearch"; break;
                case System.Windows.Forms.Keys.@BrowserFavorites: s = @"BrowserFavorites"; break;
                case System.Windows.Forms.Keys.@BrowserHome: s = @"BrowserHome"; break;
                case System.Windows.Forms.Keys.@VolumeMute: s = @"VolumeMute"; break;
                case System.Windows.Forms.Keys.@VolumeDown: s = @"VolumeDown"; break;
                case System.Windows.Forms.Keys.@VolumeUp: s = @"VolumeUp"; break;
                case System.Windows.Forms.Keys.@MediaNextTrack: s = @"MediaNextTrack"; break;
                case System.Windows.Forms.Keys.@MediaPreviousTrack: s = @"MediaPreviousTrack"; break;
                case System.Windows.Forms.Keys.@MediaStop: s = @"MediaStop"; break;
                case System.Windows.Forms.Keys.@MediaPlayPause: s = @"MediaPlayPause"; break;
                case System.Windows.Forms.Keys.@LaunchMail: s = @"LaunchMail"; break;
                case System.Windows.Forms.Keys.@SelectMedia: s = @"SelectMedia"; break;
                case System.Windows.Forms.Keys.@LaunchApplication1: s = @"LaunchApplication1"; break;
                case System.Windows.Forms.Keys.@LaunchApplication2: s = @"LaunchApplication2"; break;
                case System.Windows.Forms.Keys.@OemSemicolon: s = @"OemSemicolon"; break;
                case System.Windows.Forms.Keys.@Oemplus: s = @"Oemplus"; break;
                case System.Windows.Forms.Keys.@Oemcomma: s = @"Oemcomma"; break;
                case System.Windows.Forms.Keys.@OemMinus: s = @"OemMinus"; break;
                case System.Windows.Forms.Keys.@OemPeriod: s = @"OemPeriod"; break;
                case System.Windows.Forms.Keys.@OemQuestion: s = @"OemQuestion"; break;
                case System.Windows.Forms.Keys.@Oemtilde: s = @"Oemtilde"; break;
                case System.Windows.Forms.Keys.@OemOpenBrackets: s = @"OemOpenBrackets"; break;
                case System.Windows.Forms.Keys.@OemPipe: s = @"OemPipe"; break;
                case System.Windows.Forms.Keys.@OemCloseBrackets: s = @"OemCloseBrackets"; break;
                case System.Windows.Forms.Keys.@OemQuotes: s = @"OemQuotes"; break;
                case System.Windows.Forms.Keys.@Oem8: s = @"Oem8"; break;
                case System.Windows.Forms.Keys.@OemBackslash: s = @"OemBackslash"; break;
                case System.Windows.Forms.Keys.@Shift: s = @"Shift"; break;
                case System.Windows.Forms.Keys.@Control: s = @"Control"; break;
                case System.Windows.Forms.Keys.@Alt: s = @"Alt"; break;
                default: s = FromEnum((System.Int64)v, new System.String[] {@"KeyCode",
                    @"Modifiers",
                    @"None",
                    @"LButton",
                    @"RButton",
                    @"Cancel",
                    @"MButton",
                    @"XButton1",
                    @"XButton2",
                    @"Back",
                    @"Tab",
                    @"LineFeed",
                    @"Clear",
                    @"Return",
                    @"Enter",
                    @"ShiftKey",
                    @"ControlKey",
                    @"Menu",
                    @"Pause",
                    @"Capital",
                    @"CapsLock",
                    @"Escape",
                    @"Space",
                    @"Prior",
                    @"PageUp",
                    @"Next",
                    @"PageDown",
                    @"End",
                    @"Home",
                    @"Left",
                    @"Up",
                    @"Right",
                    @"Down",
                    @"Select",
                    @"Print",
                    @"Execute",
                    @"Snapshot",
                    @"PrintScreen",
                    @"Insert",
                    @"Delete",
                    @"Help",
                    @"D0",
                    @"D1",
                    @"D2",
                    @"D3",
                    @"D4",
                    @"D5",
                    @"D6",
                    @"D7",
                    @"D8",
                    @"D9",
                    @"A",
                    @"B",
                    @"C",
                    @"D",
                    @"E",
                    @"F",
                    @"G",
                    @"H",
                    @"I",
                    @"J",
                    @"K",
                    @"L",
                    @"M",
                    @"N",
                    @"O",
                    @"P",
                    @"Q",
                    @"R",
                    @"S",
                    @"T",
                    @"U",
                    @"V",
                    @"W",
                    @"X",
                    @"Y",
                    @"Z",
                    @"LWin",
                    @"RWin",
                    @"Apps",
                    @"NumPad0",
                    @"NumPad1",
                    @"NumPad2",
                    @"NumPad3",
                    @"NumPad4",
                    @"NumPad5",
                    @"NumPad6",
                    @"NumPad7",
                    @"NumPad8",
                    @"NumPad9",
                    @"Multiply",
                    @"Add",
                    @"Separator",
                    @"Subtract",
                    @"Decimal",
                    @"Divide",
                    @"F1",
                    @"F2",
                    @"F3",
                    @"F4",
                    @"F5",
                    @"F6",
                    @"F7",
                    @"F8",
                    @"F9",
                    @"F10",
                    @"F11",
                    @"F12",
                    @"F13",
                    @"F14",
                    @"F15",
                    @"F16",
                    @"F17",
                    @"F18",
                    @"F19",
                    @"F20",
                    @"F21",
                    @"F22",
                    @"F23",
                    @"F24",
                    @"NumLock",
                    @"Scroll",
                    @"LShiftKey",
                    @"RShiftKey",
                    @"LControlKey",
                    @"RControlKey",
                    @"LMenu",
                    @"RMenu",
                    @"ProcessKey",
                    @"Attn",
                    @"Crsel",
                    @"Exsel",
                    @"EraseEof",
                    @"Play",
                    @"Zoom",
                    @"NoName",
                    @"Pa1",
                    @"OemClear",
                    @"KanaMode",
                    @"HanguelMode",
                    @"HangulMode",
                    @"JunjaMode",
                    @"FinalMode",
                    @"HanjaMode",
                    @"KanjiMode",
                    @"IMEConvert",
                    @"IMENonconvert",
                    @"IMEAceept",
                    @"IMEModeChange",
                    @"BrowserBack",
                    @"BrowserForward",
                    @"BrowserRefresh",
                    @"BrowserStop",
                    @"BrowserSearch",
                    @"BrowserFavorites",
                    @"BrowserHome",
                    @"VolumeMute",
                    @"VolumeDown",
                    @"VolumeUp",
                    @"MediaNextTrack",
                    @"MediaPreviousTrack",
                    @"MediaStop",
                    @"MediaPlayPause",
                    @"LaunchMail",
                    @"SelectMedia",
                    @"LaunchApplication1",
                    @"LaunchApplication2",
                    @"OemSemicolon",
                    @"Oemplus",
                    @"Oemcomma",
                    @"OemMinus",
                    @"OemPeriod",
                    @"OemQuestion",
                    @"Oemtilde",
                    @"OemOpenBrackets",
                    @"OemPipe",
                    @"OemCloseBrackets",
                    @"OemQuotes",
                    @"Oem8",
                    @"OemBackslash",
                    @"Shift",
                    @"Control",
                    @"Alt"}, new System.Int64[] {(System.Int64)System.Windows.Forms.Keys.@KeyCode,
                    (System.Int64)System.Windows.Forms.Keys.@Modifiers,
                    (System.Int64)System.Windows.Forms.Keys.@None,
                    (System.Int64)System.Windows.Forms.Keys.@LButton,
                    (System.Int64)System.Windows.Forms.Keys.@RButton,
                    (System.Int64)System.Windows.Forms.Keys.@Cancel,
                    (System.Int64)System.Windows.Forms.Keys.@MButton,
                    (System.Int64)System.Windows.Forms.Keys.@XButton1,
                    (System.Int64)System.Windows.Forms.Keys.@XButton2,
                    (System.Int64)System.Windows.Forms.Keys.@Back,
                    (System.Int64)System.Windows.Forms.Keys.@Tab,
                    (System.Int64)System.Windows.Forms.Keys.@LineFeed,
                    (System.Int64)System.Windows.Forms.Keys.@Clear,
                    (System.Int64)System.Windows.Forms.Keys.@Return,
                    (System.Int64)System.Windows.Forms.Keys.@Enter,
                    (System.Int64)System.Windows.Forms.Keys.@ShiftKey,
                    (System.Int64)System.Windows.Forms.Keys.@ControlKey,
                    (System.Int64)System.Windows.Forms.Keys.@Menu,
                    (System.Int64)System.Windows.Forms.Keys.@Pause,
                    (System.Int64)System.Windows.Forms.Keys.@Capital,
                    (System.Int64)System.Windows.Forms.Keys.@CapsLock,
                    (System.Int64)System.Windows.Forms.Keys.@Escape,
                    (System.Int64)System.Windows.Forms.Keys.@Space,
                    (System.Int64)System.Windows.Forms.Keys.@Prior,
                    (System.Int64)System.Windows.Forms.Keys.@PageUp,
                    (System.Int64)System.Windows.Forms.Keys.@Next,
                    (System.Int64)System.Windows.Forms.Keys.@PageDown,
                    (System.Int64)System.Windows.Forms.Keys.@End,
                    (System.Int64)System.Windows.Forms.Keys.@Home,
                    (System.Int64)System.Windows.Forms.Keys.@Left,
                    (System.Int64)System.Windows.Forms.Keys.@Up,
                    (System.Int64)System.Windows.Forms.Keys.@Right,
                    (System.Int64)System.Windows.Forms.Keys.@Down,
                    (System.Int64)System.Windows.Forms.Keys.@Select,
                    (System.Int64)System.Windows.Forms.Keys.@Print,
                    (System.Int64)System.Windows.Forms.Keys.@Execute,
                    (System.Int64)System.Windows.Forms.Keys.@Snapshot,
                    (System.Int64)System.Windows.Forms.Keys.@PrintScreen,
                    (System.Int64)System.Windows.Forms.Keys.@Insert,
                    (System.Int64)System.Windows.Forms.Keys.@Delete,
                    (System.Int64)System.Windows.Forms.Keys.@Help,
                    (System.Int64)System.Windows.Forms.Keys.@D0,
                    (System.Int64)System.Windows.Forms.Keys.@D1,
                    (System.Int64)System.Windows.Forms.Keys.@D2,
                    (System.Int64)System.Windows.Forms.Keys.@D3,
                    (System.Int64)System.Windows.Forms.Keys.@D4,
                    (System.Int64)System.Windows.Forms.Keys.@D5,
                    (System.Int64)System.Windows.Forms.Keys.@D6,
                    (System.Int64)System.Windows.Forms.Keys.@D7,
                    (System.Int64)System.Windows.Forms.Keys.@D8,
                    (System.Int64)System.Windows.Forms.Keys.@D9,
                    (System.Int64)System.Windows.Forms.Keys.@A,
                    (System.Int64)System.Windows.Forms.Keys.@B,
                    (System.Int64)System.Windows.Forms.Keys.@C,
                    (System.Int64)System.Windows.Forms.Keys.@D,
                    (System.Int64)System.Windows.Forms.Keys.@E,
                    (System.Int64)System.Windows.Forms.Keys.@F,
                    (System.Int64)System.Windows.Forms.Keys.@G,
                    (System.Int64)System.Windows.Forms.Keys.@H,
                    (System.Int64)System.Windows.Forms.Keys.@I,
                    (System.Int64)System.Windows.Forms.Keys.@J,
                    (System.Int64)System.Windows.Forms.Keys.@K,
                    (System.Int64)System.Windows.Forms.Keys.@L,
                    (System.Int64)System.Windows.Forms.Keys.@M,
                    (System.Int64)System.Windows.Forms.Keys.@N,
                    (System.Int64)System.Windows.Forms.Keys.@O,
                    (System.Int64)System.Windows.Forms.Keys.@P,
                    (System.Int64)System.Windows.Forms.Keys.@Q,
                    (System.Int64)System.Windows.Forms.Keys.@R,
                    (System.Int64)System.Windows.Forms.Keys.@S,
                    (System.Int64)System.Windows.Forms.Keys.@T,
                    (System.Int64)System.Windows.Forms.Keys.@U,
                    (System.Int64)System.Windows.Forms.Keys.@V,
                    (System.Int64)System.Windows.Forms.Keys.@W,
                    (System.Int64)System.Windows.Forms.Keys.@X,
                    (System.Int64)System.Windows.Forms.Keys.@Y,
                    (System.Int64)System.Windows.Forms.Keys.@Z,
                    (System.Int64)System.Windows.Forms.Keys.@LWin,
                    (System.Int64)System.Windows.Forms.Keys.@RWin,
                    (System.Int64)System.Windows.Forms.Keys.@Apps,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad0,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad1,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad2,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad3,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad4,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad5,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad6,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad7,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad8,
                    (System.Int64)System.Windows.Forms.Keys.@NumPad9,
                    (System.Int64)System.Windows.Forms.Keys.@Multiply,
                    (System.Int64)System.Windows.Forms.Keys.@Add,
                    (System.Int64)System.Windows.Forms.Keys.@Separator,
                    (System.Int64)System.Windows.Forms.Keys.@Subtract,
                    (System.Int64)System.Windows.Forms.Keys.@Decimal,
                    (System.Int64)System.Windows.Forms.Keys.@Divide,
                    (System.Int64)System.Windows.Forms.Keys.@F1,
                    (System.Int64)System.Windows.Forms.Keys.@F2,
                    (System.Int64)System.Windows.Forms.Keys.@F3,
                    (System.Int64)System.Windows.Forms.Keys.@F4,
                    (System.Int64)System.Windows.Forms.Keys.@F5,
                    (System.Int64)System.Windows.Forms.Keys.@F6,
                    (System.Int64)System.Windows.Forms.Keys.@F7,
                    (System.Int64)System.Windows.Forms.Keys.@F8,
                    (System.Int64)System.Windows.Forms.Keys.@F9,
                    (System.Int64)System.Windows.Forms.Keys.@F10,
                    (System.Int64)System.Windows.Forms.Keys.@F11,
                    (System.Int64)System.Windows.Forms.Keys.@F12,
                    (System.Int64)System.Windows.Forms.Keys.@F13,
                    (System.Int64)System.Windows.Forms.Keys.@F14,
                    (System.Int64)System.Windows.Forms.Keys.@F15,
                    (System.Int64)System.Windows.Forms.Keys.@F16,
                    (System.Int64)System.Windows.Forms.Keys.@F17,
                    (System.Int64)System.Windows.Forms.Keys.@F18,
                    (System.Int64)System.Windows.Forms.Keys.@F19,
                    (System.Int64)System.Windows.Forms.Keys.@F20,
                    (System.Int64)System.Windows.Forms.Keys.@F21,
                    (System.Int64)System.Windows.Forms.Keys.@F22,
                    (System.Int64)System.Windows.Forms.Keys.@F23,
                    (System.Int64)System.Windows.Forms.Keys.@F24,
                    (System.Int64)System.Windows.Forms.Keys.@NumLock,
                    (System.Int64)System.Windows.Forms.Keys.@Scroll,
                    (System.Int64)System.Windows.Forms.Keys.@LShiftKey,
                    (System.Int64)System.Windows.Forms.Keys.@RShiftKey,
                    (System.Int64)System.Windows.Forms.Keys.@LControlKey,
                    (System.Int64)System.Windows.Forms.Keys.@RControlKey,
                    (System.Int64)System.Windows.Forms.Keys.@LMenu,
                    (System.Int64)System.Windows.Forms.Keys.@RMenu,
                    (System.Int64)System.Windows.Forms.Keys.@ProcessKey,
                    (System.Int64)System.Windows.Forms.Keys.@Attn,
                    (System.Int64)System.Windows.Forms.Keys.@Crsel,
                    (System.Int64)System.Windows.Forms.Keys.@Exsel,
                    (System.Int64)System.Windows.Forms.Keys.@EraseEof,
                    (System.Int64)System.Windows.Forms.Keys.@Play,
                    (System.Int64)System.Windows.Forms.Keys.@Zoom,
                    (System.Int64)System.Windows.Forms.Keys.@NoName,
                    (System.Int64)System.Windows.Forms.Keys.@Pa1,
                    (System.Int64)System.Windows.Forms.Keys.@OemClear,
                    (System.Int64)System.Windows.Forms.Keys.@KanaMode,
                    (System.Int64)System.Windows.Forms.Keys.@HanguelMode,
                    (System.Int64)System.Windows.Forms.Keys.@HangulMode,
                    (System.Int64)System.Windows.Forms.Keys.@JunjaMode,
                    (System.Int64)System.Windows.Forms.Keys.@FinalMode,
                    (System.Int64)System.Windows.Forms.Keys.@HanjaMode,
                    (System.Int64)System.Windows.Forms.Keys.@KanjiMode,
                    (System.Int64)System.Windows.Forms.Keys.@IMEConvert,
                    (System.Int64)System.Windows.Forms.Keys.@IMENonconvert,
                    (System.Int64)System.Windows.Forms.Keys.@IMEAceept,
                    (System.Int64)System.Windows.Forms.Keys.@IMEModeChange,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserBack,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserForward,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserRefresh,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserStop,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserSearch,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserFavorites,
                    (System.Int64)System.Windows.Forms.Keys.@BrowserHome,
                    (System.Int64)System.Windows.Forms.Keys.@VolumeMute,
                    (System.Int64)System.Windows.Forms.Keys.@VolumeDown,
                    (System.Int64)System.Windows.Forms.Keys.@VolumeUp,
                    (System.Int64)System.Windows.Forms.Keys.@MediaNextTrack,
                    (System.Int64)System.Windows.Forms.Keys.@MediaPreviousTrack,
                    (System.Int64)System.Windows.Forms.Keys.@MediaStop,
                    (System.Int64)System.Windows.Forms.Keys.@MediaPlayPause,
                    (System.Int64)System.Windows.Forms.Keys.@LaunchMail,
                    (System.Int64)System.Windows.Forms.Keys.@SelectMedia,
                    (System.Int64)System.Windows.Forms.Keys.@LaunchApplication1,
                    (System.Int64)System.Windows.Forms.Keys.@LaunchApplication2,
                    (System.Int64)System.Windows.Forms.Keys.@OemSemicolon,
                    (System.Int64)System.Windows.Forms.Keys.@Oemplus,
                    (System.Int64)System.Windows.Forms.Keys.@Oemcomma,
                    (System.Int64)System.Windows.Forms.Keys.@OemMinus,
                    (System.Int64)System.Windows.Forms.Keys.@OemPeriod,
                    (System.Int64)System.Windows.Forms.Keys.@OemQuestion,
                    (System.Int64)System.Windows.Forms.Keys.@Oemtilde,
                    (System.Int64)System.Windows.Forms.Keys.@OemOpenBrackets,
                    (System.Int64)System.Windows.Forms.Keys.@OemPipe,
                    (System.Int64)System.Windows.Forms.Keys.@OemCloseBrackets,
                    (System.Int64)System.Windows.Forms.Keys.@OemQuotes,
                    (System.Int64)System.Windows.Forms.Keys.@Oem8,
                    (System.Int64)System.Windows.Forms.Keys.@OemBackslash,
                    (System.Int64)System.Windows.Forms.Keys.@Shift,
                    (System.Int64)System.Windows.Forms.Keys.@Control,
                    (System.Int64)System.Windows.Forms.Keys.@Alt}); break;
            }
            return s;
        }

        protected override void InitCallbacks() {
        }

        protected void Write5_TOptions(object o) {
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
		protected override System.Windows.Forms.Keys Read4_Keys(string s)
		{
			System.Windows.Forms.Keys obj = base.Read4_Keys(s);
			KeysDeserializedHandler handler = KeysDeserialized;
			if (handler != null)
				handler(obj);

			return obj;
		}

		/// <summary>Reads an object of type Options.TOptions.</summary>
		internal Options.TOptions Read()
		{
			return (Options.TOptions) Read6_TOptions();
		}
        public event TOptionsDeserializedHandler TOptionsDeserialized;

        public event TSettingsObjectDeserializedHandler TSettingsObjectDeserialized;

        public event KeysDeserializedHandler KeysDeserialized;
    }

    public delegate void TOptionsDeserializedHandler(Options.TOptions toptions);

    public delegate void TSettingsObjectDeserializedHandler(Commons.TSettingsObject tsettingsobject);

    public delegate void KeysDeserializedHandler(System.Windows.Forms.Keys keys);

    public class TOptionsWriter : TOptionsSerializer.Writer
    {


		/// <summary>Writes an object of type Options.TOptions.</summary>
		internal void Write(Options.TOptions obj)
		{
			Write5_TOptions(obj);
		}}
}
