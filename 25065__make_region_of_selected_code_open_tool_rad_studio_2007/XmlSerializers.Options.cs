#if _DYNAMIC_XMLSERIALIZER_COMPILATION
[assembly:System.Security.AllowPartiallyTrustedCallers()]
[assembly:System.Security.SecurityTransparent()]
#endif
[assembly:System.Reflection.AssemblyVersionAttribute("4.0.2822.33845")]
[assembly:System.Xml.Serialization.XmlSerializerVersionAttribute(ParentAssemblyId=@"147895ac-120b-4c4c-897c-429eead1affa,", Version=@"2.0.0.0")]
namespace XmlSerializers.Options {

    public class XmlSerializationWriterTOptions : System.Xml.Serialization.XmlSerializationWriter {

        public void Write5_TOptions(object o) {
            WriteStartDocument();
            if (o == null) {
                WriteNullTagLiteral(@"TOptions", @"");
                return;
            }
            TopLevelElement();
            Write4_TOptions(@"TOptions", @"", ((global::Options.TOptions)o), true, false);
        }

        void Write4_TOptions(string n, string ns, global::Options.TOptions o, bool isNullable, bool needType) {
            if ((object)o == null) {
                if (isNullable) WriteNullTagLiteral(n, ns);
                return;
            }
            if (!needType) {
                System.Type t = o.GetType();
                if (t == typeof(global::Options.TOptions)) {
                }
                else {
                    throw CreateUnknownTypeException(o);
                }
            }
            WriteStartElement(n, ns, o, false, null);
            if (needType) WriteXsiType(@"TOptions", @"");
            WriteElementString(@"ItemRemoveShortcut", @"", Write3_Keys(((global::System.Windows.Forms.Keys)o.@ItemRemoveShortcut)));
            WriteElementString(@"ItemAddShortcut", @"", Write3_Keys(((global::System.Windows.Forms.Keys)o.@ItemAddShortcut)));
            WriteEndElement(o);
        }

        string Write3_Keys(global::System.Windows.Forms.Keys v) {
            string s = null;
            switch (v) {
                case global::System.Windows.Forms.Keys.@KeyCode: s = @"KeyCode"; break;
                case global::System.Windows.Forms.Keys.@Modifiers: s = @"Modifiers"; break;
                case global::System.Windows.Forms.Keys.@None: s = @"None"; break;
                case global::System.Windows.Forms.Keys.@LButton: s = @"LButton"; break;
                case global::System.Windows.Forms.Keys.@RButton: s = @"RButton"; break;
                case global::System.Windows.Forms.Keys.@Cancel: s = @"Cancel"; break;
                case global::System.Windows.Forms.Keys.@MButton: s = @"MButton"; break;
                case global::System.Windows.Forms.Keys.@XButton1: s = @"XButton1"; break;
                case global::System.Windows.Forms.Keys.@XButton2: s = @"XButton2"; break;
                case global::System.Windows.Forms.Keys.@Back: s = @"Back"; break;
                case global::System.Windows.Forms.Keys.@Tab: s = @"Tab"; break;
                case global::System.Windows.Forms.Keys.@LineFeed: s = @"LineFeed"; break;
                case global::System.Windows.Forms.Keys.@Clear: s = @"Clear"; break;
                case global::System.Windows.Forms.Keys.@Return: s = @"Return"; break;
                case global::System.Windows.Forms.Keys.@ShiftKey: s = @"ShiftKey"; break;
                case global::System.Windows.Forms.Keys.@ControlKey: s = @"ControlKey"; break;
                case global::System.Windows.Forms.Keys.@Menu: s = @"Menu"; break;
                case global::System.Windows.Forms.Keys.@Pause: s = @"Pause"; break;
                case global::System.Windows.Forms.Keys.@Capital: s = @"Capital"; break;
                case global::System.Windows.Forms.Keys.@KanaMode: s = @"KanaMode"; break;
                case global::System.Windows.Forms.Keys.@JunjaMode: s = @"JunjaMode"; break;
                case global::System.Windows.Forms.Keys.@FinalMode: s = @"FinalMode"; break;
                case global::System.Windows.Forms.Keys.@HanjaMode: s = @"HanjaMode"; break;
                case global::System.Windows.Forms.Keys.@Escape: s = @"Escape"; break;
                case global::System.Windows.Forms.Keys.@IMEConvert: s = @"IMEConvert"; break;
                case global::System.Windows.Forms.Keys.@IMENonconvert: s = @"IMENonconvert"; break;
                case global::System.Windows.Forms.Keys.@IMEAccept: s = @"IMEAccept"; break;
                case global::System.Windows.Forms.Keys.@IMEModeChange: s = @"IMEModeChange"; break;
                case global::System.Windows.Forms.Keys.@Space: s = @"Space"; break;
                case global::System.Windows.Forms.Keys.@Prior: s = @"Prior"; break;
                case global::System.Windows.Forms.Keys.@Next: s = @"Next"; break;
                case global::System.Windows.Forms.Keys.@End: s = @"End"; break;
                case global::System.Windows.Forms.Keys.@Home: s = @"Home"; break;
                case global::System.Windows.Forms.Keys.@Left: s = @"Left"; break;
                case global::System.Windows.Forms.Keys.@Up: s = @"Up"; break;
                case global::System.Windows.Forms.Keys.@Right: s = @"Right"; break;
                case global::System.Windows.Forms.Keys.@Down: s = @"Down"; break;
                case global::System.Windows.Forms.Keys.@Select: s = @"Select"; break;
                case global::System.Windows.Forms.Keys.@Print: s = @"Print"; break;
                case global::System.Windows.Forms.Keys.@Execute: s = @"Execute"; break;
                case global::System.Windows.Forms.Keys.@Snapshot: s = @"Snapshot"; break;
                case global::System.Windows.Forms.Keys.@Insert: s = @"Insert"; break;
                case global::System.Windows.Forms.Keys.@Delete: s = @"Delete"; break;
                case global::System.Windows.Forms.Keys.@Help: s = @"Help"; break;
                case global::System.Windows.Forms.Keys.@D0: s = @"D0"; break;
                case global::System.Windows.Forms.Keys.@D1: s = @"D1"; break;
                case global::System.Windows.Forms.Keys.@D2: s = @"D2"; break;
                case global::System.Windows.Forms.Keys.@D3: s = @"D3"; break;
                case global::System.Windows.Forms.Keys.@D4: s = @"D4"; break;
                case global::System.Windows.Forms.Keys.@D5: s = @"D5"; break;
                case global::System.Windows.Forms.Keys.@D6: s = @"D6"; break;
                case global::System.Windows.Forms.Keys.@D7: s = @"D7"; break;
                case global::System.Windows.Forms.Keys.@D8: s = @"D8"; break;
                case global::System.Windows.Forms.Keys.@D9: s = @"D9"; break;
                case global::System.Windows.Forms.Keys.@A: s = @"A"; break;
                case global::System.Windows.Forms.Keys.@B: s = @"B"; break;
                case global::System.Windows.Forms.Keys.@C: s = @"C"; break;
                case global::System.Windows.Forms.Keys.@D: s = @"D"; break;
                case global::System.Windows.Forms.Keys.@E: s = @"E"; break;
                case global::System.Windows.Forms.Keys.@F: s = @"F"; break;
                case global::System.Windows.Forms.Keys.@G: s = @"G"; break;
                case global::System.Windows.Forms.Keys.@H: s = @"H"; break;
                case global::System.Windows.Forms.Keys.@I: s = @"I"; break;
                case global::System.Windows.Forms.Keys.@J: s = @"J"; break;
                case global::System.Windows.Forms.Keys.@K: s = @"K"; break;
                case global::System.Windows.Forms.Keys.@L: s = @"L"; break;
                case global::System.Windows.Forms.Keys.@M: s = @"M"; break;
                case global::System.Windows.Forms.Keys.@N: s = @"N"; break;
                case global::System.Windows.Forms.Keys.@O: s = @"O"; break;
                case global::System.Windows.Forms.Keys.@P: s = @"P"; break;
                case global::System.Windows.Forms.Keys.@Q: s = @"Q"; break;
                case global::System.Windows.Forms.Keys.@R: s = @"R"; break;
                case global::System.Windows.Forms.Keys.@S: s = @"S"; break;
                case global::System.Windows.Forms.Keys.@T: s = @"T"; break;
                case global::System.Windows.Forms.Keys.@U: s = @"U"; break;
                case global::System.Windows.Forms.Keys.@V: s = @"V"; break;
                case global::System.Windows.Forms.Keys.@W: s = @"W"; break;
                case global::System.Windows.Forms.Keys.@X: s = @"X"; break;
                case global::System.Windows.Forms.Keys.@Y: s = @"Y"; break;
                case global::System.Windows.Forms.Keys.@Z: s = @"Z"; break;
                case global::System.Windows.Forms.Keys.@LWin: s = @"LWin"; break;
                case global::System.Windows.Forms.Keys.@RWin: s = @"RWin"; break;
                case global::System.Windows.Forms.Keys.@Apps: s = @"Apps"; break;
                case global::System.Windows.Forms.Keys.@Sleep: s = @"Sleep"; break;
                case global::System.Windows.Forms.Keys.@NumPad0: s = @"NumPad0"; break;
                case global::System.Windows.Forms.Keys.@NumPad1: s = @"NumPad1"; break;
                case global::System.Windows.Forms.Keys.@NumPad2: s = @"NumPad2"; break;
                case global::System.Windows.Forms.Keys.@NumPad3: s = @"NumPad3"; break;
                case global::System.Windows.Forms.Keys.@NumPad4: s = @"NumPad4"; break;
                case global::System.Windows.Forms.Keys.@NumPad5: s = @"NumPad5"; break;
                case global::System.Windows.Forms.Keys.@NumPad6: s = @"NumPad6"; break;
                case global::System.Windows.Forms.Keys.@NumPad7: s = @"NumPad7"; break;
                case global::System.Windows.Forms.Keys.@NumPad8: s = @"NumPad8"; break;
                case global::System.Windows.Forms.Keys.@NumPad9: s = @"NumPad9"; break;
                case global::System.Windows.Forms.Keys.@Multiply: s = @"Multiply"; break;
                case global::System.Windows.Forms.Keys.@Add: s = @"Add"; break;
                case global::System.Windows.Forms.Keys.@Separator: s = @"Separator"; break;
                case global::System.Windows.Forms.Keys.@Subtract: s = @"Subtract"; break;
                case global::System.Windows.Forms.Keys.@Decimal: s = @"Decimal"; break;
                case global::System.Windows.Forms.Keys.@Divide: s = @"Divide"; break;
                case global::System.Windows.Forms.Keys.@F1: s = @"F1"; break;
                case global::System.Windows.Forms.Keys.@F2: s = @"F2"; break;
                case global::System.Windows.Forms.Keys.@F3: s = @"F3"; break;
                case global::System.Windows.Forms.Keys.@F4: s = @"F4"; break;
                case global::System.Windows.Forms.Keys.@F5: s = @"F5"; break;
                case global::System.Windows.Forms.Keys.@F6: s = @"F6"; break;
                case global::System.Windows.Forms.Keys.@F7: s = @"F7"; break;
                case global::System.Windows.Forms.Keys.@F8: s = @"F8"; break;
                case global::System.Windows.Forms.Keys.@F9: s = @"F9"; break;
                case global::System.Windows.Forms.Keys.@F10: s = @"F10"; break;
                case global::System.Windows.Forms.Keys.@F11: s = @"F11"; break;
                case global::System.Windows.Forms.Keys.@F12: s = @"F12"; break;
                case global::System.Windows.Forms.Keys.@F13: s = @"F13"; break;
                case global::System.Windows.Forms.Keys.@F14: s = @"F14"; break;
                case global::System.Windows.Forms.Keys.@F15: s = @"F15"; break;
                case global::System.Windows.Forms.Keys.@F16: s = @"F16"; break;
                case global::System.Windows.Forms.Keys.@F17: s = @"F17"; break;
                case global::System.Windows.Forms.Keys.@F18: s = @"F18"; break;
                case global::System.Windows.Forms.Keys.@F19: s = @"F19"; break;
                case global::System.Windows.Forms.Keys.@F20: s = @"F20"; break;
                case global::System.Windows.Forms.Keys.@F21: s = @"F21"; break;
                case global::System.Windows.Forms.Keys.@F22: s = @"F22"; break;
                case global::System.Windows.Forms.Keys.@F23: s = @"F23"; break;
                case global::System.Windows.Forms.Keys.@F24: s = @"F24"; break;
                case global::System.Windows.Forms.Keys.@NumLock: s = @"NumLock"; break;
                case global::System.Windows.Forms.Keys.@Scroll: s = @"Scroll"; break;
                case global::System.Windows.Forms.Keys.@LShiftKey: s = @"LShiftKey"; break;
                case global::System.Windows.Forms.Keys.@RShiftKey: s = @"RShiftKey"; break;
                case global::System.Windows.Forms.Keys.@LControlKey: s = @"LControlKey"; break;
                case global::System.Windows.Forms.Keys.@RControlKey: s = @"RControlKey"; break;
                case global::System.Windows.Forms.Keys.@LMenu: s = @"LMenu"; break;
                case global::System.Windows.Forms.Keys.@RMenu: s = @"RMenu"; break;
                case global::System.Windows.Forms.Keys.@BrowserBack: s = @"BrowserBack"; break;
                case global::System.Windows.Forms.Keys.@BrowserForward: s = @"BrowserForward"; break;
                case global::System.Windows.Forms.Keys.@BrowserRefresh: s = @"BrowserRefresh"; break;
                case global::System.Windows.Forms.Keys.@BrowserStop: s = @"BrowserStop"; break;
                case global::System.Windows.Forms.Keys.@BrowserSearch: s = @"BrowserSearch"; break;
                case global::System.Windows.Forms.Keys.@BrowserFavorites: s = @"BrowserFavorites"; break;
                case global::System.Windows.Forms.Keys.@BrowserHome: s = @"BrowserHome"; break;
                case global::System.Windows.Forms.Keys.@VolumeMute: s = @"VolumeMute"; break;
                case global::System.Windows.Forms.Keys.@VolumeDown: s = @"VolumeDown"; break;
                case global::System.Windows.Forms.Keys.@VolumeUp: s = @"VolumeUp"; break;
                case global::System.Windows.Forms.Keys.@MediaNextTrack: s = @"MediaNextTrack"; break;
                case global::System.Windows.Forms.Keys.@MediaPreviousTrack: s = @"MediaPreviousTrack"; break;
                case global::System.Windows.Forms.Keys.@MediaStop: s = @"MediaStop"; break;
                case global::System.Windows.Forms.Keys.@MediaPlayPause: s = @"MediaPlayPause"; break;
                case global::System.Windows.Forms.Keys.@LaunchMail: s = @"LaunchMail"; break;
                case global::System.Windows.Forms.Keys.@SelectMedia: s = @"SelectMedia"; break;
                case global::System.Windows.Forms.Keys.@LaunchApplication1: s = @"LaunchApplication1"; break;
                case global::System.Windows.Forms.Keys.@LaunchApplication2: s = @"LaunchApplication2"; break;
                case global::System.Windows.Forms.Keys.@OemSemicolon: s = @"OemSemicolon"; break;
                case global::System.Windows.Forms.Keys.@Oemplus: s = @"Oemplus"; break;
                case global::System.Windows.Forms.Keys.@Oemcomma: s = @"Oemcomma"; break;
                case global::System.Windows.Forms.Keys.@OemMinus: s = @"OemMinus"; break;
                case global::System.Windows.Forms.Keys.@OemPeriod: s = @"OemPeriod"; break;
                case global::System.Windows.Forms.Keys.@OemQuestion: s = @"OemQuestion"; break;
                case global::System.Windows.Forms.Keys.@Oemtilde: s = @"Oemtilde"; break;
                case global::System.Windows.Forms.Keys.@OemOpenBrackets: s = @"OemOpenBrackets"; break;
                case global::System.Windows.Forms.Keys.@OemPipe: s = @"OemPipe"; break;
                case global::System.Windows.Forms.Keys.@OemCloseBrackets: s = @"OemCloseBrackets"; break;
                case global::System.Windows.Forms.Keys.@OemQuotes: s = @"OemQuotes"; break;
                case global::System.Windows.Forms.Keys.@Oem8: s = @"Oem8"; break;
                case global::System.Windows.Forms.Keys.@OemBackslash: s = @"OemBackslash"; break;
                case global::System.Windows.Forms.Keys.@ProcessKey: s = @"ProcessKey"; break;
                case global::System.Windows.Forms.Keys.@Packet: s = @"Packet"; break;
                case global::System.Windows.Forms.Keys.@Attn: s = @"Attn"; break;
                case global::System.Windows.Forms.Keys.@Crsel: s = @"Crsel"; break;
                case global::System.Windows.Forms.Keys.@Exsel: s = @"Exsel"; break;
                case global::System.Windows.Forms.Keys.@EraseEof: s = @"EraseEof"; break;
                case global::System.Windows.Forms.Keys.@Play: s = @"Play"; break;
                case global::System.Windows.Forms.Keys.@Zoom: s = @"Zoom"; break;
                case global::System.Windows.Forms.Keys.@NoName: s = @"NoName"; break;
                case global::System.Windows.Forms.Keys.@Pa1: s = @"Pa1"; break;
                case global::System.Windows.Forms.Keys.@OemClear: s = @"OemClear"; break;
                case global::System.Windows.Forms.Keys.@Shift: s = @"Shift"; break;
                case global::System.Windows.Forms.Keys.@Control: s = @"Control"; break;
                case global::System.Windows.Forms.Keys.@Alt: s = @"Alt"; break;
                default: s = FromEnum(((System.Int64)v), new string[] {@"KeyCode",
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
                    @"KanaMode",
                    @"HanguelMode",
                    @"HangulMode",
                    @"JunjaMode",
                    @"FinalMode",
                    @"HanjaMode",
                    @"KanjiMode",
                    @"Escape",
                    @"IMEConvert",
                    @"IMENonconvert",
                    @"IMEAccept",
                    @"IMEAceept",
                    @"IMEModeChange",
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
                    @"Sleep",
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
                    @"Oem1",
                    @"Oemplus",
                    @"Oemcomma",
                    @"OemMinus",
                    @"OemPeriod",
                    @"OemQuestion",
                    @"Oem2",
                    @"Oemtilde",
                    @"Oem3",
                    @"OemOpenBrackets",
                    @"Oem4",
                    @"OemPipe",
                    @"Oem5",
                    @"OemCloseBrackets",
                    @"Oem6",
                    @"OemQuotes",
                    @"Oem7",
                    @"Oem8",
                    @"OemBackslash",
                    @"Oem102",
                    @"ProcessKey",
                    @"Packet",
                    @"Attn",
                    @"Crsel",
                    @"Exsel",
                    @"EraseEof",
                    @"Play",
                    @"Zoom",
                    @"NoName",
                    @"Pa1",
                    @"OemClear",
                    @"Shift",
                    @"Control",
                    @"Alt"}, new System.Int64[] {(long)global::System.Windows.Forms.Keys.@KeyCode,
                    (long)global::System.Windows.Forms.Keys.@Modifiers,
                    (long)global::System.Windows.Forms.Keys.@None,
                    (long)global::System.Windows.Forms.Keys.@LButton,
                    (long)global::System.Windows.Forms.Keys.@RButton,
                    (long)global::System.Windows.Forms.Keys.@Cancel,
                    (long)global::System.Windows.Forms.Keys.@MButton,
                    (long)global::System.Windows.Forms.Keys.@XButton1,
                    (long)global::System.Windows.Forms.Keys.@XButton2,
                    (long)global::System.Windows.Forms.Keys.@Back,
                    (long)global::System.Windows.Forms.Keys.@Tab,
                    (long)global::System.Windows.Forms.Keys.@LineFeed,
                    (long)global::System.Windows.Forms.Keys.@Clear,
                    (long)global::System.Windows.Forms.Keys.@Return,
                    (long)global::System.Windows.Forms.Keys.@Enter,
                    (long)global::System.Windows.Forms.Keys.@ShiftKey,
                    (long)global::System.Windows.Forms.Keys.@ControlKey,
                    (long)global::System.Windows.Forms.Keys.@Menu,
                    (long)global::System.Windows.Forms.Keys.@Pause,
                    (long)global::System.Windows.Forms.Keys.@Capital,
                    (long)global::System.Windows.Forms.Keys.@CapsLock,
                    (long)global::System.Windows.Forms.Keys.@KanaMode,
                    (long)global::System.Windows.Forms.Keys.@HanguelMode,
                    (long)global::System.Windows.Forms.Keys.@HangulMode,
                    (long)global::System.Windows.Forms.Keys.@JunjaMode,
                    (long)global::System.Windows.Forms.Keys.@FinalMode,
                    (long)global::System.Windows.Forms.Keys.@HanjaMode,
                    (long)global::System.Windows.Forms.Keys.@KanjiMode,
                    (long)global::System.Windows.Forms.Keys.@Escape,
                    (long)global::System.Windows.Forms.Keys.@IMEConvert,
                    (long)global::System.Windows.Forms.Keys.@IMENonconvert,
                    (long)global::System.Windows.Forms.Keys.@IMEAccept,
                    (long)global::System.Windows.Forms.Keys.@IMEAceept,
                    (long)global::System.Windows.Forms.Keys.@IMEModeChange,
                    (long)global::System.Windows.Forms.Keys.@Space,
                    (long)global::System.Windows.Forms.Keys.@Prior,
                    (long)global::System.Windows.Forms.Keys.@PageUp,
                    (long)global::System.Windows.Forms.Keys.@Next,
                    (long)global::System.Windows.Forms.Keys.@PageDown,
                    (long)global::System.Windows.Forms.Keys.@End,
                    (long)global::System.Windows.Forms.Keys.@Home,
                    (long)global::System.Windows.Forms.Keys.@Left,
                    (long)global::System.Windows.Forms.Keys.@Up,
                    (long)global::System.Windows.Forms.Keys.@Right,
                    (long)global::System.Windows.Forms.Keys.@Down,
                    (long)global::System.Windows.Forms.Keys.@Select,
                    (long)global::System.Windows.Forms.Keys.@Print,
                    (long)global::System.Windows.Forms.Keys.@Execute,
                    (long)global::System.Windows.Forms.Keys.@Snapshot,
                    (long)global::System.Windows.Forms.Keys.@PrintScreen,
                    (long)global::System.Windows.Forms.Keys.@Insert,
                    (long)global::System.Windows.Forms.Keys.@Delete,
                    (long)global::System.Windows.Forms.Keys.@Help,
                    (long)global::System.Windows.Forms.Keys.@D0,
                    (long)global::System.Windows.Forms.Keys.@D1,
                    (long)global::System.Windows.Forms.Keys.@D2,
                    (long)global::System.Windows.Forms.Keys.@D3,
                    (long)global::System.Windows.Forms.Keys.@D4,
                    (long)global::System.Windows.Forms.Keys.@D5,
                    (long)global::System.Windows.Forms.Keys.@D6,
                    (long)global::System.Windows.Forms.Keys.@D7,
                    (long)global::System.Windows.Forms.Keys.@D8,
                    (long)global::System.Windows.Forms.Keys.@D9,
                    (long)global::System.Windows.Forms.Keys.@A,
                    (long)global::System.Windows.Forms.Keys.@B,
                    (long)global::System.Windows.Forms.Keys.@C,
                    (long)global::System.Windows.Forms.Keys.@D,
                    (long)global::System.Windows.Forms.Keys.@E,
                    (long)global::System.Windows.Forms.Keys.@F,
                    (long)global::System.Windows.Forms.Keys.@G,
                    (long)global::System.Windows.Forms.Keys.@H,
                    (long)global::System.Windows.Forms.Keys.@I,
                    (long)global::System.Windows.Forms.Keys.@J,
                    (long)global::System.Windows.Forms.Keys.@K,
                    (long)global::System.Windows.Forms.Keys.@L,
                    (long)global::System.Windows.Forms.Keys.@M,
                    (long)global::System.Windows.Forms.Keys.@N,
                    (long)global::System.Windows.Forms.Keys.@O,
                    (long)global::System.Windows.Forms.Keys.@P,
                    (long)global::System.Windows.Forms.Keys.@Q,
                    (long)global::System.Windows.Forms.Keys.@R,
                    (long)global::System.Windows.Forms.Keys.@S,
                    (long)global::System.Windows.Forms.Keys.@T,
                    (long)global::System.Windows.Forms.Keys.@U,
                    (long)global::System.Windows.Forms.Keys.@V,
                    (long)global::System.Windows.Forms.Keys.@W,
                    (long)global::System.Windows.Forms.Keys.@X,
                    (long)global::System.Windows.Forms.Keys.@Y,
                    (long)global::System.Windows.Forms.Keys.@Z,
                    (long)global::System.Windows.Forms.Keys.@LWin,
                    (long)global::System.Windows.Forms.Keys.@RWin,
                    (long)global::System.Windows.Forms.Keys.@Apps,
                    (long)global::System.Windows.Forms.Keys.@Sleep,
                    (long)global::System.Windows.Forms.Keys.@NumPad0,
                    (long)global::System.Windows.Forms.Keys.@NumPad1,
                    (long)global::System.Windows.Forms.Keys.@NumPad2,
                    (long)global::System.Windows.Forms.Keys.@NumPad3,
                    (long)global::System.Windows.Forms.Keys.@NumPad4,
                    (long)global::System.Windows.Forms.Keys.@NumPad5,
                    (long)global::System.Windows.Forms.Keys.@NumPad6,
                    (long)global::System.Windows.Forms.Keys.@NumPad7,
                    (long)global::System.Windows.Forms.Keys.@NumPad8,
                    (long)global::System.Windows.Forms.Keys.@NumPad9,
                    (long)global::System.Windows.Forms.Keys.@Multiply,
                    (long)global::System.Windows.Forms.Keys.@Add,
                    (long)global::System.Windows.Forms.Keys.@Separator,
                    (long)global::System.Windows.Forms.Keys.@Subtract,
                    (long)global::System.Windows.Forms.Keys.@Decimal,
                    (long)global::System.Windows.Forms.Keys.@Divide,
                    (long)global::System.Windows.Forms.Keys.@F1,
                    (long)global::System.Windows.Forms.Keys.@F2,
                    (long)global::System.Windows.Forms.Keys.@F3,
                    (long)global::System.Windows.Forms.Keys.@F4,
                    (long)global::System.Windows.Forms.Keys.@F5,
                    (long)global::System.Windows.Forms.Keys.@F6,
                    (long)global::System.Windows.Forms.Keys.@F7,
                    (long)global::System.Windows.Forms.Keys.@F8,
                    (long)global::System.Windows.Forms.Keys.@F9,
                    (long)global::System.Windows.Forms.Keys.@F10,
                    (long)global::System.Windows.Forms.Keys.@F11,
                    (long)global::System.Windows.Forms.Keys.@F12,
                    (long)global::System.Windows.Forms.Keys.@F13,
                    (long)global::System.Windows.Forms.Keys.@F14,
                    (long)global::System.Windows.Forms.Keys.@F15,
                    (long)global::System.Windows.Forms.Keys.@F16,
                    (long)global::System.Windows.Forms.Keys.@F17,
                    (long)global::System.Windows.Forms.Keys.@F18,
                    (long)global::System.Windows.Forms.Keys.@F19,
                    (long)global::System.Windows.Forms.Keys.@F20,
                    (long)global::System.Windows.Forms.Keys.@F21,
                    (long)global::System.Windows.Forms.Keys.@F22,
                    (long)global::System.Windows.Forms.Keys.@F23,
                    (long)global::System.Windows.Forms.Keys.@F24,
                    (long)global::System.Windows.Forms.Keys.@NumLock,
                    (long)global::System.Windows.Forms.Keys.@Scroll,
                    (long)global::System.Windows.Forms.Keys.@LShiftKey,
                    (long)global::System.Windows.Forms.Keys.@RShiftKey,
                    (long)global::System.Windows.Forms.Keys.@LControlKey,
                    (long)global::System.Windows.Forms.Keys.@RControlKey,
                    (long)global::System.Windows.Forms.Keys.@LMenu,
                    (long)global::System.Windows.Forms.Keys.@RMenu,
                    (long)global::System.Windows.Forms.Keys.@BrowserBack,
                    (long)global::System.Windows.Forms.Keys.@BrowserForward,
                    (long)global::System.Windows.Forms.Keys.@BrowserRefresh,
                    (long)global::System.Windows.Forms.Keys.@BrowserStop,
                    (long)global::System.Windows.Forms.Keys.@BrowserSearch,
                    (long)global::System.Windows.Forms.Keys.@BrowserFavorites,
                    (long)global::System.Windows.Forms.Keys.@BrowserHome,
                    (long)global::System.Windows.Forms.Keys.@VolumeMute,
                    (long)global::System.Windows.Forms.Keys.@VolumeDown,
                    (long)global::System.Windows.Forms.Keys.@VolumeUp,
                    (long)global::System.Windows.Forms.Keys.@MediaNextTrack,
                    (long)global::System.Windows.Forms.Keys.@MediaPreviousTrack,
                    (long)global::System.Windows.Forms.Keys.@MediaStop,
                    (long)global::System.Windows.Forms.Keys.@MediaPlayPause,
                    (long)global::System.Windows.Forms.Keys.@LaunchMail,
                    (long)global::System.Windows.Forms.Keys.@SelectMedia,
                    (long)global::System.Windows.Forms.Keys.@LaunchApplication1,
                    (long)global::System.Windows.Forms.Keys.@LaunchApplication2,
                    (long)global::System.Windows.Forms.Keys.@OemSemicolon,
                    (long)global::System.Windows.Forms.Keys.@Oem1,
                    (long)global::System.Windows.Forms.Keys.@Oemplus,
                    (long)global::System.Windows.Forms.Keys.@Oemcomma,
                    (long)global::System.Windows.Forms.Keys.@OemMinus,
                    (long)global::System.Windows.Forms.Keys.@OemPeriod,
                    (long)global::System.Windows.Forms.Keys.@OemQuestion,
                    (long)global::System.Windows.Forms.Keys.@Oem2,
                    (long)global::System.Windows.Forms.Keys.@Oemtilde,
                    (long)global::System.Windows.Forms.Keys.@Oem3,
                    (long)global::System.Windows.Forms.Keys.@OemOpenBrackets,
                    (long)global::System.Windows.Forms.Keys.@Oem4,
                    (long)global::System.Windows.Forms.Keys.@OemPipe,
                    (long)global::System.Windows.Forms.Keys.@Oem5,
                    (long)global::System.Windows.Forms.Keys.@OemCloseBrackets,
                    (long)global::System.Windows.Forms.Keys.@Oem6,
                    (long)global::System.Windows.Forms.Keys.@OemQuotes,
                    (long)global::System.Windows.Forms.Keys.@Oem7,
                    (long)global::System.Windows.Forms.Keys.@Oem8,
                    (long)global::System.Windows.Forms.Keys.@OemBackslash,
                    (long)global::System.Windows.Forms.Keys.@Oem102,
                    (long)global::System.Windows.Forms.Keys.@ProcessKey,
                    (long)global::System.Windows.Forms.Keys.@Packet,
                    (long)global::System.Windows.Forms.Keys.@Attn,
                    (long)global::System.Windows.Forms.Keys.@Crsel,
                    (long)global::System.Windows.Forms.Keys.@Exsel,
                    (long)global::System.Windows.Forms.Keys.@EraseEof,
                    (long)global::System.Windows.Forms.Keys.@Play,
                    (long)global::System.Windows.Forms.Keys.@Zoom,
                    (long)global::System.Windows.Forms.Keys.@NoName,
                    (long)global::System.Windows.Forms.Keys.@Pa1,
                    (long)global::System.Windows.Forms.Keys.@OemClear,
                    (long)global::System.Windows.Forms.Keys.@Shift,
                    (long)global::System.Windows.Forms.Keys.@Control,
                    (long)global::System.Windows.Forms.Keys.@Alt}, @"System.Windows.Forms.Keys"); break;
            }
            return s;
        }

        protected override void InitCallbacks() {
        }
    }

    public class XmlSerializationReaderTOptions : System.Xml.Serialization.XmlSerializationReader {

        public object Read5_TOptions() {
            object o = null;
            Reader.MoveToContent();
            if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                if (((object) Reader.LocalName == (object)id1_TOptions && (object) Reader.NamespaceURI == (object)id2_Item)) {
                    o = Read4_TOptions(true, true);
                }
                else {
                    throw CreateUnknownNodeException();
                }
            }
            else {
                UnknownNode(null, @":TOptions");
            }
            return (object)o;
        }

        global::Options.TOptions Read4_TOptions(bool isNullable, bool checkType) {
            System.Xml.XmlQualifiedName xsiType = checkType ? GetXsiType() : null;
            bool isNull = false;
            if (isNullable) isNull = ReadNull();
            if (checkType) {
            if (xsiType == null || ((object) ((System.Xml.XmlQualifiedName)xsiType).Name == (object)id1_TOptions && (object) ((System.Xml.XmlQualifiedName)xsiType).Namespace == (object)id2_Item)) {
            }
            else
                throw CreateUnknownTypeException((System.Xml.XmlQualifiedName)xsiType);
            }
            if (isNull) return null;
            global::Options.TOptions o;
            o = new global::Options.TOptions();
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
            int whileIterations0 = 0;
            int readerCount0 = ReaderCount;
            while (Reader.NodeType != System.Xml.XmlNodeType.EndElement && Reader.NodeType != System.Xml.XmlNodeType.None) {
                if (Reader.NodeType == System.Xml.XmlNodeType.Element) {
                    if (!paramsRead[0] && ((object) Reader.LocalName == (object)id3_ItemRemoveShortcut && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@ItemRemoveShortcut = Read3_Keys(Reader.ReadElementString());
                        }
                        paramsRead[0] = true;
                    }
                    else if (!paramsRead[1] && ((object) Reader.LocalName == (object)id4_ItemAddShortcut && (object) Reader.NamespaceURI == (object)id2_Item)) {
                        {
                            o.@ItemAddShortcut = Read3_Keys(Reader.ReadElementString());
                        }
                        paramsRead[1] = true;
                    }
                    else {
                        UnknownNode((object)o, @":ItemRemoveShortcut, :ItemAddShortcut");
                    }
                }
                else {
                    UnknownNode((object)o, @":ItemRemoveShortcut, :ItemAddShortcut");
                }
                Reader.MoveToContent();
                CheckReaderCount(ref whileIterations0, ref readerCount0);
            }
            ReadEndElement();
            return o;
        }

        System.Collections.Hashtable _KeysValues;

        internal System.Collections.Hashtable KeysValues {
            get {
                if ((object)_KeysValues == null) {
                    System.Collections.Hashtable h = new System.Collections.Hashtable();
                    h.Add(@"KeyCode", (long)global::System.Windows.Forms.Keys.@KeyCode);
                    h.Add(@"Modifiers", (long)global::System.Windows.Forms.Keys.@Modifiers);
                    h.Add(@"None", (long)global::System.Windows.Forms.Keys.@None);
                    h.Add(@"LButton", (long)global::System.Windows.Forms.Keys.@LButton);
                    h.Add(@"RButton", (long)global::System.Windows.Forms.Keys.@RButton);
                    h.Add(@"Cancel", (long)global::System.Windows.Forms.Keys.@Cancel);
                    h.Add(@"MButton", (long)global::System.Windows.Forms.Keys.@MButton);
                    h.Add(@"XButton1", (long)global::System.Windows.Forms.Keys.@XButton1);
                    h.Add(@"XButton2", (long)global::System.Windows.Forms.Keys.@XButton2);
                    h.Add(@"Back", (long)global::System.Windows.Forms.Keys.@Back);
                    h.Add(@"Tab", (long)global::System.Windows.Forms.Keys.@Tab);
                    h.Add(@"LineFeed", (long)global::System.Windows.Forms.Keys.@LineFeed);
                    h.Add(@"Clear", (long)global::System.Windows.Forms.Keys.@Clear);
                    h.Add(@"Return", (long)global::System.Windows.Forms.Keys.@Return);
                    h.Add(@"Enter", (long)global::System.Windows.Forms.Keys.@Enter);
                    h.Add(@"ShiftKey", (long)global::System.Windows.Forms.Keys.@ShiftKey);
                    h.Add(@"ControlKey", (long)global::System.Windows.Forms.Keys.@ControlKey);
                    h.Add(@"Menu", (long)global::System.Windows.Forms.Keys.@Menu);
                    h.Add(@"Pause", (long)global::System.Windows.Forms.Keys.@Pause);
                    h.Add(@"Capital", (long)global::System.Windows.Forms.Keys.@Capital);
                    h.Add(@"CapsLock", (long)global::System.Windows.Forms.Keys.@CapsLock);
                    h.Add(@"KanaMode", (long)global::System.Windows.Forms.Keys.@KanaMode);
                    h.Add(@"HanguelMode", (long)global::System.Windows.Forms.Keys.@HanguelMode);
                    h.Add(@"HangulMode", (long)global::System.Windows.Forms.Keys.@HangulMode);
                    h.Add(@"JunjaMode", (long)global::System.Windows.Forms.Keys.@JunjaMode);
                    h.Add(@"FinalMode", (long)global::System.Windows.Forms.Keys.@FinalMode);
                    h.Add(@"HanjaMode", (long)global::System.Windows.Forms.Keys.@HanjaMode);
                    h.Add(@"KanjiMode", (long)global::System.Windows.Forms.Keys.@KanjiMode);
                    h.Add(@"Escape", (long)global::System.Windows.Forms.Keys.@Escape);
                    h.Add(@"IMEConvert", (long)global::System.Windows.Forms.Keys.@IMEConvert);
                    h.Add(@"IMENonconvert", (long)global::System.Windows.Forms.Keys.@IMENonconvert);
                    h.Add(@"IMEAccept", (long)global::System.Windows.Forms.Keys.@IMEAccept);
                    h.Add(@"IMEAceept", (long)global::System.Windows.Forms.Keys.@IMEAceept);
                    h.Add(@"IMEModeChange", (long)global::System.Windows.Forms.Keys.@IMEModeChange);
                    h.Add(@"Space", (long)global::System.Windows.Forms.Keys.@Space);
                    h.Add(@"Prior", (long)global::System.Windows.Forms.Keys.@Prior);
                    h.Add(@"PageUp", (long)global::System.Windows.Forms.Keys.@PageUp);
                    h.Add(@"Next", (long)global::System.Windows.Forms.Keys.@Next);
                    h.Add(@"PageDown", (long)global::System.Windows.Forms.Keys.@PageDown);
                    h.Add(@"End", (long)global::System.Windows.Forms.Keys.@End);
                    h.Add(@"Home", (long)global::System.Windows.Forms.Keys.@Home);
                    h.Add(@"Left", (long)global::System.Windows.Forms.Keys.@Left);
                    h.Add(@"Up", (long)global::System.Windows.Forms.Keys.@Up);
                    h.Add(@"Right", (long)global::System.Windows.Forms.Keys.@Right);
                    h.Add(@"Down", (long)global::System.Windows.Forms.Keys.@Down);
                    h.Add(@"Select", (long)global::System.Windows.Forms.Keys.@Select);
                    h.Add(@"Print", (long)global::System.Windows.Forms.Keys.@Print);
                    h.Add(@"Execute", (long)global::System.Windows.Forms.Keys.@Execute);
                    h.Add(@"Snapshot", (long)global::System.Windows.Forms.Keys.@Snapshot);
                    h.Add(@"PrintScreen", (long)global::System.Windows.Forms.Keys.@PrintScreen);
                    h.Add(@"Insert", (long)global::System.Windows.Forms.Keys.@Insert);
                    h.Add(@"Delete", (long)global::System.Windows.Forms.Keys.@Delete);
                    h.Add(@"Help", (long)global::System.Windows.Forms.Keys.@Help);
                    h.Add(@"D0", (long)global::System.Windows.Forms.Keys.@D0);
                    h.Add(@"D1", (long)global::System.Windows.Forms.Keys.@D1);
                    h.Add(@"D2", (long)global::System.Windows.Forms.Keys.@D2);
                    h.Add(@"D3", (long)global::System.Windows.Forms.Keys.@D3);
                    h.Add(@"D4", (long)global::System.Windows.Forms.Keys.@D4);
                    h.Add(@"D5", (long)global::System.Windows.Forms.Keys.@D5);
                    h.Add(@"D6", (long)global::System.Windows.Forms.Keys.@D6);
                    h.Add(@"D7", (long)global::System.Windows.Forms.Keys.@D7);
                    h.Add(@"D8", (long)global::System.Windows.Forms.Keys.@D8);
                    h.Add(@"D9", (long)global::System.Windows.Forms.Keys.@D9);
                    h.Add(@"A", (long)global::System.Windows.Forms.Keys.@A);
                    h.Add(@"B", (long)global::System.Windows.Forms.Keys.@B);
                    h.Add(@"C", (long)global::System.Windows.Forms.Keys.@C);
                    h.Add(@"D", (long)global::System.Windows.Forms.Keys.@D);
                    h.Add(@"E", (long)global::System.Windows.Forms.Keys.@E);
                    h.Add(@"F", (long)global::System.Windows.Forms.Keys.@F);
                    h.Add(@"G", (long)global::System.Windows.Forms.Keys.@G);
                    h.Add(@"H", (long)global::System.Windows.Forms.Keys.@H);
                    h.Add(@"I", (long)global::System.Windows.Forms.Keys.@I);
                    h.Add(@"J", (long)global::System.Windows.Forms.Keys.@J);
                    h.Add(@"K", (long)global::System.Windows.Forms.Keys.@K);
                    h.Add(@"L", (long)global::System.Windows.Forms.Keys.@L);
                    h.Add(@"M", (long)global::System.Windows.Forms.Keys.@M);
                    h.Add(@"N", (long)global::System.Windows.Forms.Keys.@N);
                    h.Add(@"O", (long)global::System.Windows.Forms.Keys.@O);
                    h.Add(@"P", (long)global::System.Windows.Forms.Keys.@P);
                    h.Add(@"Q", (long)global::System.Windows.Forms.Keys.@Q);
                    h.Add(@"R", (long)global::System.Windows.Forms.Keys.@R);
                    h.Add(@"S", (long)global::System.Windows.Forms.Keys.@S);
                    h.Add(@"T", (long)global::System.Windows.Forms.Keys.@T);
                    h.Add(@"U", (long)global::System.Windows.Forms.Keys.@U);
                    h.Add(@"V", (long)global::System.Windows.Forms.Keys.@V);
                    h.Add(@"W", (long)global::System.Windows.Forms.Keys.@W);
                    h.Add(@"X", (long)global::System.Windows.Forms.Keys.@X);
                    h.Add(@"Y", (long)global::System.Windows.Forms.Keys.@Y);
                    h.Add(@"Z", (long)global::System.Windows.Forms.Keys.@Z);
                    h.Add(@"LWin", (long)global::System.Windows.Forms.Keys.@LWin);
                    h.Add(@"RWin", (long)global::System.Windows.Forms.Keys.@RWin);
                    h.Add(@"Apps", (long)global::System.Windows.Forms.Keys.@Apps);
                    h.Add(@"Sleep", (long)global::System.Windows.Forms.Keys.@Sleep);
                    h.Add(@"NumPad0", (long)global::System.Windows.Forms.Keys.@NumPad0);
                    h.Add(@"NumPad1", (long)global::System.Windows.Forms.Keys.@NumPad1);
                    h.Add(@"NumPad2", (long)global::System.Windows.Forms.Keys.@NumPad2);
                    h.Add(@"NumPad3", (long)global::System.Windows.Forms.Keys.@NumPad3);
                    h.Add(@"NumPad4", (long)global::System.Windows.Forms.Keys.@NumPad4);
                    h.Add(@"NumPad5", (long)global::System.Windows.Forms.Keys.@NumPad5);
                    h.Add(@"NumPad6", (long)global::System.Windows.Forms.Keys.@NumPad6);
                    h.Add(@"NumPad7", (long)global::System.Windows.Forms.Keys.@NumPad7);
                    h.Add(@"NumPad8", (long)global::System.Windows.Forms.Keys.@NumPad8);
                    h.Add(@"NumPad9", (long)global::System.Windows.Forms.Keys.@NumPad9);
                    h.Add(@"Multiply", (long)global::System.Windows.Forms.Keys.@Multiply);
                    h.Add(@"Add", (long)global::System.Windows.Forms.Keys.@Add);
                    h.Add(@"Separator", (long)global::System.Windows.Forms.Keys.@Separator);
                    h.Add(@"Subtract", (long)global::System.Windows.Forms.Keys.@Subtract);
                    h.Add(@"Decimal", (long)global::System.Windows.Forms.Keys.@Decimal);
                    h.Add(@"Divide", (long)global::System.Windows.Forms.Keys.@Divide);
                    h.Add(@"F1", (long)global::System.Windows.Forms.Keys.@F1);
                    h.Add(@"F2", (long)global::System.Windows.Forms.Keys.@F2);
                    h.Add(@"F3", (long)global::System.Windows.Forms.Keys.@F3);
                    h.Add(@"F4", (long)global::System.Windows.Forms.Keys.@F4);
                    h.Add(@"F5", (long)global::System.Windows.Forms.Keys.@F5);
                    h.Add(@"F6", (long)global::System.Windows.Forms.Keys.@F6);
                    h.Add(@"F7", (long)global::System.Windows.Forms.Keys.@F7);
                    h.Add(@"F8", (long)global::System.Windows.Forms.Keys.@F8);
                    h.Add(@"F9", (long)global::System.Windows.Forms.Keys.@F9);
                    h.Add(@"F10", (long)global::System.Windows.Forms.Keys.@F10);
                    h.Add(@"F11", (long)global::System.Windows.Forms.Keys.@F11);
                    h.Add(@"F12", (long)global::System.Windows.Forms.Keys.@F12);
                    h.Add(@"F13", (long)global::System.Windows.Forms.Keys.@F13);
                    h.Add(@"F14", (long)global::System.Windows.Forms.Keys.@F14);
                    h.Add(@"F15", (long)global::System.Windows.Forms.Keys.@F15);
                    h.Add(@"F16", (long)global::System.Windows.Forms.Keys.@F16);
                    h.Add(@"F17", (long)global::System.Windows.Forms.Keys.@F17);
                    h.Add(@"F18", (long)global::System.Windows.Forms.Keys.@F18);
                    h.Add(@"F19", (long)global::System.Windows.Forms.Keys.@F19);
                    h.Add(@"F20", (long)global::System.Windows.Forms.Keys.@F20);
                    h.Add(@"F21", (long)global::System.Windows.Forms.Keys.@F21);
                    h.Add(@"F22", (long)global::System.Windows.Forms.Keys.@F22);
                    h.Add(@"F23", (long)global::System.Windows.Forms.Keys.@F23);
                    h.Add(@"F24", (long)global::System.Windows.Forms.Keys.@F24);
                    h.Add(@"NumLock", (long)global::System.Windows.Forms.Keys.@NumLock);
                    h.Add(@"Scroll", (long)global::System.Windows.Forms.Keys.@Scroll);
                    h.Add(@"LShiftKey", (long)global::System.Windows.Forms.Keys.@LShiftKey);
                    h.Add(@"RShiftKey", (long)global::System.Windows.Forms.Keys.@RShiftKey);
                    h.Add(@"LControlKey", (long)global::System.Windows.Forms.Keys.@LControlKey);
                    h.Add(@"RControlKey", (long)global::System.Windows.Forms.Keys.@RControlKey);
                    h.Add(@"LMenu", (long)global::System.Windows.Forms.Keys.@LMenu);
                    h.Add(@"RMenu", (long)global::System.Windows.Forms.Keys.@RMenu);
                    h.Add(@"BrowserBack", (long)global::System.Windows.Forms.Keys.@BrowserBack);
                    h.Add(@"BrowserForward", (long)global::System.Windows.Forms.Keys.@BrowserForward);
                    h.Add(@"BrowserRefresh", (long)global::System.Windows.Forms.Keys.@BrowserRefresh);
                    h.Add(@"BrowserStop", (long)global::System.Windows.Forms.Keys.@BrowserStop);
                    h.Add(@"BrowserSearch", (long)global::System.Windows.Forms.Keys.@BrowserSearch);
                    h.Add(@"BrowserFavorites", (long)global::System.Windows.Forms.Keys.@BrowserFavorites);
                    h.Add(@"BrowserHome", (long)global::System.Windows.Forms.Keys.@BrowserHome);
                    h.Add(@"VolumeMute", (long)global::System.Windows.Forms.Keys.@VolumeMute);
                    h.Add(@"VolumeDown", (long)global::System.Windows.Forms.Keys.@VolumeDown);
                    h.Add(@"VolumeUp", (long)global::System.Windows.Forms.Keys.@VolumeUp);
                    h.Add(@"MediaNextTrack", (long)global::System.Windows.Forms.Keys.@MediaNextTrack);
                    h.Add(@"MediaPreviousTrack", (long)global::System.Windows.Forms.Keys.@MediaPreviousTrack);
                    h.Add(@"MediaStop", (long)global::System.Windows.Forms.Keys.@MediaStop);
                    h.Add(@"MediaPlayPause", (long)global::System.Windows.Forms.Keys.@MediaPlayPause);
                    h.Add(@"LaunchMail", (long)global::System.Windows.Forms.Keys.@LaunchMail);
                    h.Add(@"SelectMedia", (long)global::System.Windows.Forms.Keys.@SelectMedia);
                    h.Add(@"LaunchApplication1", (long)global::System.Windows.Forms.Keys.@LaunchApplication1);
                    h.Add(@"LaunchApplication2", (long)global::System.Windows.Forms.Keys.@LaunchApplication2);
                    h.Add(@"OemSemicolon", (long)global::System.Windows.Forms.Keys.@OemSemicolon);
                    h.Add(@"Oem1", (long)global::System.Windows.Forms.Keys.@Oem1);
                    h.Add(@"Oemplus", (long)global::System.Windows.Forms.Keys.@Oemplus);
                    h.Add(@"Oemcomma", (long)global::System.Windows.Forms.Keys.@Oemcomma);
                    h.Add(@"OemMinus", (long)global::System.Windows.Forms.Keys.@OemMinus);
                    h.Add(@"OemPeriod", (long)global::System.Windows.Forms.Keys.@OemPeriod);
                    h.Add(@"OemQuestion", (long)global::System.Windows.Forms.Keys.@OemQuestion);
                    h.Add(@"Oem2", (long)global::System.Windows.Forms.Keys.@Oem2);
                    h.Add(@"Oemtilde", (long)global::System.Windows.Forms.Keys.@Oemtilde);
                    h.Add(@"Oem3", (long)global::System.Windows.Forms.Keys.@Oem3);
                    h.Add(@"OemOpenBrackets", (long)global::System.Windows.Forms.Keys.@OemOpenBrackets);
                    h.Add(@"Oem4", (long)global::System.Windows.Forms.Keys.@Oem4);
                    h.Add(@"OemPipe", (long)global::System.Windows.Forms.Keys.@OemPipe);
                    h.Add(@"Oem5", (long)global::System.Windows.Forms.Keys.@Oem5);
                    h.Add(@"OemCloseBrackets", (long)global::System.Windows.Forms.Keys.@OemCloseBrackets);
                    h.Add(@"Oem6", (long)global::System.Windows.Forms.Keys.@Oem6);
                    h.Add(@"OemQuotes", (long)global::System.Windows.Forms.Keys.@OemQuotes);
                    h.Add(@"Oem7", (long)global::System.Windows.Forms.Keys.@Oem7);
                    h.Add(@"Oem8", (long)global::System.Windows.Forms.Keys.@Oem8);
                    h.Add(@"OemBackslash", (long)global::System.Windows.Forms.Keys.@OemBackslash);
                    h.Add(@"Oem102", (long)global::System.Windows.Forms.Keys.@Oem102);
                    h.Add(@"ProcessKey", (long)global::System.Windows.Forms.Keys.@ProcessKey);
                    h.Add(@"Packet", (long)global::System.Windows.Forms.Keys.@Packet);
                    h.Add(@"Attn", (long)global::System.Windows.Forms.Keys.@Attn);
                    h.Add(@"Crsel", (long)global::System.Windows.Forms.Keys.@Crsel);
                    h.Add(@"Exsel", (long)global::System.Windows.Forms.Keys.@Exsel);
                    h.Add(@"EraseEof", (long)global::System.Windows.Forms.Keys.@EraseEof);
                    h.Add(@"Play", (long)global::System.Windows.Forms.Keys.@Play);
                    h.Add(@"Zoom", (long)global::System.Windows.Forms.Keys.@Zoom);
                    h.Add(@"NoName", (long)global::System.Windows.Forms.Keys.@NoName);
                    h.Add(@"Pa1", (long)global::System.Windows.Forms.Keys.@Pa1);
                    h.Add(@"OemClear", (long)global::System.Windows.Forms.Keys.@OemClear);
                    h.Add(@"Shift", (long)global::System.Windows.Forms.Keys.@Shift);
                    h.Add(@"Control", (long)global::System.Windows.Forms.Keys.@Control);
                    h.Add(@"Alt", (long)global::System.Windows.Forms.Keys.@Alt);
                    _KeysValues = h;
                }
                return _KeysValues;
            }
        }

        global::System.Windows.Forms.Keys Read3_Keys(string s) {
            return (global::System.Windows.Forms.Keys)ToEnum(s, KeysValues, @"global::System.Windows.Forms.Keys");
        }

        protected override void InitCallbacks() {
        }

        string id1_TOptions;
        string id2_Item;
        string id3_ItemRemoveShortcut;
        string id4_ItemAddShortcut;

        protected override void InitIDs() {
            id1_TOptions = Reader.NameTable.Add(@"TOptions");
            id2_Item = Reader.NameTable.Add(@"");
            id3_ItemRemoveShortcut = Reader.NameTable.Add(@"ItemRemoveShortcut");
            id4_ItemAddShortcut = Reader.NameTable.Add(@"ItemAddShortcut");
        }
    }

    public abstract class XmlSerializer1 : System.Xml.Serialization.XmlSerializer {
        protected override System.Xml.Serialization.XmlSerializationReader CreateReader() {
            return new XmlSerializationReaderTOptions();
        }
        protected override System.Xml.Serialization.XmlSerializationWriter CreateWriter() {
            return new XmlSerializationWriterTOptions();
        }
    }

    public sealed class TOptionsSerializer : XmlSerializer1 {

        public override System.Boolean CanDeserialize(System.Xml.XmlReader xmlReader) {
            return xmlReader.IsStartElement(@"TOptions", @"");
        }

        protected override void Serialize(object objectToSerialize, System.Xml.Serialization.XmlSerializationWriter writer) {
            ((XmlSerializationWriterTOptions)writer).Write5_TOptions(objectToSerialize);
        }

        protected override object Deserialize(System.Xml.Serialization.XmlSerializationReader reader) {
            return ((XmlSerializationReaderTOptions)reader).Read5_TOptions();
        }
    }

    public class XmlSerializerContract : global::System.Xml.Serialization.XmlSerializerImplementation {
        public override global::System.Xml.Serialization.XmlSerializationReader Reader { get { return new XmlSerializationReaderTOptions(); } }
        public override global::System.Xml.Serialization.XmlSerializationWriter Writer { get { return new XmlSerializationWriterTOptions(); } }
        System.Collections.Hashtable readMethods = null;
        public override System.Collections.Hashtable ReadMethods {
            get {
                if (readMethods == null) {
                    System.Collections.Hashtable _tmp = new System.Collections.Hashtable();
                    _tmp[@"Options.TOptions::"] = @"Read5_TOptions";
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
                    _tmp[@"Options.TOptions::"] = @"Write5_TOptions";
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
                    _tmp.Add(@"Options.TOptions::", new TOptionsSerializer());
                    if (typedSerializers == null) typedSerializers = _tmp;
                }
                return typedSerializers;
            }
        }
        public override System.Boolean CanSerialize(System.Type type) {
            if (type == typeof(global::Options.TOptions)) return true;
            return false;
        }
        public override System.Xml.Serialization.XmlSerializer GetSerializer(System.Type type) {
            if (type == typeof(global::Options.TOptions)) return new TOptionsSerializer();
            return null;
        }
    }
}
