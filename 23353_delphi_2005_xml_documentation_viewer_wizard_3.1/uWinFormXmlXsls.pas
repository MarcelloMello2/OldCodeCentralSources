{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uWinFormXmlXsls;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, AxSHDocVw, System.Resources,
  System.Xml.Xsl, uWinFormManageXSLs;

type
  TWinFormXmlXsls = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    TabControlTop: System.Windows.Forms.TabControl;
    TabPage_DocStr: System.Windows.Forms.TabPage;
    ToolBar_DocStr: System.Windows.Forms.ToolBar;
    ToolBarButton_CopyToClipboard: System.Windows.Forms.ToolBarButton;
    WebBrowserIE: AxSHDocVw.AxWebBrowser;
    ImageListMain: System.Windows.Forms.ImageList;
    TabPage_XSL: System.Windows.Forms.TabPage;
    TabPage_XML: System.Windows.Forms.TabPage;
    ComboBox_XSL: System.Windows.Forms.ComboBox;
    ToolBar_XSL: System.Windows.Forms.ToolBar;
    ToolBarButton_ManageXSL: System.Windows.Forms.ToolBarButton;
    ToolBar_XML: System.Windows.Forms.ToolBar;
    ToolBarButton_OpenXML: System.Windows.Forms.ToolBarButton;
    OpenFileDlg_XmlDocFile: System.Windows.Forms.OpenFileDialog;
    ToolBarButton_Refresh: System.Windows.Forms.ToolBarButton;
    TextBox_XML: System.Windows.Forms.TextBox;
    ToolBarButton_SaveToFile: System.Windows.Forms.ToolBarButton;
    ToolBarButton_Separator: System.Windows.Forms.ToolBarButton;
    SaveFileDlg_Doc: System.Windows.Forms.SaveFileDialog;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure ToolBar_DocStr_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure ComboBox_XSL_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure ToolBar_XML_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure ToolBar_XSL_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure TWinFormXmlXsls_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    FDocStr: string;
    FXslt: XslTransform;
    FXslLoaded: Boolean;
    FWinFormManageXSLs: TWinFormManageXSLs;
    procedure NavigateBlank;
    procedure DisplayDocStr(const s: string);
    procedure StylesheetChanged;
    function getCfgFileName: string;
    procedure DisplayException(e: Exception);
  public
    procedure Do_CopyToClipboard;
    procedure Do_Refresh;
    procedure Do_OpenXML;
    procedure Do_ManageXSLs;
    procedure Do_SaveToCfg;
    procedure Do_ReadFromCfg;
    procedure Do_SaveToFile;
    procedure set_XmlFilename(const Value: string);
    function get_XmlFilename: string;
  public
    procedure set_DocStr(const Value: string);
  public
    constructor Create;
    function GetTransformResult: string;
    property DocStr: string read FDocStr write set_DocStr;
    property XmlFilename: string read get_XmlFilename write set_XmlFilename;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TWinFormXmlXsls))]

implementation

uses Borland.mshtml, System.Xml.XPath, System.IO, System.Xml, System.Text;

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWinFormXmlXsls.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ToolBarButton = array of System.Windows.Forms.ToolBarButton;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TWinFormXmlXsls));
  Self.TabControlTop := System.Windows.Forms.TabControl.Create;
  Self.TabPage_XSL := System.Windows.Forms.TabPage.Create;
  Self.ComboBox_XSL := System.Windows.Forms.ComboBox.Create;
  Self.ToolBar_XSL := System.Windows.Forms.ToolBar.Create;
  Self.ToolBarButton_ManageXSL := System.Windows.Forms.ToolBarButton.Create;
  Self.ImageListMain := System.Windows.Forms.ImageList.Create(Self.components);
  Self.TabPage_XML := System.Windows.Forms.TabPage.Create;
  Self.TextBox_XML := System.Windows.Forms.TextBox.Create;
  Self.ToolBar_XML := System.Windows.Forms.ToolBar.Create;
  Self.ToolBarButton_OpenXML := System.Windows.Forms.ToolBarButton.Create;
  Self.TabPage_DocStr := System.Windows.Forms.TabPage.Create;
  Self.ToolBar_DocStr := System.Windows.Forms.ToolBar.Create;
  Self.ToolBarButton_Refresh := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolBarButton_Separator := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolBarButton_CopyToClipboard := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolBarButton_SaveToFile := System.Windows.Forms.ToolBarButton.Create;
  Self.WebBrowserIE := AxSHDocVw.AxWebBrowser.Create;
  Self.OpenFileDlg_XmlDocFile := System.Windows.Forms.OpenFileDialog.Create;
  Self.SaveFileDlg_Doc := System.Windows.Forms.SaveFileDialog.Create;
  Self.TabControlTop.SuspendLayout;
  Self.TabPage_XSL.SuspendLayout;
  Self.TabPage_XML.SuspendLayout;
  Self.TabPage_DocStr.SuspendLayout;
  (System.ComponentModel.ISupportInitialize(Self.WebBrowserIE)).BeginInit;
  Self.SuspendLayout;
  // 
  // TabControlTop
  // 
  Self.TabControlTop.Controls.Add(Self.TabPage_XSL);
  Self.TabControlTop.Controls.Add(Self.TabPage_XML);
  Self.TabControlTop.Controls.Add(Self.TabPage_DocStr);
  Self.TabControlTop.Dock := System.Windows.Forms.DockStyle.Top;
  Self.TabControlTop.Location := System.Drawing.Point.Create(0, 0);
  Self.TabControlTop.Name := 'TabControlTop';
  Self.TabControlTop.SelectedIndex := 0;
  Self.TabControlTop.Size := System.Drawing.Size.Create(490, 54);
  Self.TabControlTop.TabIndex := 0;
  // 
  // TabPage_XSL
  // 
  Self.TabPage_XSL.Controls.Add(Self.ComboBox_XSL);
  Self.TabPage_XSL.Controls.Add(Self.ToolBar_XSL);
  Self.TabPage_XSL.Location := System.Drawing.Point.Create(4, 22);
  Self.TabPage_XSL.Name := 'TabPage_XSL';
  Self.TabPage_XSL.Size := System.Drawing.Size.Create(482, 28);
  Self.TabPage_XSL.TabIndex := 1;
  Self.TabPage_XSL.Text := 'XSL';
  // 
  // ComboBox_XSL
  // 
  Self.ComboBox_XSL.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.ComboBox_XSL.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.ComboBox_XSL.Location := System.Drawing.Point.Create(30, 3);
  Self.ComboBox_XSL.Name := 'ComboBox_XSL';
  Self.ComboBox_XSL.Size := System.Drawing.Size.Create(454, 21);
  Self.ComboBox_XSL.TabIndex := 1;
  Include(Self.ComboBox_XSL.SelectedIndexChanged, Self.ComboBox_XSL_SelectedIndexChanged);
  // 
  // ToolBar_XSL
  // 
  Self.ToolBar_XSL.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar_XSL.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.ToolBarButton_ManageXSL));
  Self.ToolBar_XSL.DropDownArrows := True;
  Self.ToolBar_XSL.Font := System.Drawing.Font.Create('Microsoft Sans Serif', 
      9.75, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 
      (Byte(0)));
  Self.ToolBar_XSL.ImageList := Self.ImageListMain;
  Self.ToolBar_XSL.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar_XSL.Name := 'ToolBar_XSL';
  Self.ToolBar_XSL.ShowToolTips := True;
  Self.ToolBar_XSL.Size := System.Drawing.Size.Create(482, 28);
  Self.ToolBar_XSL.TabIndex := 2;
  Include(Self.ToolBar_XSL.ButtonClick, Self.ToolBar_XSL_ButtonClick);
  // 
  // ToolBarButton_ManageXSL
  // 
  Self.ToolBarButton_ManageXSL.ImageIndex := 1;
  Self.ToolBarButton_ManageXSL.ToolTipText := 'Manage XSL stylesheets';
  // 
  // ImageListMain
  // 
  Self.ImageListMain.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageListMain.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('I' +
    'mageListMain.ImageStream')));
  Self.ImageListMain.TransparentColor := System.Drawing.Color.Fuchsia;
  // 
  // TabPage_XML
  // 
  Self.TabPage_XML.Controls.Add(Self.TextBox_XML);
  Self.TabPage_XML.Controls.Add(Self.ToolBar_XML);
  Self.TabPage_XML.Location := System.Drawing.Point.Create(4, 22);
  Self.TabPage_XML.Name := 'TabPage_XML';
  Self.TabPage_XML.Size := System.Drawing.Size.Create(482, 28);
  Self.TabPage_XML.TabIndex := 2;
  Self.TabPage_XML.Text := 'XML';
  // 
  // TextBox_XML
  // 
  Self.TextBox_XML.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.TextBox_XML.Location := System.Drawing.Point.Create(29, 5);
  Self.TextBox_XML.Name := 'TextBox_XML';
  Self.TextBox_XML.Size := System.Drawing.Size.Create(450, 20);
  Self.TextBox_XML.TabIndex := 1;
  Self.TextBox_XML.Text := '';
  // 
  // ToolBar_XML
  // 
  Self.ToolBar_XML.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar_XML.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.ToolBarButton_OpenXML));
  Self.ToolBar_XML.DropDownArrows := True;
  Self.ToolBar_XML.ImageList := Self.ImageListMain;
  Self.ToolBar_XML.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar_XML.Name := 'ToolBar_XML';
  Self.ToolBar_XML.ShowToolTips := True;
  Self.ToolBar_XML.Size := System.Drawing.Size.Create(482, 42);
  Self.ToolBar_XML.TabIndex := 0;
  Include(Self.ToolBar_XML.ButtonClick, Self.ToolBar_XML_ButtonClick);
  // 
  // ToolBarButton_OpenXML
  // 
  Self.ToolBarButton_OpenXML.ImageIndex := 2;
  Self.ToolBarButton_OpenXML.ToolTipText := 'Open XML file...';
  // 
  // TabPage_DocStr
  // 
  Self.TabPage_DocStr.Controls.Add(Self.ToolBar_DocStr);
  Self.TabPage_DocStr.Location := System.Drawing.Point.Create(4, 22);
  Self.TabPage_DocStr.Name := 'TabPage_DocStr';
  Self.TabPage_DocStr.Size := System.Drawing.Size.Create(482, 28);
  Self.TabPage_DocStr.TabIndex := 0;
  Self.TabPage_DocStr.Text := 'Doc';
  // 
  // ToolBar_DocStr
  // 
  Self.ToolBar_DocStr.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar_DocStr.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.ToolBarButton_Refresh, 
          Self.ToolBarButton_Separator, Self.ToolBarButton_CopyToClipboard, Self.ToolBarButton_SaveToFile));
  Self.ToolBar_DocStr.DropDownArrows := True;
  Self.ToolBar_DocStr.ImageList := Self.ImageListMain;
  Self.ToolBar_DocStr.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar_DocStr.Name := 'ToolBar_DocStr';
  Self.ToolBar_DocStr.ShowToolTips := True;
  Self.ToolBar_DocStr.Size := System.Drawing.Size.Create(482, 42);
  Self.ToolBar_DocStr.TabIndex := 0;
  Include(Self.ToolBar_DocStr.ButtonClick, Self.ToolBar_DocStr_ButtonClick);
  // 
  // ToolBarButton_Refresh
  // 
  Self.ToolBarButton_Refresh.ImageIndex := 3;
  Self.ToolBarButton_Refresh.ToolTipText := 'Refresh';
  // 
  // ToolBarButton_Separator
  // 
  Self.ToolBarButton_Separator.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // ToolBarButton_CopyToClipboard
  // 
  Self.ToolBarButton_CopyToClipboard.ImageIndex := 0;
  Self.ToolBarButton_CopyToClipboard.ToolTipText := 'Copy to Clipboard';
  // 
  // ToolBarButton_SaveToFile
  // 
  Self.ToolBarButton_SaveToFile.ImageIndex := 4;
  Self.ToolBarButton_SaveToFile.ToolTipText := 'Save As...';
  // 
  // WebBrowserIE
  // 
  Self.WebBrowserIE.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.WebBrowserIE.Enabled := True;
  Self.WebBrowserIE.Location := System.Drawing.Point.Create(0, 54);
  Self.WebBrowserIE.OcxState := (System.Windows.Forms.AxHost.State(resources.GetObject('W' +
    'ebBrowserIE.OcxState')));
  Self.WebBrowserIE.Size := System.Drawing.Size.Create(490, 277);
  Self.WebBrowserIE.TabIndex := 1;
  // 
  // OpenFileDlg_XmlDocFile
  // 
  Self.OpenFileDlg_XmlDocFile.DefaultExt := 'xml';
  Self.OpenFileDlg_XmlDocFile.Filter := 'XML files|*.xml';
  Self.OpenFileDlg_XmlDocFile.RestoreDirectory := True;
  Self.OpenFileDlg_XmlDocFile.Title := 'Select XML Document...';
  // 
  // SaveFileDlg_Doc
  // 
  Self.SaveFileDlg_Doc.DefaultExt := 'html';
  Self.SaveFileDlg_Doc.Filter := 'HTML files|*.html|All files|*.*';
  Self.SaveFileDlg_Doc.Title := 'Save As...';
  // 
  // TWinFormXmlXsls
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(490, 331);
  Self.Controls.Add(Self.WebBrowserIE);
  Self.Controls.Add(Self.TabControlTop);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.Name := 'TWinFormXmlXsls';
  Self.ShowInTaskbar := False;
  Self.Text := 'XML Documentation';
  Include(Self.Closing, Self.TWinFormXmlXsls_Closing);
  Self.TabControlTop.ResumeLayout(False);
  Self.TabPage_XSL.ResumeLayout(False);
  Self.TabPage_XML.ResumeLayout(False);
  Self.TabPage_DocStr.ResumeLayout(False);
  (System.ComponentModel.ISupportInitialize(Self.WebBrowserIE)).EndInit;
  Self.ResumeLayout(False);
end;
{$ENDREGION}

resourcestring
  STR_CFG_FILENAME = 'pgXmlDocViewer30Cfg.xml';
  STR_CFG_NAMESPACE = 'BDS.XmlDocViewer.30';
  STR_XMLDOCVIEWER_READY = 'XML Documentation Viewer is ready';
  STR_XSL_NOT_LOADED = 'XSL stylesheet is not loaded.';

procedure TWinFormXmlXsls.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TWinFormXmlXsls.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  NavigateBlank;
  FDocStr := '';

  FXslt := XslTransform.Create;
  FXslLoaded := False;

  Do_ReadFromCfg;

  if ComboBox_XSL.Items.Count > 0
    then ComboBox_XSL.SelectedIndex := 0;

  Do_Refresh;
end;

procedure TWinFormXmlXsls.NavigateBlank;
begin
  WebBrowserIE.Navigate('about:blank');
end;

procedure TWinFormXmlXsls.DisplayDocStr(const s: string);
var aDoc: IHTMLDocument2; objArray: array of System.&Object;
begin
  aDoc := WebBrowserIE.Document as IHTMLDocument2;
  aDoc.close; // clears current doc
  SetLength(objArray,1);
  objArray[0] := s;
  aDoc.write(objArray);
end;

procedure TWinFormXmlXsls.set_DocStr(const Value: string);
begin
  FDocStr := Value;
  DisplayDocStr(DocStr);
end;

procedure TWinFormXmlXsls.Do_CopyToClipboard;
begin
  Clipboard.SetDataObject(DocStr);
end;

procedure TWinFormXmlXsls.ToolBar_DocStr_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  case ToolBar_DocStr.Buttons.IndexOf(e.Button) of
    0: Do_Refresh;
    1: ; // separator
    2: Do_CopyToClipboard;
    3: Do_SaveToFile;
  end;
end;

procedure TWinFormXmlXsls.Do_Refresh;
begin
  DocStr := GetTransformResult;
end;

procedure TWinFormXmlXsls.set_XmlFilename(const Value: string);
begin
  if TextBox_XML.Text <> Value
  then TextBox_XML.Text := Value;
end;

function TWinFormXmlXsls.get_XmlFilename: string;
begin
  Result := TextBox_XML.Text;
end;

function TWinFormXmlXsls.GetTransformResult: string;
var aXPathDoc: XPathDocument; sWriter: StringWriter;
begin
  Result := STR_XMLDOCVIEWER_READY;
  try
    if &File.Exists(XmlFilename) then
    begin
      if FXslLoaded then
      begin
        aXPathDoc := XPathDocument.Create(XmlFilename);
        sWriter := StringWriter.Create;
        FXslt.Transform(aXPathDoc,nil,sWriter,nil);
        Result := sWriter.ToString;
      end
      else Result := STR_XSL_NOT_LOADED;
    end;
  except
    on e: Exception do DisplayException(e);
  end;
end;

procedure TWinFormXmlXsls.StylesheetChanged;
begin
  if &File.Exists(ComboBox_XSL.Text) then
  try
    FXslt.Load(ComboBox_XSL.Text);
    FXslLoaded := True;
  except
    on e: Exception do
    begin
      DisplayException(e);
      FXslLoaded := False;
    end;
  end;
  Do_Refresh;
end;

procedure TWinFormXmlXsls.ComboBox_XSL_SelectedIndexChanged(sender: System.Object;
  e: System.EventArgs);
begin
  StylesheetChanged;
end;

procedure TWinFormXmlXsls.ToolBar_XML_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  case ToolBar_XML.Buttons.IndexOf(e.Button) of
    0: Do_OpenXML;
  end;
end;

procedure TWinFormXmlXsls.Do_OpenXML;
begin
  if &File.Exists(XmlFilename)
    then OpenFileDlg_XmlDocFile.FileName := XmlFilename;

  if OpenFileDlg_XmlDocFile.ShowDialog = System.Windows.Forms.DialogResult.OK
  then XmlFilename := OpenFileDlg_XmlDocFile.FileName;

  Do_Refresh;
end;

procedure TWinFormXmlXsls.ToolBar_XSL_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  case ToolBar_XSL.Buttons.IndexOf(e.Button) of
    0: Do_ManageXSLs;
  end;
end;

procedure TWinFormXmlXsls.Do_ManageXSLs;
var aSelectedText: string; s: string; i: integer;
begin
  aSelectedText := ComboBox_XSL.Text;
  if FWinFormManageXSLs = nil then FWinFormManageXSLs := TWinFormManageXSLs.Create;

  FWinFormManageXSLs.ClearList;

  for s in ComboBox_XSL.Items do FWinFormManageXSLs.AddItem(s);

  if FWinFormManageXSLs.ShowDialog = System.Windows.Forms.DialogResult.OK then
  begin
    ComboBox_XSL.Items.Clear;
    for i:=0 to FWinFormManageXSLs.GetCount-1 do
      ComboBox_XSL.Items.Add(FWinFormManageXSLs.GetItem(i));

    if ComboBox_XSL.Items.Count > 0 then
    begin
      i := ComboBox_XSL.Items.IndexOf(aSelectedText);
      if i > -1
      then ComboBox_XSL.SelectedIndex := i
      else ComboBox_XSL.SelectedIndex := 0;
    end;

    Do_SaveToCfg;
  end;
end;

procedure TWinFormXmlXsls.Do_ReadFromCfg;
var aReader: XmlTextReader; aCfg: string;
begin
  aCfg := getCfgFileName;
  if &File.Exists(aCfg) then
  begin
    if ComboBox_XSL.Items.Count > 0 then ComboBox_XSL.Items.Clear;

    try
      aReader := XmlTextReader.Create(aCfg);
      try
        while aReader.Read do
          if (aReader.NodeType = XmlNodeType.Element) then
            if aReader.Name = 'XmlDocXsl' then
            begin
              if aReader.Read then
                if aReader.NodeType = XmlNodeType.Text then
                  ComboBox_XSL.Items.Add(aReader.Value);
            end
            else if aReader.Name = 'Left' then
            begin
              if aReader.Read then
                if aReader.NodeType = XmlNodeType.Text then
                  self.Left := Convert.ToInt32(aReader.Value);
            end
            else if aReader.Name = 'Top' then
            begin
              if aReader.Read then
                if aReader.NodeType = XmlNodeType.Text then
                  self.Top := Convert.ToInt32(aReader.Value);
            end
            else if aReader.Name = 'Width' then
            begin
              if aReader.Read then
                if aReader.NodeType = XmlNodeType.Text then
                  self.Width := Convert.ToInt32(aReader.Value);
            end
            else if aReader.Name = 'Height' then
            begin
              if aReader.Read then
                if aReader.NodeType = XmlNodeType.Text then
                  self.Height := Convert.ToInt32(aReader.Value);
            end;
      finally
        aReader.Close;
      end;
    except
      on e: Exception do DisplayException(e);
    end;
  end;
end;

procedure TWinFormXmlXsls.Do_SaveToCfg;
var aWriter: XmlTextWriter; s: string; aNS: string;
begin
  aNS := STR_CFG_NAMESPACE;
  try
    aWriter := XmlTextWriter.Create(getCfgFileName, Encoding.UTF8);
    try
      aWriter.WriteStartDocument;
        aWriter.WriteStartElement('XmlDocViewerCfg',aNS);

          aWriter.WriteStartElement('Coords',aNS);
            aWriter.WriteElementString('Left', aNS, self.Left.ToString);
            aWriter.WriteElementString('Top', aNS, self.Top.ToString);
            aWriter.WriteElementString('Width', aNS, self.Width.ToString);
            aWriter.WriteElementString('Height', aNS, self.Height.ToString);
          aWriter.WriteEndElement; // Coords

          aWriter.WriteStartElement('XmlDocXsls', aNS);
            for s in ComboBox_XSL.Items do aWriter.WriteElementString('XmlDocXsl', aNS, s);
          aWriter.WriteEndElement; // XmlDocXsls

        aWriter.WriteEndElement; // XmlDocViewerCfg
      aWriter.WriteEndDocument;
    finally
      aWriter.Close;
    end;
  except
    on e: Exception do DisplayException(e);
  end;
end;

function TWinFormXmlXsls.getCfgFileName: string;
begin
   Result := Environment.CurrentDirectory + '\' + STR_CFG_FILENAME;
end;

procedure TWinFormXmlXsls.TWinFormXmlXsls_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
begin
  e.Cancel := True;
  self.Hide;
end;

procedure TWinFormXmlXsls.Do_SaveToFile;
var aWriter: StreamWriter;
begin
  if SaveFileDlg_Doc.ShowDialog = System.Windows.Forms.DialogResult.OK then
  begin
    aWriter := StreamWriter.Create(SaveFileDlg_Doc.FileName);
    try
      aWriter.Write(DocStr);
    finally
      aWriter.Close;
    end;
  end;
end;

procedure TWinFormXmlXsls.DisplayException(e: Exception);
var s: string;
begin
  if e <> nil then
  begin
    s := 'An exception occured of type <B>'
       + e.GetType.ToString + '</B> with message: <I>' + e.Message + '</I>';
    DisplayDocStr(s);
  end;
end;

end.
