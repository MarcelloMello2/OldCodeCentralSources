unit Unit1;

// (c) 2013 Paul TOTH <tothpaul@free.fr>
// http://lookinside.free.fr/delphi.php?Delphi+et+les+DFM

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure SetMsg(Value: string);
  published
    property Msg: string write SetMsg;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TTreeReader = class(TReader)
  private
    FStream  : TResourceStream;
    FTreeView: TTreeView;
    function StringValue: string;
    procedure ReadNode(Parent: TTreeNode);
  public
    constructor Create(AResourceName: string; ATreeView: TTreeView);
    destructor Destroy; override;
  end;

{ TTreeReader }

constructor TTreeReader.Create(AResourceName: string; ATreeView: TTreeView);
begin
  FTreeView := ATreeView;
  FStream := TResourceStream.Create(hInstance, AResourceName, RT_RCDATA);
  inherited Create(FStream, 4096);
  ReadSignature; // Signature du DFM
  FTreeView.Items.Clear;
  ReadNode(nil); // lire les noeuds
end;

destructor TTreeReader.Destroy;
begin
  inherited;
  FStream.Free;
end;

// Retourne une valeur quelconque sous forme de chaîne
function TTreeReader.StringValue: string;
var
  Str: string;
begin
  case NextValue of
    vaSet:
    begin
      ReadValue;
      Result := '';
      repeat
        Str := ReadStr;
        Result := Result + ',' + Str;
      until Str = '';
      if Result = ',' then
        Result := '[]'
      else begin
        Result[1] := '[';
        Result[Length(Result)] := ']';
      end;
    end;
    vaIdent : Result := ReadIdent;
  else
    Result := ReadVariant;
  end;
end;

procedure TTreeReader.ReadNode(Parent: TTreeNode);
var
  Flags   : TFilerFlags;
  Index   : Integer;
  strClass: string;
  strName : string;
  root    : TTreeNode;
begin
  ReadPrefix(Flags, Index); // utile pour TFrame par exemple

  strClass := ReadStr; // la classe du composant
  strName  := ReadStr; // son nom

  root := FTreeView.Items.AddChild(Parent, strName + ':' + strClass);

  // Liste de propriétés du composant
  while not EndOfList do
  begin
    strName := ReadStr; // nom de la propriété, peut être sous la forme 'Font.Name'
    FTreeView.Items.AddChild(root, strName + ':' + StringValue);
  end;
  ReadListEnd;

  // Liste des enfants de ce composant
  while not EndOfList do
  begin
    ReadNode(root);
  end;
  ReadListEnd;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TTreeReader.Create('TFORM1', TreeView1) do
    Free;
end;

{$R ExtraFM.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  with TTreeReader.Create('ROOTNODE', TreeView1) do
    Free;
end;

{$R ExtraData.dfm}

procedure TForm1.Button3Click(Sender: TObject);
begin
  with TTreeReader.Create('TFORM1_EXTRA', TreeView1) do
    Free;
end;

type
  TOpenReader = class(TReader)
  end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Resource: TResourceStream;
  Reader  : TOpenReader;

  procedure ReadExtra;
  var
    Flags   : TFilerFlags;
    Index   : Integer;
    strClass: string;
    strName : string;
    Comp    : TComponent;
  begin
    Reader.ReadPrefix(Flags, Index); // don't care
    strClass := Reader.ReadStr;
    strName  := Reader.ReadStr;

    if strName = Self.Name then
      Comp := Self
    else begin
      Comp := FindComponent(strName);
      if Comp = nil then
        raise Exception.Create(strName + ':' + strClass + ' not found');
    end;

    while not Reader.EndOfList do
      Reader.ReadProperty(Comp);
    Reader.ReadListEnd;

    while not Reader.EndOfList do
      ReadExtra;
    Reader.ReadListEnd;
  end;

begin
  Resource := TResourceStream.Create(hInstance, 'TFORM1_EXTRA', RT_RCDATA);
  try
    Reader := TOpenReader.Create(Resource, 4096);
    try
      Reader.ReadSignature; // DFM signature

      ReadExtra;

    finally
      Reader.Free;
    end;
  finally
    Resource.Free;
  end;
end;

procedure TForm1.SetMsg(Value: string);
begin
  ShowMessage(Value);
end;

end.

