unit EmbededPanelUnit;

interface

uses Classes,
     Forms,
     ExtCtrls,
     Graphics,
     Controls;

type TFormLink = class (TComponent);

     TPaintProcedure = procedure of object;

     TEmbededFormPanel = class (TPanel)
     private
      FLink           : TFormLink;
      FLinkedForm     : TForm;
      FAlwaysVisible  : Boolean;
      FPaintProcedure : TPaintProcedure;

      procedure ShowLinkedFormImage;
      procedure ShowLinkedForm;
      procedure ReShowLinkedForm;
     protected
      procedure Paint;           override;
      procedure SetLinkedForm;   virtual;
      procedure ClearLinkedForm; virtual;

      procedure SetFormLink (const aLink : TFormLink);
     public
      constructor Create (aOwner : TComponent); override;
     published
      property  FormLink      : TFormLink read FLink          write SetFormLink;
      property  AlwaysVisible : Boolean   read FAlwaysVisible write FAlwaysVisible;
     end;

     TEmbededInstanceFormPanel = class (TEmbededFormPanel)
     private
      FFormClass : TFormClass;

      procedure CreateFormInstance;
     protected
      procedure SetLinkedForm;   override;
      procedure ClearLinkedForm; override;
     public
      property  Form : TForm read FLinkedForm; 
     end;

procedure Register;

implementation

uses Dialogs;

{ TEmbededFormPanel }

constructor TEmbededFormPanel.Create (aOwner : TComponent);
begin
 inherited;

 BevelOuter := bvNone;
end;

procedure TEmbededFormPanel.Paint;
begin
 inherited;

 if   Assigned (FPaintProcedure)
 then FPaintProcedure;
end;

procedure TEmbededFormPanel.ShowLinkedFormImage;
 var aBitmap : TBitmap;
begin
 aBitmap := FLinkedForm.GetFormImage;
 try
  Self.Canvas.Draw (0, 0, aBitmap);
 finally
  aBitmap.Free;
 end;
end;

procedure TEmbededFormPanel.ShowLinkedForm;
begin
 with FLinkedForm do
 begin
  Parent      := Self;
  Align       := alClient;
  BorderStyle := bsNone;

  Show;
 end;
end;

procedure TEmbededFormPanel.ReShowLinkedForm;

 function IsFormActive : Boolean;
  var aOuterParent : TWinControl;
      aParent      : TWinControl;
 begin
  Result       := False;
  aParent      := Parent;
  aOuterParent := aParent.Parent;

  while aOuterParent <> Nil do
  begin
   aParent      := aOuterParent;
   aOuterParent := aParent.Parent;
  end;

  try
   Result := (aParent as TForm).Active;
  except
  end;
 end;

begin
 if   (FLinkedForm.Parent <> Self) and IsFormActive
 then FLinkedForm.Parent := Self
 else
 if   FAlwaysVisible
 then ShowLinkedFormImage;
end;

procedure TEmbededFormPanel.SetLinkedForm;
begin
 FLinkedForm       := FLink.Owner as TForm;

 Self.ClientWidth  := FLinkedForm.ClientWidth;
 Self.ClientHeight := FLinkedForm.ClientHeight;
 Self.Color        := FLinkedForm.Color;
 Self.Caption      := '';

 if csDesigning	in ComponentState then
 begin
  ShowLinkedFormImage;

  FPaintProcedure := ShowLinkedFormImage
 end
 else
 begin
  ShowLinkedForm;

  FPaintProcedure := ReShowLinkedForm;
 end; 
end;

procedure TEmbededFormPanel.ClearLinkedForm;
begin
 FLink           := Nil;
 FLinkedForm     := Nil;
 FPaintProcedure := Nil;
end;

procedure TEmbededFormPanel.SetFormLink;
begin
 if   aLink = FLink
 then Exit;

 if aLink = Nil then
 begin
  ClearLinkedForm;

  Paint;

  Exit;
 end;

 if   aLink.Owner = Self.Owner
 then Exit;

 FLink := aLink;

 SetLinkedForm;
end;

{ TEmbededInstanceFormPanel }

procedure TEmbededInstanceFormPanel.CreateFormInstance;
begin
 FLinkedForm     := FFormClass.Create (Self);
 FFormClass      := Nil;
 FPaintProcedure := Nil;

 ShowLinkedForm;
end;

procedure TEmbededInstanceFormPanel.ClearLinkedForm;
begin
 if   not (csDesigning in ComponentState) 
 then FLinkedForm.Free;

 FFormClass := Nil;

 inherited;
end;

procedure TEmbededInstanceFormPanel.SetLinkedForm;
begin
 if   csDesigning	in ComponentState
 then inherited
 else
 begin
  FLinkedForm.Free;
  FLinkedForm     := Nil;
  FFormClass      := Nil;
  FPaintProcedure := Nil;

  FFormClass      := TFormClass ((FLink.Owner as TForm).ClassType);
  FPaintProcedure := CreateFormInstance;
 end;
end;

procedure Register;
begin
 RegisterComponents ('Embedded Form Panel', [TEmbededFormPanel, TEmbededInstanceFormPanel, TFormLink]);
end;

end.
