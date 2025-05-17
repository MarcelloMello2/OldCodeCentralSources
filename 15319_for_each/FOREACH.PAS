unit forEach;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$OPTIMIZATION OFF}
procedure ForEach(const A: array of Integer; Proc: Pointer); overload;
// Proc prototype:  procedure (I: Integer); far;
var Index: Integer;
    Value: Integer;
    CallerBP: Cardinal;
BEGIN
  asm
    mov   EAX,[ebp]
    mov   CallerBP,EAX
  end;
  for Index := Low(A) to High(A) do begin
    Value := A[Index];
    asm
      mov   EAX,Value
      push  CallerBP
      call  Proc
      pop   EAX
    end;
  end;
END;
{$OPTIMIZATION ON}

{$OPTIMIZATION OFF}
procedure ForEach(const A: array of TObject; Proc: Pointer); overload;
// Proc prototype:  procedure (I: Integer); far;
var Index: Integer;
    Value: TObject;
    CallerBP: Cardinal;
BEGIN
  asm
    mov   EAX,[ebp]
    mov   CallerBP,EAX
  end;
  for Index := Low(A) to High(A) do begin
    Value := A[Index];
    asm
      mov   EAX,Value
      push  CallerBP
      call  Proc
      pop   EAX
    end;
  end;
END;
{$OPTIMIZATION ON}

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
  procedure CaptionIt(Obj: TObject); far;
  begin
    Caption := Caption +' '+Obj.ClassName;
  end;
var
  A : array[1..2] of TObject;
begin
  A[1] := Self;
  A[2] := Application;
  Caption := '';
  ForEach(A,@CaptionIt);
end;

end.
