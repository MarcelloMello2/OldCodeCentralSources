{*------------------------------------------------------------------------------
 Component Driver - Mainline for Programmer's Workbench

 This establishes the package and driver for the component.

 @Author    Mr. Arch Brooks
 @Version   2010.08.09   Mr. Arch Brooks Initial revision
 -------------------------------------------------------------------------------}

unit BCSPwbdp;

interface

uses
  BCSPwbdmu, BCSPwbwbu, Classes, Forms, Graphics, SysUtils;

type

{*------------------------------------------------------------------------------
  Main Class for Component

  This is the primary class used by this oomponent.
------------------------------------------------------------------------------*}

    TBCSPwbdp = class(TComponent)
  private
    {Private declarations}
  protected
    {Protected declarations}
    /// Dialog Color Variable
    FColor: TColor;
    /// Connectin String Variable
    FConStr: String;
    /// Table Name Variable
    FTabName: string;
  public
    {Public declarations}
  published
    {Published declarations}
    property RColor: TColor read FColor write FColor;
    function Execute: Boolean;
    property RConnStr: string read FConStr write FConStr;
    property RTabName: string read FTabName write FTabName;
  end;

procedure Register;

implementation


{*------------------------------------------------------------------------------
  Execute Function

  This is the default function for this component.
------------------------------------------------------------------------------*}

  function TBCSPwbdp.Execute: Boolean;
begin
  Result := True;
  Application.CreateForm(TBCSPwbC, BCSPwbC);
  Application.CreateForm(TBCSPwbD, BCSPwbD);
  BCSPwbC.RColor := RColor;
  BCSPwbD.RConnStr := RConnStr;
  BCSPwbD.RTabName := RTabName;
  BCSPwbD.OpenRDS;
  BCSPwbC.ShowModal;
  BCSPwbD.CloseRDS;
  BCSPwbD.Free;
  BCSPwbC.Free;
end;


{*------------------------------------------------------------------------------
  Register Procedure

  This Procedure identifies the palate where the component will reside.

------------------------------------------------------------------------------*}

  procedure Register;
begin
  RegisterComponents('AB Comps', [TBCSPwbdp]);
end;

end.
