{
  Interfaces + Classes is greater than the sum

  Written by Wayne Niddery, February 2001
  This sample unit may be freely copied, used, and distributed.

  This unit demonstrates use of the interfaces defined in the
  WCIntf unit.
}

unit WCSample1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WCIntf, StdCtrls;

type
  TForm1 = class(TForm)
    btnTime: TButton;
    labTime: TLabel;
    edNum: TEdit;
    btnAdd: TButton;
    btnSubtract: TButton;
    labTotal: TLabel;
    btnStart: TButton;
    labStop: TLabel;
    btnAdd10: TButton;
    labElapsed10: TLabel;
    labStart: TLabel;
    btnStop: TButton;
    labElapsed: TLabel;
    procedure btnTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSubtractClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnAdd10Click(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  public
    // note the use of the interface type, not the class type
    ElapsedTime: IElapsedTime;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // create an implementation of the interface
  ElapsedTime := TElapsedTime.Create;
  // because this class descends from TInterfacedObject
  // it is reference counted and will automatically be destroyed
  // when the application closes
end;

procedure TForm1.btnTimeClick(Sender: TObject);
begin
  // access the IClock interface
  labTime.Caption := TimeToStr(
    (ElapsedTime as IClock).CurrentTime);
end;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  // access the ICalculator interface
  labTotal.Caption := IntToStr(
    (ElapsedTime as ICalculator).Add(StrToInt(edNum.Text)));
end;

procedure TForm1.btnSubtractClick(Sender: TObject);
begin
  // access the ICalculator interface
  labTotal.Caption := IntToStr(
    (ElapsedTime as ICalculator).Subtract(StrToInt(edNum.Text)));
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  // Start time
  labStart.Caption := TimeToStr(ElapsedTime.Start);
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  // Stop time
  labStop.Caption := TimeToStr(ElapsedTime.Stop);
  // show elapsed seconds
  labElapsed.Caption :=
    IntToStr(ElapsedTime.ElapsedSeconds) + ' seconds';
end;

procedure TForm1.btnAdd10Click(Sender: TObject);
begin
  // Add 10 seconds to the last calculated elapsed time
  // This is done by accessing the ICalculator interface
  // of ElapsedTime
  labElapsed10.Caption := IntToStr(
    (ElapsedTime as ICalculator).Add(10)) + ' seconds';
end;

end.
