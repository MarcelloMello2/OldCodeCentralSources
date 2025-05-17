unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SampleGCThread, StdCtrls, ComCtrls, AppEvnts, ExtCtrls;

type
  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnNewThread: TButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    rgSweepMode: TRadioGroup;
    chbNewThreads: TCheckBox;
    rgBlockGranularity: TRadioGroup;
    rgCacheCapacity: TRadioGroup;
    gbSampleClasses: TGroupBox;
    chbStatic: TCheckBox;
    chbDynamic: TCheckBox;
    chbConvoluted: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnNewThreadClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fSweepThread: tSweepThread;
    fStartTick: Cardinal;
    procedure DisableOptions;
    procedure EnableOptions;
    procedure AbleOptions(aEnable: Boolean);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  stGC, SampleGCObjects, SampleGCObjects2;

{$R *.dfm}

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled := False;
  DisableOptions;
  btnNewThread.Enabled := True;
  btnStop.Enabled := True;
  tGCManager.GetDefault.SetUseLocks(rgSweepMode.ItemIndex = 1);
  tSweepThread.NoNewThreads(not chbNewThreads.Checked);
  tSweepThread.EnableStatic(chbStatic.Checked);
  tSweepThread.EnableDynamic(chbDynamic.Checked);
  tSweepThread.EnableConvoluted(chbConvoluted.Checked);
  case rgBlockGranularity.ItemIndex of
    0: tGCManager.GetDefault.SetBlockGranularity(8);
    1: tGCManager.GetDefault.SetBlockGranularity(16);
    2: tGCManager.GetDefault.SetBlockGranularity(32);
    3: tGCManager.GetDefault.SetBlockGranularity(64);
  end;
  case rgCacheCapacity.ItemIndex of
    0: tGCManager.GetDefault.SetCacheCapacity(0);
    1: tGCManager.GetDefault.SetCacheCapacity(100000);
    2: tGCManager.GetDefault.SetCacheCapacity(200000);
  end;
  FreeAndNil(fSweepThread);
  fSweepThread := tSweepThread.Create(False);
  tSweepThread.ResetSampleStepCount;
  fStartTick := GetTickCount;
  tSampleThread.Create(0);
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  btnNewThread.Enabled := False;
  tSweepThread.TerminateAll(True);
  if Assigned(fSweepThread) then begin
    fSweepThread.Terminate;
    fSweepThread.WaitFor;
    FreeAndNil(fSweepThread);
  end;
  tGCManager.GetDefault.Sweep;
  EnableOptions;
  btnStart.Enabled := True;
end;

procedure TfrmMain.DisableOptions;
begin
  AbleOptions(False);
end;

procedure TfrmMain.EnableOptions;
begin
  AbleOptions(True);
end;

procedure TfrmMain.AbleOptions(aEnable: Boolean);
begin
  rgSweepMode.Enabled := aEnable;
  rgBlockGranularity.Enabled := aEnable;
  rgCacheCapacity.Enabled := aEnable;
  chbNewThreads.Enabled := aEnable;
  gbSampleClasses.Enabled := aEnable;
  chbStatic.Enabled := aEnable;
  chbDynamic.Enabled := aEnable;
  chbConvoluted.Enabled := aEnable;
end;

procedure TfrmMain.btnNewThreadClick(Sender: TObject);
begin
  tSampleThread.Create(0);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Threads: ' +
    IntToStr(tSweepThread.GetThreadCount) +
    '(' + IntToStr(tSweepThread.GetActiveThreadCount) + ')';
  if Assigned(fSweepThread) then begin
    StatusBar1.Panels[1].Text := 'Sweep Time: ' +
      IntToStr(fSweepThread.MaxTicks) + '/' +
      IntToStr(fSweepThread.AvgTicks) + '/' +
      IntToStr(fSweepThread.LastTicks) +
      ' ms (max/avg/last)';
    StatusBar1.Panels[3].Text := 'Steps: ' +
      IntToStr((1000*tSweepThread.GetSampleStepCount) div Integer(GetTickCount - fStartTick)) +
      ' steps/s';
  end;
  StatusBar1.Panels[2].Text := 'Instances: ' + IntToStr(tGCManager.GetDefault.ObjectCount);
end;

end.
