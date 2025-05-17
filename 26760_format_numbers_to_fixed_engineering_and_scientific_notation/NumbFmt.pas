unit NumbFmt;

{Conversion started on 10th March 2009}
{The original methods where written in turbo-pascal and then converted to}
{Delphi oop; however, several modifications have been made. The multitude of}
{global variables was reduced by using a record. Many of the original}
{methods where changed to cope with this idea, also several bugs where fixed}
{Version set at 1.0.0.0 and is as complete as possible as of the 23/03/2009}
{The two public methods take a real number, or its string representation and}
{format it according to the given precision (number of decimal places) and}
{type - nfNone, nfScientific, nfEngineering or nfFixed. These are the typical}
{formats found on a scientific calculator. This unit is released to the}
{public as is, to be changed or used as one sees fit. The author takes no}
{responsibility for any errors}

interface
uses
 SysUtils, StdCtrls, Dialogs;

type
  TNumberFormat = (nfNone, nfScientific, nfEngineering, nfFixed);

  NFCharSet=set of char; //number format character

  TNumberRec=record
    NumberStr: string;
    ExponentialStr: string;
    Exponential: integer;
    NegativeNumber: integer;
    NegativeExponential: integer;
    DecimalPos: integer;
    DecimalPoint: integer;
    private
      procedure Initialize; //initialize all variables
  end;

  TNumberFormatter = class(TObject)
  private
    NR: TNumberRec;
    FPrecision: integer;
    procedure RndNum;
    procedure LeadZeroFix;
    procedure Separate(PStr: string);
    procedure decRshift;
    procedure decLshift;
    {Returns the string number formatted to scientific notation and precision}
    function Scientific: string;
    {Returns the string number formatted to engineering notation}
    function Engineering: string;
    {Returns the string number formatted without an exponential if possible}
    function Fixed: string;
    function ConstructNumberStr:string;
    procedure NumberStrToScientific;
  public
    function FormatNumberString(NumberString: string; Precision: integer;
                     NumberFormat: TNumberFormat): string;
    function FormatNumber(Number: extended; Precision: integer;
                     NumberFormat: TNumberFormat): string;
  end;


implementation

{initialize values used by the number record}
procedure TNumberRec.Initialize;
begin
  NumberStr:='';//null number string
  ExponentialStr:='';//null exponential string
  Exponential:=0;//set to 0 to begin with
  NegativeNumber:=0;//number is positive 1=negative
  NegativeExponential:=0;//exponential is positive 1=negative
  DecimalPos:=0;//decimal point is at first point
  DecimalPoint:=0;//there is no deciaml point
end;

{Routine that rounds string number (without its exponential)}
{to a given number of decimal places<18}
procedure TNumberFormatter.RndNum;
var
  i: integer;
  Strlength: integer;
  done, Cflag: boolean; //Cflag = carry over flag
begin
  Strlength:=length(NR.NumberStr);
  {i:=1;}
  i:=NR.DecimalPos;
  {check to see if FPrecision is greater than 18 decimal places and correct}
  if FPrecision+i>18 then FPrecision:=18-i;
  {add zero's if necessary}
  while Strlength<FPrecision+NR.DecimalPoint+i do
  begin
    NR.NumberStr:=NR.NumberStr+'0';
    inc(Strlength)
  end;
  {do actual rounding off}
  if Strlength>i+FPrecision then
  begin
    i:=i+FPrecision+{NR.DecimalPoint}+1;
    if (NR.NumberStr[i]>='5') then
    begin
      dec(i);
      delete(NR.NumberStr,i+1,Strlength-i);
      Cflag:=true;
      done:=false;
      while not(done) do
      begin
        if (Cflag=true) then
        begin
          if (Cflag=true) and (NR.NumberStr[i]='-') then
          begin
            insert('1',NR.NumberStr,i+1);done:=true
          end
          else
          begin
            if (Cflag=true) and (i=0) then
            begin
              insert('1',NR.NumberStr,i+1);
              done:=true;
            end
            else
            begin
              if (NR.NumberStr[i]='.') or (NR.NumberStr[i]='-')
              then dec(i)
              else
              begin
                inc(NR.NumberStr[i]);
                if NR.NumberStr[i]>'9' then
                begin
                  NR.NumberStr[i]:='0';
                  Cflag:=true;
                  dec(i)
                end
                else
                begin
                  Cflag:=false;
                  done:=true
                end;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      if NR.DecimalPoint=1 then i:=NR.DecimalPos+FPrecision
      else dec(i);
      delete(NR.NumberStr,i+1,Strlength-i);
    end;
  end;
end;

{get rid of any unnecessary leading zeros}
procedure TNumberFormatter.LeadZeroFix;
var
 k: integer;
 done: boolean;
begin
  k:=1;done:=false;
  while not(done) do
  begin
    if k+1<=length(NR.NumberStr) then
    begin
      if (NR.NumberStr[k+NR.NegativeNumber]='0') and (k+1<NR.DecimalPos) then
      begin
        delete(NR.NumberStr,k+NR.NegativeNumber,1);
        dec(NR.DecimalPos);
      end
      else done:=true;
    end;
  end;
end;

{separate number string into number and exponent}
{and sets variables decpos, negsign and StrNumbL}
procedure TNumberFormatter.Separate(PStr: string);
var
  i: integer;
  ExponSet: NFCharSet;
begin
  NR.Initialize;//reset all the values to default
  i:=1;
  ExponSet:=['E','e'];
  while (i<=length(PStr)) and (not(PStr[i]in ExponSet)) do
  begin
    NR.NumberStr:=NR.NumberStr + PStr[i];
    if PStr[i] = '.' then
    begin
      NR.DecimalPos:=i;
      NR.DecimalPoint:=1; //add a position for the decimal point
    end;
    inc(i);
  end;
  if (PStr[i] in ExponSet) then
  begin
    inc(i);
    while (i<=length(PStr)) do
    begin
      NR.ExponentialStr:=NR.ExponentialStr + PStr[i];
      inc(i);
    end;
  end;
  if NR.NumberStr[length(NR.NumberStr)-1]='.'
  then NR.NumberStr:=NR.NumberStr+'0000';
  if NR.DecimalPoint=0 then
  begin
    NR.DecimalPos:=length(NR.NumberStr)+1;
    NR.NumberStr:=NR.NumberStr+'.0000';
    NR.DecimalPoint:=1;
  end;
  if NR.NumberStr[1]='-' then NR.NegativeNumber:=1;
  if NR.NumberStr[1+NR.NegativeNumber]='.' then
  begin
    insert('0',NR.NumberStr,1+NR.NegativeNumber);
    inc(NR.DecimalPos);
  end;
  if NR.ExponentialStr<>'' then
  begin
   NR.Exponential:=StrToInt(NR.ExponentialStr);
   NR.NegativeExponential:=1;
  end;
end;

{Shift the decimal point right in the string}
procedure TNumberFormatter.decRshift;
begin
  if NR.DecimalPos=length(NR.NumberStr) then NR.NumberStr:=NR.NumberStr+'0';
  NR.NumberStr[NR.DecimalPos]:=NR.NumberStr[NR.DecimalPos+1];
  inc(NR.DecimalPos);
  dec(NR.Exponential);
  if NR.Exponential<0 then NR.NegativeExponential:=1;
  NR.NumberStr[NR.DecimalPos]:='.';
  if NR.NumberStr[1+NR.NegativeNumber]='0' then
  begin
    delete(NR.NumberStr,1+NR.NegativeNumber,1);
    dec(NR.DecimalPos)
  end;
end;

{Shift the decimal point left in the string}
procedure TNumberFormatter.decLshift;
begin
  if NR.DecimalPos=1 then //.1245 becomes 0.1245
  begin
    insert('0',NR.NumberStr,1);
    inc(NR.DecimalPos);
  end;
  NR.NumberStr[NR.DecimalPos]:=NR.NumberStr[NR.DecimalPos-1];
  dec(NR.DecimalPos);
  inc(NR.Exponential);
  NR.NumberStr[NR.DecimalPos]:='.';
  if NR.NumberStr[1+NR.NegativeNumber]='.' then
  begin
    insert('0',NR.NumberStr,1+NR.NegativeNumber);
    inc(NR.DecimalPos);
  end;
end;

function TNumberFormatter.Scientific: string;
begin
  NumberStrToScientific;
  RndNum;
  result:=ConstructNumberStr;
end;

function TNumberFormatter.Engineering: string;
var
 decshift, k: integer;
begin
  NumberStrToScientific;
  {Old engineering routine}
  decshift:=0;
  if NR.Exponential<0 then decshift:=3-round(frac(abs(NR.Exponential)/3.001)*3);
  if NR.Exponential>0 then decshift:=round(frac(NR.Exponential/2.999)*3);
  if NR.DecimalPos-NR.NegativeNumber-1>3-decshift then
  begin
    for k:=3-decshift downto 1 do decLshift
  end
  else
  begin
    for k:=decshift downto 1 do decRshift
  end;
  RndNum; //round the numberstr part of the record
  result:=ConstructNumberStr;
end;

function TNumberFormatter.Fixed: string;
begin
  if NR.Exponential=0 then
  begin
    RndNum;
    result:=NR.NumberStr;
  end
  else
  begin
    if NR.Exponential>0 then
    begin
      if NR.DecimalPos+FPrecision+1+NR.Exponential>18 then
      begin
        RndNum;
        result:=NR.NumberStr+'E'+NR.ExponentialStr;
      end
      else
      begin
        while NR.Exponential>0 do decRshift;
        RndNum;
        result:=NR.NumberStr;
      end;
    end;
    if NR.Exponential<0 then
    begin
      if abs(NR.Exponential)>18 then result:='0'
      else
      begin
        while NR.Exponential<0 do decLshift;
        RndNum;
        result:=NR.NumberStr;
      end;
    end;
  end;
end;

function TNumberFormatter.FormatNumber(Number: extended; Precision: integer;
  NumberFormat: TNumberFormat): string;
var
  TempStr: string;
begin
  try
    TempStr:=FloatToStr(Number);
    FPrecision:=Precision;//make this global to all methods in NumbFmt
    separate(TempStr);
    case NumberFormat of
      nfNone: result:=TempStr;
      nfScientific: result:=Scientific;
      nfEngineering: result:=Engineering;
      nfFixed: result:=Fixed;
    end;
  except
    on EconvertError do
      Messagedlg('Attempt to process an invalid number',mtWarning,[mbOk],0);
  end;
end;

procedure TNumberFormatter.NumberStrToScientific;
begin
  if (NR.DecimalPos+NR.NegativeNumber>1)
  and (NR.NumberStr[NR.NegativeNumber+1]>'0')
  then begin
    while NR.DecimalPos > 2 + NR.NegativeNumber do decLShift;
  end
  else
  begin
    repeat
      decRshift;
    until (NR.NumberStr[NR.DecimalPos - 1] > '0')
       or (NR.DecimalPos = length(NR.NumberStr));
    LeadZeroFix; //remove any unnecessary leading zeros
  end;
end;

function TNumberFormatter.ConstructNumberStr:string;
begin
  if NR.Exponential = 0
  then NR.ExponentialStr := ''
  else NR.ExponentialStr := IntToStr(NR.Exponential);
  if (FPrecision = 0)
  then delete(NR.NumberStr, NR.DecimalPos, 1);
  if NR.ExponentialStr = '' then
    result := NR.NumberStr
  else
    result := NR.NumberStr + 'E' + NR.ExponentialStr;
end;

function TNumberFormatter.FormatNumberString(NumberString: string; Precision: integer;
  NumberFormat: TNumberFormat): string;
begin
  try
    StrToFloat(NumberString);
    FPrecision:=Precision;//make this global to all methods in NumbFmt
    separate(NumberString);
    case NumberFormat of
      nfNone: result:=NumberString;
      nfScientific: result:=Scientific;
      nfEngineering: result:=Engineering;
      nfFixed: result:=Fixed;
    end;
  except
    on EconvertError do
      Messagedlg('Attempt to process an invalid number',mtWarning,[mbOk],0);
  end;
end;

end.
