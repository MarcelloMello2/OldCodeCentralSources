{ *********************************************************************** }
{                                                                         }
{ AltEncoding Unit ver1.00                                                }
{ Copyright (c) 2010  Hideaki Tominaga (DEKO)                             }
{                                                                         }
{ *********************************************************************** }
unit AltEnc;
 
interface
 
uses
  Windows, SysUtils;
 
type
  TAltEncoding = class(TMBCSEncoding)
  private
    FCodePage: Cardinal;
  strict protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
  public
    constructor Create(CodePage: Integer); virtual;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
  end;
 
implementation
 
{ TAltEncoding }
 
constructor TAltEncoding.Create(CodePage: Integer);
begin
  FCodePage := CodePage;
  inherited Create(CodePage, 0, 0);
end;
 
function TAltEncoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
var
  NeedFix: Boolean;
  BufSize: Integer;
  Buf: RawByteString;
begin
  NeedFix := False;
  if not CheckWin32Version(5, 1) then
    case FCodePage of
      50220, // ISO-2022-JP
      50221, // csISO2022JP
      50222, // iso-2022-jp
      50225, // iso-2022-kr
      50227, // x-cp50227
      50229: // iso-2022-tw
        NeedFix := True;
    else
      NeedFix := False;
    end;
  if NeedFix then
    begin
      BufSize := (CharCount * 5) + 4 - (CharCount div 2);
      SetLength(Buf, BufSize);
      Result := WideCharToMultiByte(FCodePage, 0, PChar(Chars), CharCount, PAnsiChar(Buf), BufSize, nil, nil);
    end
  else
    Result := inherited GetByteCount(Chars, CharCount);
end;
 
function TAltEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount * 5) + 4 - (CharCount div 2);
end;
 
function TAltEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;
 
end.
