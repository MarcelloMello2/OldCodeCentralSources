
{ ****************************************** }
{ 					Delphi Runtime Library           }
{              Unit CryptoStream             }
{              for EMBARCADERO ®             }
{          © Calabrese Luigi 2010            }
{             Release 1.0.0.0                }
{         Last revision: 08/10/2010          }
{ ****************************************** }

{ The simple object TCryptoStream, can effectively replace TFileStream, from 
	which it derives, anywhere a simple but efficient way of codifying of the data
	is required by to save on file. In fact it is projected to work 
	equally of TFileStream if it notices that the work file is not type 
	TCryptoStream. In this case it behaves in completely transparent way.
	The key of coding 'FKey', is opportunely chosen by the object in automatic way
	as soon as the first data are written (method 'Write') and is opportunely 
	saved in the file so that to immediately be recognized in phase of decoding 
	of the data (method 'Read') without possibility of bugs. }

unit CryptoStream;

{$R-,T-,X+,H+,B-}

interface

uses

{$IFDEF MSWINDOWS}
	Windows, 
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Types, PosixSysTypes, PosixUniStd, PosixSignal,
{$ENDIF POSIX}
{$IFDEF MACOS}
  CoreServices,
{$ENDIF MACOS}
  SysUtils, Classes; 

type

{ TCryptoStream }

	TCryptoStream = class sealed(TFileStream)
	strict private
		FKey: Char;
		FIsFirstRead: Boolean;
		FIsFirstWrite: Boolean;
		FIsCripted: Boolean;
		function IsCrypted: Boolean;	
	public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
		function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;
	
implementation

{ TCryptoStream }

constructor TCryptoStream.Create(const AFileName: string; Mode: Word);
begin
	inherited Create(AFileName, Mode);
	FKey := #$A4; // Default;
	FIsFirstRead := True;
	FIsFirstWrite := True;
	FIsCripted := IsCrypted;
end;

constructor TCryptoStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
	inherited Create(AFileName, Mode, Rights);
	FKey := #$A4; // Default;
	FIsFirstRead := True;
	FIsFirstWrite := True;
	FIsCripted := IsCrypted;
end;

destructor TCryptoStream.Destroy;
var
	CryptHeader: string; 
begin
	if FIsCripted and (not FIsFirstWrite) then
	begin
		Position := Size;
		CryptHeader := 'kr<' + FKey + '>kr';
		inherited Write(Pointer(CryptHeader)^, Length(CryptHeader) * SizeOf(Char));
		inherited Write(FKey, SizeOf(Char));
	end;
	inherited Destroy;
end;

function TCryptoStream.IsCrypted: Boolean;
var
	TotalBytes: Int64;
	H, CryptHeader: string;
	K: Char;
begin
	Result := False;
	CryptHeader := 'kr<x>kr';
	TotalBytes := Int64(Length(CryptHeader) * SizeOf(Char) + SizeOf(Char));
	if Size < TotalBytes then Exit;
	Position := Size - TotalBytes;
	SetLength(H, Length(CryptHeader));
	inherited Read(Pointer(H)^, Length(CryptHeader) * SizeOf(Char));
	inherited Read(K, SizeOf(Char));
	Position := 0;
	CryptHeader[4] := K;
	Result := AnsiSameText(H, CryptHeader) and (H[4] = K);
end;

function TCryptoStream.Read(var Buffer; Count: Integer): Longint;
var
	Buf, CryptBuf: array of Byte;  // no use TBytes type for backward 
	// compatibility RTL compilers version
	I, CountRead: Integer;
	CryptVal: Byte;
	SavedPosition: Int64;
begin
	if not FIsCripted then
	begin
		Result := inherited Read(Buffer, Count);
		Exit;
  end;	
	SetLength(CryptBuf, Count);
	SetLength(Buf, Count);
	try
		CountRead := inherited Read(Pointer(CryptBuf)^, Count);
		if (CountRead <= 0) then 
		begin
			Result := 0;
			Exit;
    end;
		if FIsFirstRead then
		begin
			if (Size > Int64(SizeOf(Char))) then
			begin
				SavedPosition := Position;
				Position := Size - Int64(SizeOf(Char));
				inherited Read(FKey, SizeOf(Char));
				Position := SavedPosition;
			end;
			FIsFirstRead := False;	
    end;		
		for I := 0 to CountRead - 1 do
		begin
			CryptVal := Byte(CryptBuf[I] - Byte(FKey)) mod 256;
			Buf[I] := CryptVal;
		end;
		System.Move(Pointer(Buf)^, Buffer, CountRead);
	finally
		if Buf <> nil then Finalize(Buf);
		if CryptBuf <> nil then Finalize(CryptBuf);
	end;
	Result := CountRead;
end;

function TCryptoStream.Write(const Buffer; Count: Integer): Longint;
var
	Buf, CryptBuf: TBytes; // no use TBytes type for backward 
	// compatibility RTL compilers version
	I: Integer;
	CryptVal: Byte;
	Temp: Char;
begin
	SetLength(CryptBuf, Count);
	SetLength(Buf, Count);
	try
		System.Move(Buffer, Pointer(Buf)^, Count);
		if (Length(Buf) <= 0) then
		begin
			Result := 0;
			Exit;
    end;	
		if FIsFirstWrite then
		begin
			Temp := Char(Buf[0]);
			if Integer(Temp) > Integer($A4) then FKey := Temp;
			FIsFirstWrite := False;
			FIsCripted := True;
    end;	
		for I := 0 to Count - 1 do
		begin
			CryptVal := Byte(Buf[I] + Byte(FKey)) mod 256;
			CryptBuf[I] := CryptVal;
		end;
		Result := inherited Write(Pointer(CryptBuf)^, Count);	
	finally
		if Buf <> nil then Finalize(Buf);
		if CryptBuf <> nil then Finalize(CryptBuf);
	end;
end;

end.
