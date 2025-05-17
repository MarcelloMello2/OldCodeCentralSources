unit UValidaN;

interface

uses sysutils, classes;

type
 // CalssObj Pascal Para validação de, NIF, BI, NISS
 // fcarvalho98@gmail.com

 TValidaNumeros=class
  protected
   FNumero : string;
   function Get_isValidoNIF:boolean;
   function Get_isValidoNISS:boolean;
   function Get_isValidoBI:boolean;
  public
   constructor create;
   property Numero:string read FNumero write FNumero;
   property isValidoNIF: boolean read Get_isValidoNIF;
   property isValidoNISS: boolean read Get_isValidoNISS;
   property isValidoBI: boolean read Get_isValidoBI;

   function IsNumero(s: string): Boolean;
 end;

implementation

function TValidaNumeros.IsNumero(s: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(s) do
    case s[i] of '0'..'9':;
     else Exit;
    end;
  Result := True;
end;

// Função para validar Nº idenf. Fiscal
function TValidaNumeros.Get_isValidoNIF:boolean;
var
  checkdigito, i : integer;
begin
  result := false;
  if (length(FNumero) = 9) and (isNumero(FNumero)) then
   begin
    if (FNumero[1] in ['1','2','5','6','8','9'] )  then
     begin
         checkdigito := strtoint(FNumero[1]) * 9;
        for i := 3 to 9 do
         checkdigito := checkdigito + (strtoint(FNumero[i-1])  * (11 - i));
         checkdigito := 11 - (checkDigito mod 11);
        If (checkDigito >= 10) then checkDigito := 0;
        If (checkDigito = strtoint(FNumero[9])) then result := True;
      end;
    end
   else
    result := false;
end;


// Função para validar Nº idenf. Seg social
function TValidaNumeros.Get_isValidoNISS:boolean;
const
 factor : array[1..10] of byte = (29,23,19,17,13,11,7,5,3,2);
var
 soma, i : integer;
begin
  soma := 0;
  if (length(FNumero) = 11) and (isNumero(FNumero)) then
   begin
    if (FNumero[1] in ['1','2'] ) then
     begin
        for i := 1 to 10 do
         soma  := soma + ((strtoint(FNumero[i])  * factor[i]));
         If (strtoint(FNumero[11]) = (9 - (soma mod 10))) then result := True;
      end;
    end
   else
    result := false;
end;


// Valida
constructor TValidaNumeros.create;
begin
 FNumero := '123456789';
end;

function TValidaNumeros.Get_isValidoBI:boolean;
var
  soma : integer;
  TamanhoBI, i, cont : byte;
begin
  soma := 0;
  TamanhoBI := length(FNumero);
  if ((tamanhoBI > 6) and (TamanhoBI < 11)) and (isNumero(FNumero)) then
   begin
        cont := 0;
        for i := TamanhoBI downto 1 do
         begin
          inc(cont);
          soma  := soma + ((strtoint(FNumero[i])  * cont));
         end;
         if ((Soma mod 11 = 0) or (soma mod 11 = 10)) then result := True;
    end
   else
    result := false;
end;


end.
