{== CharactersU =======================================================}
{! <summary>
 This unit defines symbolic constants for most of the non-alphanumeric
  characters from the 7-bit ASCII character set, with a few additions
  from the Latin-1 extension.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2005-06-07</para>
<para>Last modified       2009-02-13</para>
</history>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
{======================================================================}
unit CharactersU;

interface

const
  Nul = #0;
  SOH = #1;
  STX = #2;
  ETX = #3;
  EOT = #4;
  ENQ = #5;
  ACK = #6;
  BEL = #7;
  BS  = #8;
  Backspace = #8;
  Tab = #9;
  LF  = #10;
  Linefeed = #10;
  VT  = #11;
  FF  = #12;
  Formfeed = #12;
  CR  = #13;
  CarriageReturn = #13;
  SO  = #14;
  SI  = #15;
  DLE = #16;
  DC1 = #17;
  DC2 = #18;
  DC3 = #19;
  DC4 = #20;
  NAK = #21;
  SYN = #22;
  ETB = #23;
  CAN = #24;
  EM  = #25;
  SUB = #26;
  ESC = #27;
  FS  = #28;
  GS  = #29;
  RS  = #30;
  US  = #31;
  Space = #32;
  AtSign = '@';
  DoubleQuote = '"';
  SingleQuote = '''';
  Comma = ',';
  Semicolon = ';';
  Colon = ':';
  Hyphen = '-';
  Underbar = '_';
  Dot = '.';
  Ampersand = '&';
  Percent = '%';
  Slash = '/';
  Backslash = '\';
  Equal = '=';
  Minus = '-';
  Plus  = '+';
  Hashmark = '#';
  Asterisk = '*';
  Tilde    = '~';
  Questionmark = '?';
  Exclamationmark = '!';
  Paragraph = '§';
  OpeningParanthesis = '(';
  ClosingParanthesis = ')';
  OpeningBracket = '[';
  ClosingBracket = ']';
  OpeningBrace   = '{';
  ClosingBrace   = '}';
  Dollar = '$';
  Euro   = '€';
  Pipe = '|';
  Greater = '>';
  Smaller = '<';
  Micron = 'µ';
  Sterling = '£';
  Yen   = '¥';
  Copyright = '©';
  RegisteredTrademark = '®';
  PlusMinus = '±';
  Dash = '­';
  Bullet = '•';
  Trademark = '™';
  Permille = '‰';
  Ellipsis = '…';
  CRLF = #13#10;
  NonBreakingSpace = #$A0;
  UTF16BOM = #$FEFF;


implementation

end.
