{$BOOLEVAL OFF}{Most units depend on shortcut boolean evaluation}
{$IFDEF WIN32}{$DEFINE COMPILER2_UP}{$ENDIF}
{$IFDEF ConditionalExpressions}
  {$DEFINE COMPILER6_UP}
  {$IF CompilerVersion > 16} {Delphi 2005 or newer}
    {$DEFINE SUPPORTS_INLINE}
    {$DEFINE IGNOREFILEAGEWARNING}
  {$IFEND}
  {$IF CompilerVersion > 17} {Delphi 2006 or newer}
    {$DEFINE SUPPORTS_CLASS_ABSTRACT}
    {$DEFINE SUPPORTS_STRICT}
    {$DEFINE SUPPORTS_STATIC}
    {$DEFINE SUPPORTS_PADDING}
  {$IFEND}
  {$IF CompilerVersion > 19} {Delphi 2009 or newer}
    {$DEFINE SUPPORTS_DEPRECATEDMSG}
  {$IFEND}
  {$IF CompilerVersion > 20} {Delphi 2010 or newer}
    {$DEFINE SUPPORTS_START}
    {$DEFINE SUPPORTS_XBUTTONS}
    {$IFDEF NO_RTTI}
      { $RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {Cannot use the above, it causes an internal error when enabled and
       this include file is included above the unit directive.}
    {$ENDIF}
  {$IFEND}
{$ENDIF}


