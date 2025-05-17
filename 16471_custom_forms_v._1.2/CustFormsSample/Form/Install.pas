unit Install;

interface
uses
  Classes, DsgnIntf, Font_Form, Font_Form2;

procedure Register;

implementation

procedure Register;
begin
  RegisterCustomModule(TBaseForm, TCustomModule);
  RegisterCustomModule(TBaseForm2, TCustomModule);
end; 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
end.
