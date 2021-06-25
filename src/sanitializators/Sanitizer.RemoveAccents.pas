{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.RemoveAccents;

interface

uses
  DataValidator.ItemBase.Sanitizer;

type
  TSanitizerRemoveAccents = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerRemoveAccents }

constructor TSanitizerRemoveAccents.Create;
begin
end;

function TSanitizerRemoveAccents.Sanitize: TValue;
type
  ASCIIString = type AnsiString(20127);
var
  LValue: string;
begin
  LValue := string(ASCIIString(GetValueAsString));

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
