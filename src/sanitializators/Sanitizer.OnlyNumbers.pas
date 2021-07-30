{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.OnlyNumbers;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.RegularExpressions;

type
  TSanitizerOnlyNumbers = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerOnlyNumbers }

constructor TSanitizerOnlyNumbers.Create;
begin
end;

function TSanitizerOnlyNumbers.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := TRegEx.Replace(GetValueAsString, '\D', '');

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
