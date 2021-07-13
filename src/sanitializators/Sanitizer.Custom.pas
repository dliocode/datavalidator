{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.Custom;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.RegularExpressions;

type
  TSanitizerCustom = class(TDataValidatorItemBaseSanitizer)
  private
    FExecute: TDataValidatorCustomSanitizerExecute;
  public
    function Sanitize: TValue; override;
    constructor Create(const AExecute: TDataValidatorCustomSanitizerExecute);
  end;

implementation

{ TSanitizerCustom }

constructor TSanitizerCustom.Create(const AExecute: TDataValidatorCustomSanitizerExecute);
begin
  FExecute := AExecute;
end;

function TSanitizerCustom.Sanitize: TValue;
var
  LValue: TValue;
begin
  if Assigned(FExecute) then
  begin
    LValue := FExecute(GetValueAsString);
    SetValueAdapter(LValue);
  end;

  Result := FValue;
end;

end.
