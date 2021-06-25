{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.Replace;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerReplace = class(TDataValidatorItemBaseSanitizer)
  private
    FOldValue: string;
    FNewValue: string;
  public
    function Sanitize: TValue; override;
    constructor Create(const AOldValue: string; const ANewValue: string);
  end;

implementation

{ TSanitizerReplace }

constructor TSanitizerReplace.Create(const AOldValue: string; const ANewValue: string);
begin
  FOldValue := AOldValue;
  FNewValue := ANewValue;
end;

function TSanitizerReplace.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := GetValueAsString.Replace(FOldValue, FNewValue);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
