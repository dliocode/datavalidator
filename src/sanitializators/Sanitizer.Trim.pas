{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.Trim;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerTrim = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerTrim }

constructor TSanitizerTrim.Create;
begin
end;

function TSanitizerTrim.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := Trim(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
