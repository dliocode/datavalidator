{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.TrimLeft;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerTrimLeft = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerTrimLeft }

constructor TSanitizerTrimLeft.Create;
begin
end;

function TSanitizerTrimLeft.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := TrimLeft(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
