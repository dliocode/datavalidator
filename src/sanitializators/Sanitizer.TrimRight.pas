{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.TrimRight;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerTrimRight = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerTrimRight }

constructor TSanitizerTrimRight.Create;
begin
end;

function TSanitizerTrimRight.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := TrimRight(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
