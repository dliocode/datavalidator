{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToUpperCase;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerToUpperCase = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerToUpperCase }

constructor TSanitizerToUpperCase.Create;
begin
end;

function TSanitizerToUpperCase.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := UpperCase(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
