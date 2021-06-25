{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToLowerCase;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerToLowerCase = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerToLowerCase }

constructor TSanitizerToLowerCase.Create;
begin
end;

function TSanitizerToLowerCase.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := LowerCase(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
