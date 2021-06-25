{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.Base64.Encode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerBase64Encode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerBase64Encode }

constructor TSanitizerBase64Encode.Create;
begin
end;

function TSanitizerBase64Encode.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := TNetEncoding.Base64.Encode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
