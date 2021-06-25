{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.Base64.Decode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerBase64Decode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerBase64Decode }

constructor TSanitizerBase64Decode.Create;
begin
end;

function TSanitizerBase64Decode.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := TNetEncoding.Base64.Decode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
