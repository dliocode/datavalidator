{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.URL.Decode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerURLDecode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerURLDecode }

constructor TSanitizerURLDecode.Create;
begin
end;

function TSanitizerURLDecode.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := THTMLEncoding.URL.Decode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
