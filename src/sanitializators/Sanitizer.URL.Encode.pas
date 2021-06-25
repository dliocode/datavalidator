{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.URL.Encode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerURLEncode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerURLEncode }

constructor TSanitizerURLEncode.Create;
begin
end;

function TSanitizerURLEncode.Sanitize: TValue;
var
  LValue: string;
begin
  LValue := THTMLEncoding.URL.Encode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
