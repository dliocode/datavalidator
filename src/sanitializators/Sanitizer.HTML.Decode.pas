{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.HTML.Decode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerHTMLDecode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerHTMLDecode }
constructor TSanitizerHTMLDecode.Create;
begin
end;

function TSanitizerHTMLDecode.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := THTMLEncoding.HTML.Decode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
