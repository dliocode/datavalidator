{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.HTML.Encode;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.NetEncoding;

type
  TSanitizerHTMLEncode = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerHTMLEncode }

constructor TSanitizerHTMLEncode.Create;
begin
end;

function TSanitizerHTMLEncode.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := THTMLEncoding.HTML.Encode(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
