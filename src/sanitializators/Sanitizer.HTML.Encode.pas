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
  LValue: string;
  LValueEncode: string;
begin
  LValue := GetValueAsString;

  try
    LValueEncode := THTMLEncoding.HTML.Encode(LValue);
  except
    LValueEncode := LValue;
  end;

  SetValueAdapter(LValueEncode);

  Result := FValue;
end;


end.
