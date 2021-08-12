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
  LValue: string;
  LValueDecode: string;
begin
  LValue := GetValueAsString;

  try
    LValueDecode := THTMLEncoding.HTML.Decode(LValue);
  except
    LValueDecode := LValue;
  end;

  SetValueAdapter(LValueDecode);

  Result := FValue;
end;

end.
