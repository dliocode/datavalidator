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
  LValueDecode: string;
begin
  LValue := GetValueAsString;

  try
    LValueDecode := THTMLEncoding.URL.Decode(LValue);
  except
    LValueDecode := LValue;
  end;

  SetValueAdapter(LValueDecode);

  Result := FValue;
end;

end.
