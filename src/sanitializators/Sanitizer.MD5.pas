{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.MD5;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.Hash;

type
  TSanitizerMD5 = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerMD5 }

constructor TSanitizerMD5.Create;
begin
end;

function TSanitizerMD5.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := THashMD5.GetHashString(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
