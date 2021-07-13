{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToMD5;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.Hash;

type
  TSanitizerToMD5 = class(TDataValidatorItemBaseSanitizer)
  private
  public
    function Sanitize: TValue; override;
    constructor Create;
  end;

implementation

{ TSanitizerMD5 }

constructor TSanitizerToMD5.Create;
begin
end;

function TSanitizerToMD5.Sanitize: TValue;
var
  LValue: TValue;
begin
  LValue := THashMD5.GetHashString(GetValueAsString);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
