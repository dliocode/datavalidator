{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToDateTime;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.DateUtils;

type
  TSanitizerToDateTime = class(TDataValidatorItemBaseSanitizer)
  private
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Sanitize: TValue; override;
    constructor Create(const AJSONISO8601ReturnUTC: Boolean);
  end;

implementation

{ TSanitizerToDateTime }

constructor TSanitizerToDateTime.Create(const AJSONISO8601ReturnUTC: Boolean);
begin
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
end;

function TSanitizerToDateTime.Sanitize: TValue;
var
  LValue: string;
  R: Boolean;
  LDateTime: TDateTime;
begin
  LValue := Trim(GetValueAsString);
  LValue := LValue.Replace('\','');

  R := TryStrToDateTime(LValue, LDateTime);

  if not R then
    R := TryISO8601ToDate(LValue, LDateTime, FJSONISO8601ReturnUTC);

  if R then
    SetValueAdapter(TValue.From<TDateTime>(LDateTime));

  Result := FValue;
end;

end.
