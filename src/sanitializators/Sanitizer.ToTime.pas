{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToTime;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.DateUtils;

type
  TSanitizerToTime = class(TDataValidatorItemBaseSanitizer)
  private
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Sanitize: TValue; override;
    constructor Create(const AJSONISO8601ReturnUTC: Boolean);
  end;

implementation

{ TSanitizerToTime }

constructor TSanitizerToTime.Create(const AJSONISO8601ReturnUTC: Boolean);
begin
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
end;

function TSanitizerToTime.Sanitize: TValue;
var
  LValue: string;
  R: Boolean;
  LTime: TDateTime;
begin
  LValue := Trim(GetValueAsString);

  R := TryStrToTime(LValue, LTime);

  if not R then
    R := TryISO8601ToDate(LValue, LTime, FJSONISO8601ReturnUTC);

  if R then
    SetValueAdapter(TValue.From<TTime>(LTime));

  Result := FValue;
end;

end.
