{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.ToDate;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils, System.DateUtils;

type
  TSanitizerToDate = class(TDataValidatorItemBaseSanitizer)
  private
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Sanitize: TValue; override;
    constructor Create(const AJSONISO8601ReturnUTC: Boolean);
  end;

implementation

{ TSanitizerToDate }

constructor TSanitizerToDate.Create(const AJSONISO8601ReturnUTC: Boolean);
begin
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
end;

function TSanitizerToDate.Sanitize: TValue;
var
  LValue: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := Trim(GetValueAsString);
  LValue := LValue.Replace('\','');

  R := TryStrToDate(LValue, LDate);

  if not R then
    R := TryISO8601ToDate(LValue, LDate, FJSONISO8601ReturnUTC);

  if R then
    SetValueAdapter(TValue.From<TDate>(LDate));

  Result := FValue;
end;

end.
