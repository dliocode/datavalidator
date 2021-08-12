{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsDateEquals;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils, System.Types;

type
  TValidatorIsDateEquals = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCompareDate: TDate;
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsDateEquals }

constructor TValidatorIsDateEquals.Create(const ACompareDate: TDate; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCompareDate := ACompareDate;
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsDateEquals.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValue := LValue.Replace('\','');

    R := TryStrToDate(LValue, LDate);

    if not R then
      R := TryISO8601ToDate(LValue, LDate, FJSONISO8601ReturnUTC);

    if R then
      R := CompareDate(LDate, FCompareDate) = EqualsValue;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
