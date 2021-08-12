{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsTimeEquals;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils, System.Types;

type
  TValidatorIsTimeEquals = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCompareTime: TTime;
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsTimeEquals }

constructor TValidatorIsTimeEquals.Create(const ACompareTime: TTime; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCompareTime := ACompareTime;
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsTimeEquals.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LTime: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValue := LValue.Replace('\','');

    R := TryStrToTime(LValue, LTime);

    if not R then
      R := TryISO8601ToDate(LValue, LTime, FJSONISO8601ReturnUTC);

    if R then
      R := CompareTime(LTime, FCompareTime) = EqualsValue;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
