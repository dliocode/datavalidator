{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsTimeBetween;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils;

type
  TValidatorIsTimeBetween = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FStartTime: TTime;
    FEndTime: TTime;
    FJSONISO8601ReturnUTC: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AStartTime: TTime; const AEndTime: TTime; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsTimeBetween }

constructor TValidatorIsTimeBetween.Create(const AStartTime: TTime; const AEndTime: TTime; const AJSONISO8601ReturnUTC: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FStartTime := AStartTime;
  FEndTime := AEndTime;
  FJSONISO8601ReturnUTC := AJSONISO8601ReturnUTC;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsTimeBetween.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LTime: TDateTime;
begin
  LValue := GetValueAsString;

  R := TryStrToTime(LValue, LTime);

  if not R then
    R := TryISO8601ToDate(LValue, LTime, FJSONISO8601ReturnUTC);

  if R then
    R := TimeInRange(LTime, FStartTime, FEndTime);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
