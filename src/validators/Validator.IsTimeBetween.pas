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
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AStartTime: TTime; const AEndTime: TTime; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsTimeBetween }

constructor TValidatorIsTimeBetween.Create(const AStartTime: TTime; const AEndTime: TTime; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FStartTime := AStartTime;
  FEndTime := AEndTime;
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
  R := False;

  if TryStrToTime(LValue, LTime) then
    R := TimeInRange(LTime, FStartTime, FEndTime);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
