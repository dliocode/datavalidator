{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsDateBetween;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils;

type
  TValidatorIsDateBetween = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FStartDate: TDate;
    FEndDate: TDate;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AStartDate: TDate; const AEndDate: TDate; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsDateBetween }

constructor TValidatorIsDateBetween.Create(const AStartDate: TDate; const AEndDate: TDate; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FStartDate := AStartDate;
  FEndDate := AEndDate;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsDateBetween.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if TryStrToDate(LValue, LDate) then
    R := DateInRange(LDate, FStartDate, FEndDate);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
