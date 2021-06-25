{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsDateGreaterThan;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils, System.Variants, System.Types;

type
  TValidatorIsDateGreaterThan = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCompareDate: TDate;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const ACompareDate: TDate; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsDateGreaterThan }

constructor TValidatorIsDateGreaterThan.Create(const ACompareDate: TDate; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCompareDate := ACompareDate;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsDateGreaterThan.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LDate: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if TryStrToDate(VarToStr(LValue), LDate) then
    R := CompareDate(LDate, FCompareDate) = GreaterThanValue;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
