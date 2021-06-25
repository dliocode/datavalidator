{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsGreaterThan;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Variants;

type
  TValidatorIsGreaterThan = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueLessThan: Integer;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AValueLessThan: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsGreaterThan }

constructor TValidatorIsGreaterThan.Create(const AValueLessThan: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueLessThan := AValueLessThan;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsGreaterThan.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := VarCompareValue(LValue, FValueLessThan) = vrGreaterThan;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
