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
    function Check: IDataValidatorResult;
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

function TValidatorIsGreaterThan.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if not Trim(LValue).IsEmpty then
      R := VarCompareValue(LValue, FValueLessThan) = vrGreaterThan;
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
