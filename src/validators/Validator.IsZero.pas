{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsZero;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Variants;

type
  TValidatorIsZero = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsZero }

constructor TValidatorIsZero.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsZero.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if not Trim(LValue).IsEmpty then
      R := VarCompareValue(LValue, 0) = vrEqual;
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
