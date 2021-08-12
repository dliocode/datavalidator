{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.Custom;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorCustom = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCustomExecute: TDataValidatorCustomExecute;
    FCustomMessageExecute: TDataValidatorCustomMessageExecute;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ACustomExecute: TDataValidatorCustomExecute; const ACustomMessageExecute: TDataValidatorCustomMessageExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorCustom }

constructor TValidatorCustom.Create(const ACustomExecute: TDataValidatorCustomExecute; const ACustomMessageExecute: TDataValidatorCustomMessageExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCustomExecute := ACustomExecute;
  FCustomMessageExecute := ACustomMessageExecute;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorCustom.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if Assigned(FCustomExecute) then
      R := FCustomExecute(LValue)
    else
      if Assigned(FCustomMessageExecute) then
        R := FCustomMessageExecute(LValue, FMessage);
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
