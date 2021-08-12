{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsOptional;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorIsOptional = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FOptionalExecute: TDataValidatorCustomExecute;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AOptionalExecute: TDataValidatorCustomExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsOptional }

constructor TValidatorIsOptional.Create(const AOptionalExecute: TDataValidatorCustomExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FOptionalExecute := AOptionalExecute;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsOptional.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;

  if Assigned(FOptionalExecute) then
    R := FOptionalExecute(LValue)
  else
    R := LValue.IsEmpty;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
