{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.Value.Custom;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.JSON;

type
  TValidatorJSONValueCustom = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCustomJSONValueExecute: TDataValidatorCustomJSONValueExecute;
    FCustomJSONValueMessageExecute: TDataValidatorCustomJSONValueMessageExecute;
  public
    function Check: IDataValidatorResult;
    constructor Create(
      const ACustomJSONObjectExecute: TDataValidatorCustomJSONValueExecute; const ACustomJSONObjectMessageExecute: TDataValidatorCustomJSONValueMessageExecute;
      const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorJSONValueCustom }

constructor TValidatorJSONValueCustom.Create(
  const ACustomJSONObjectExecute: TDataValidatorCustomJSONValueExecute; const ACustomJSONObjectMessageExecute: TDataValidatorCustomJSONValueMessageExecute;
  const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCustomJSONValueExecute := ACustomJSONObjectExecute;
  FCustomJSONValueMessageExecute := ACustomJSONObjectMessageExecute;

  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorJSONValueCustom.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LJSONPair: TJSONPair;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if not Trim(LValue).IsEmpty then
      if FValue.IsType<TJSONPair> then
      begin
        LJSONPair := FValue.AsType<TJSONPair>;

        if Assigned(LJSONPair) then
          if Assigned(FCustomJSONValueExecute) then
            R := FCustomJSONValueExecute(LJSONPair.JsonValue)
          else
            if Assigned(FCustomJSONValueMessageExecute) then
              R := FCustomJSONValueMessageExecute(LJSONPair.JsonValue, FMessage);

        LValue := GetValueAsString;
      end;
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
