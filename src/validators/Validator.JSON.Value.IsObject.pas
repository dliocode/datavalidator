{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.Value.IsObject;

interface

uses
  DataValidator.ItemBase,
  System.JSON;

type
  TDataValidatorJSONValueIsObject = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONValueIsObject }

constructor TDataValidatorJSONValueIsObject.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TDataValidatorJSONValueIsObject.Checked: IDataValidatorResult;
var
  R: Boolean;
  LValue: string;
  LJSONPair: TJSONPair;
begin
  R := False;
  LValue := GetValueAsString;

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
      R := LJSONPair.JsonValue is TJSONObject;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
