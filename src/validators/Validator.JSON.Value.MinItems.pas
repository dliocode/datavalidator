{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.Value.MinItems;

interface

uses
  DataValidator.ItemBase,
  System.JSON;

type
  TDataValidatorJSONValueMinItems = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FMinItems: Integer;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMinItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONValueMinItems }

constructor TDataValidatorJSONValueMinItems.Create(const AMinItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMinItems := AMinItems;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TDataValidatorJSONValueMinItems.Checked: IDataValidatorResult;
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
    begin
      if LJSONPair.JsonValue is TJSONArray then
        R := (LJSONPair.JsonValue as TJSONArray).Count >= FMinItems
      else
        if LJSONPair.JsonValue is TJSONObject then
          R := (LJSONPair.JsonValue as TJSONObject).Count >= FMinItems
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
