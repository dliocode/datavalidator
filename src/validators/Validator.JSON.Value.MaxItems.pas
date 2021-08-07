{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.Value.MaxItems;

interface

uses
  DataValidator.ItemBase,
  System.JSON;

type
  TDataValidatorJSONValueMaxItems = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FMaxItems: Integer;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMaxItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONValueMaxItems }

constructor TDataValidatorJSONValueMaxItems.Create(const AMaxItems: Integer; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMaxItems := AMaxItems;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TDataValidatorJSONValueMaxItems.Checked: IDataValidatorResult;
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
        R := (LJSONPair.JsonValue as TJSONArray).Count <= FMaxItems
      else
        if LJSONPair.JsonValue is TJSONObject then
          R := (LJSONPair.JsonValue as TJSONObject).Count <= FMaxItems
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
