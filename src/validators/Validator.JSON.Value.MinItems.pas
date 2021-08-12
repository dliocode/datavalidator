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
  System.JSON, System.SysUtils;

type
  TDataValidatorJSONValueMinItems = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FMinItems: Integer;
  public
    function Check: IDataValidatorResult;
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

function TDataValidatorJSONValueMinItems.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LJSONPair: TJSONPair;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
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
