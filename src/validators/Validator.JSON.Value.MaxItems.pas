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
  System.JSON, System.SysUtils;

type
  TDataValidatorJSONValueMaxItems = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FMaxItems: Integer;
  public
    function Check: IDataValidatorResult;
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

function TDataValidatorJSONValueMaxItems.Check: IDataValidatorResult;
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
