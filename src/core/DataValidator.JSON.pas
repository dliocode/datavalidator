{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.JSON;

interface

uses
  DataValidator.JSON.Intf, DataValidator.Values.Intf, DataValidator.ItemBase.Intf, DataValidator.Base.Intf, DataValidator.Information.Intf,
  DataValidator.Result, DataValidator.Information, DataValidator.Types, DataValidator.ItemBase.Sanitizer, DataValidator.Base,
  System.SysUtils, System.Variants, System.Generics.Collections, System.JSON;

type
  TDataValidatorJSON = class(TInterfacedObject, IDataValidatorJSONBase, IDataValidatorJSON)
  private
    FJSON: TJSONObject;
    FListBase: TList<IDataValidatorsBaseJSON<IDataValidatorJSON>>;

    function Check(const ACheckAll: Boolean): IDataValidatorResult;
  public
    function Validate(const AName: string): IDataValidatorsBaseJSON<IDataValidatorJSON>;

    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;

    constructor Create(const AJSON: TJSONObject);
    destructor Destroy; override;

    class function New(const AJSON: TJSONObject): IDataValidatorJSONBase;
  end;

implementation

{ TDataValidatorJSON }

class function TDataValidatorJSON.New(const AJSON: TJSONObject): IDataValidatorJSONBase;
begin
  Result := TDataValidatorJSON.Create(AJSON);
end;

constructor TDataValidatorJSON.Create(const AJSON: TJSONObject);
begin
  if not Assigned(AJSON) then
    raise Exception.Create('JSON invalid!');

  FJSON := AJSON;

  FListBase := TList < IDataValidatorsBaseJSON < IDataValidatorJSON >>.Create;
end;

destructor TDataValidatorJSON.Destroy;
begin
  FListBase.Clear;
  FListBase.DisposeOf;

  inherited;
end;

function TDataValidatorJSON.Validate(const AName: string): IDataValidatorsBaseJSON<IDataValidatorJSON>;
var
  LJSONPair: TJSONPair;
begin
  LJSONPair := FJSON.Get(AName);

  FListBase.Add(TDataValidatorsBase<IDataValidatorJSON>.New(Self, LJSONPair) as IDataValidatorsBaseJSON<IDataValidatorJSON>);
  Result := FListBase.Last;
end;

function TDataValidatorJSON.Checked: IDataValidatorResult;
begin
  Result := Check(False);
end;

function TDataValidatorJSON.CheckedAll: IDataValidatorResult;
begin
  Result := Check(True);
end;

function TDataValidatorJSON.Check(const ACheckAll: Boolean): IDataValidatorResult;
  function ValueString(const AValue: TValue): string;
  var
    LJSONPair: TJSONPair;
  begin
    Result := '';

    if AValue.IsType<TJSONPair> then
    begin
      LJSONPair := AValue.AsType<TJSONPair>;

      if Assigned(LJSONPair) then
        Result := LJSONPair.JsonValue.Value;
    end
    else
      Result := AValue.AsString;
  end;

var
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  LValues: TArray<string>;
  I: Integer;
  LListValidatorItem: TList<IDataValidatorItem>;
  LValueSanitizer: TValue;
  J: Integer;
  LValidatorResult: IDataValidatorResult;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.New;

  for I := 0 to Pred(FListBase.Count) do
  begin
    LListValidatorItem := (FListBase.Items[I] as IDataValidatorsBaseItem<IDataValidatorValues>).GetItem;
    LValueSanitizer := (FListBase.Items[I] as IDataValidatorsBaseItem<IDataValidatorValues>).GetValue;

    for J := 0 to Pred(LListValidatorItem.Count) do
    begin
      if (LListValidatorItem.Items[J] is TDataValidatorItemBaseSanitizer) then
      begin
        LListValidatorItem.Items[J].SetValue(LValueSanitizer);
        LValueSanitizer := (LListValidatorItem.Items[J] as TDataValidatorItemBaseSanitizer).Sanitize;
        Continue;
      end;

      LListValidatorItem.Items[J].SetValue(LValueSanitizer);
      LValidatorResult := LListValidatorItem.Items[J].Checked;

      if not LValidatorResult.OK then
      begin
        LOK := False;
        LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

        if not ACheckAll then
          Break;
      end;
    end;

    LValues := Concat(LValues, [ValueString(LValueSanitizer)]);

    LListValidatorItem.Clear;

    if not LOK then
      if not ACheckAll then
        Break;
  end;

  FListBase.Clear;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

end.
