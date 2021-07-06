{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Values;

interface

uses
  DataValidator.Values.Intf, DataValidator.ItemBase.Intf, DataValidator.Base.Intf, DataValidator.Information.Intf,
  DataValidator.Result, DataValidator.Information, DataValidator.Types, DataValidator.ItemBase.Sanitizer, DataValidator.Base,
  System.SysUtils, System.Variants, System.Generics.Collections, System.JSON;

type
  TDataValidatorValues = class(TInterfacedObject, IDataValidatorValuesBase, IDataValidatorValues)
  strict private
    FListBase: TList<IDataValidatorsBase<IDataValidatorValues>>;

    function Check(const ACheckAll: Boolean): IDataValidatorResult;
  public
    function Validate(const AValue: string): IDataValidatorsBase<IDataValidatorValues>;

    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;

    constructor Create;
    destructor Destroy; override;

    class function New(): IDataValidatorValuesBase;
  end;

implementation

{ TDataValidatorValues }

class function TDataValidatorValues.New(): IDataValidatorValuesBase;
begin
  Result := TDataValidatorValues.Create;
end;

constructor TDataValidatorValues.Create;
begin
  inherited;
  FListBase := TList < IDataValidatorsBase < IDataValidatorValues >>.Create;
end;

destructor TDataValidatorValues.Destroy;
begin
  FListBase.Clear;
  FListBase.DisposeOf;
  inherited;
end;

function TDataValidatorValues.Validate(const AValue: string): IDataValidatorsBase<IDataValidatorValues>;
begin
  FListBase.Add(TDataValidatorsBase<IDataValidatorValues>.Create(Self, AValue) as IDataValidatorsBase<IDataValidatorValues>);
  Result := FListBase.Last;
end;

function TDataValidatorValues.Check(const ACheckAll: Boolean): IDataValidatorResult;
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
  I: Integer;
  LListValidatorItem: TList<IDataValidatorItem>;
  LValueSanitizer: TValue;
  J: Integer;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.Create;

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

function TDataValidatorValues.Checked: IDataValidatorResult;
begin
  Result := Check(False);
end;

function TDataValidatorValues.CheckedAll: IDataValidatorResult;
begin
  Result := Check(True);
end;

end.
