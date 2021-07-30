{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Value;

interface

uses
  DataValidator.Intf, DataValidator.Result.Intf, DataValidator.Information.Intf, DataValidator.ItemBase.Intf, DataValidator.Context.Intf,
  DataValidator.Value.Base, DataValidator.Information, DataValidator.ItemBase.Sanitizer, DataValidator.ItemBase,
  System.Generics.Collections, System.Rtti, System.JSON;

type
  TDataValidatorValue = class(TInterfacedObject, IDataValidatorValue, IDataValidatorValueResult)
  private
    FList: TList<IDataValidatorValueBaseContext>;

    function Check(const ACheckAll: Boolean): IDataValidatorResult;
  public
    function Validate(const AValue: string): IDataValidatorValueBaseContext;

    function Checked: IDataValidatorResult;
    function CheckedAll: IDataValidatorResult;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorValue }

constructor TDataValidatorValue.Create;
begin
  FList := TList<IDataValidatorValueBaseContext>.Create;
end;

destructor TDataValidatorValue.Destroy;
begin
  FList.Clear;
  FList.DisposeOf;

  inherited;
end;

function TDataValidatorValue.Validate(const AValue: string): IDataValidatorValueBaseContext;
begin
  FList.Add(TDataValidatorValueBase.Create(Self, AValue));
  Result := FList.Last;
end;

function TDataValidatorValue.Checked: IDataValidatorResult;
begin
  Result := Check(False);
end;

function TDataValidatorValue.CheckedAll: IDataValidatorResult;
begin
  Result := Check(True);
end;

function TDataValidatorValue.Check(const ACheckAll: Boolean): IDataValidatorResult;
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
  LValidatorItem: IDataValidatorItem;
  LValueSanitizer: TValue;

  J: Integer;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  for I := 0 to Pred(FList.Count) do
  begin
    LListValidatorItem := (FList.Items[I] as IDataValidatorContextBase<IDataValidatorItem>).GetItem;
    LValueSanitizer := (FList.Items[I] as IDataValidatorContextBase<IDataValidatorItem>).GetValue;

    for J := 0 to Pred(LListValidatorItem.Count) do
    begin
      LValidatorItem := LListValidatorItem.Items[J];

      if (LValidatorItem is TDataValidatorItemBaseSanitizer) then
      begin
        LValidatorItem.SetValue(LValueSanitizer);
        LValueSanitizer := (LValidatorItem as TDataValidatorItemBaseSanitizer).Sanitize;
        Continue;
      end;

      LValidatorItem.SetValue(LValueSanitizer);

      LValidatorResult := LValidatorItem.Checked;

      if not LValidatorResult.OK then
      begin
        LOK := False;
        LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

        if not ACheckAll then
          Break;
      end;
    end;

    LValues := Concat(LValues, [ValueString(LValueSanitizer)]);

    if not LOK then
      if not ACheckAll then
        Break;
  end;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

end.
