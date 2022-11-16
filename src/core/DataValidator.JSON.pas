{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

unit DataValidator.JSON;

interface

uses
  DataValidator.Types,
  DataValidator.Intf,
  System.Generics.Collections, System.Rtti, System.JSON, System.SysUtils;

type
  TDataValidatorJSON = class(TInterfacedObject, IDataValidatorJSON, IDataValidatorJSONResult)
  private
    FJSON: TJSONValue;
    FList: TList<TPair<string, IDataValidatorJSONBaseContext>>;

    function CheckValueArray(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
    function CheckValueObject(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;

    function CheckItemValueArray(const AKey: string; const ACheckAll: Boolean): IDataValidatorResult;
    function CheckItemValueObject(const AKey: string; const ACheckAll: Boolean): IDataValidatorResult;

    function TValueToString(const AValue: TValue): string;
    function TryGetValue(const AKey: string; var AValue: IDataValidatorJSONBaseContext): Boolean;
  public
    function Validate(const AKey: TArray<string>; const AName: string = ''): IDataValidatorJSONBaseContext; overload;
    function Validate(const AKey: string; const AName: string = ''): IDataValidatorJSONBaseContext; overload;

    function Check: IDataValidatorResult;
    function CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;

    function CheckItem(const AKey: string): IDataValidatorResult;
    function CheckItemAll(const AKey: string): IDataValidatorResult;

    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const AJSON: TJSONArray); overload;
    constructor Create(const AJSON: TJSONValue); overload;
    destructor Destroy; override;
  end;

implementation

uses
  DataValidator.JSON.Base,

  DataValidator.Information,
  DataValidator.ItemBase.Sanitizer,
  DataValidator.ItemBase,

  Validator.JSON.Key.IsRequired,
  Validator.JSON.Key.IsOptional,
  Validator.IsOptional;

{ TDataValidatorJSON }

constructor TDataValidatorJSON.Create(const AJSON: TJSONObject);
begin
  if not Assigned(AJSON) then
    raise EDataValidatorException.Create('JSON is nil');

  FJSON := AJSON;
  FList := TList < TPair < string, IDataValidatorJSONBaseContext >>.Create();
end;

constructor TDataValidatorJSON.Create(const AJSON: TJSONArray);
begin
  if not Assigned(AJSON) then
    raise EDataValidatorException.Create('JSON is nil');

  FJSON := AJSON;
  FList := TList < TPair < string, IDataValidatorJSONBaseContext >>.Create();
end;

constructor TDataValidatorJSON.Create(const AJSON: TJSONValue);
begin
  if AJSON is TJSONObject then
    Create(AJSON as TJSONObject)
  else
    if AJSON is TJSONArray then
      Create(AJSON as TJSONArray)
    else
      raise EDataValidatorException.Create('JSON invalid!');
end;

destructor TDataValidatorJSON.Destroy;
begin
  FList.Free;
end;

function TDataValidatorJSON.Validate(const AKey: TArray<string>; const AName: string = ''): IDataValidatorJSONBaseContext;
var
  LBase: IDataValidatorJSONBaseContext;
  I: Integer;
begin
  LBase := TDataValidatorJSONBase.Create(Self, nil, AName);

  for I := Low(AKey) to High(AKey) do
    FList.Add(TPair<string, IDataValidatorJSONBaseContext>.Create(AKey[I], LBase));

  Result := LBase;
end;

function TDataValidatorJSON.Validate(const AKey: string; const AName: string = ''): IDataValidatorJSONBaseContext;
begin
  Result := Validate([AKey], AName);
end;

function TDataValidatorJSON.Check: IDataValidatorResult;
begin
  if FJSON is TJSONObject then
    Result := CheckValueObject(False)
  else
    if FJSON is TJSONArray then
      Result := CheckValueArray(False);
end;

function TDataValidatorJSON.CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
begin
  if FJSON is TJSONObject then
    Result := CheckValueObject(True, ATypeCheck)
  else
    if FJSON is TJSONArray then
      Result := CheckValueArray(True, ATypeCheck);
end;

function TDataValidatorJSON.CheckItem(const AKey: string): IDataValidatorResult;
begin
  if FJSON is TJSONObject then
    Result := CheckItemValueObject(AKey, False)
  else
    if FJSON is TJSONArray then
      Result := CheckItemValueArray(AKey, False);
end;

function TDataValidatorJSON.CheckItemAll(const AKey: string): IDataValidatorResult;
begin
  if FJSON is TJSONObject then
    Result := CheckItemValueObject(AKey, True)
  else
    if FJSON is TJSONArray then
      Result := CheckItemValueArray(AKey, True);
end;

function TDataValidatorJSON.CheckValueObject(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
var
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  Enum: TPair<string, IDataValidatorJSONBaseContext>;
  LListValidatorItem: TList<IDataValidatorItem>;
  LName: string;
  LValueSanitizer: TValue;
  LValidatorItem: IDataValidatorItem;
  I: Integer;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  for Enum in FList do
  begin
    LListValidatorItem := (Enum.Value as IDataValidatorContextBase<IDataValidatorItem>).GetItem;

    LName := (Enum.Value as IDataValidatorJSONValues).GetName;

    LValueSanitizer := TValue.From<TJSONPair>((FJSON as TJSONObject).Get(Enum.Key));

    for I := 0 to Pred(LListValidatorItem.Count) do
    begin
      LValidatorItem := LListValidatorItem.Items[I];

      LValidatorItem.SetKey(Enum.Key);
      LValidatorItem.SetName(LName);
      LValidatorItem.SetValue(LValueSanitizer);
      LValidatorItem.SetIndex('${index}');

      if (LValidatorItem is TDataValidatorItemBaseSanitizer) then
      begin
        LValueSanitizer := (LValidatorItem as TDataValidatorItemBaseSanitizer).Sanitize;
        Continue;
      end;

      LValidatorResult := LValidatorItem.Check;

      if not LValidatorResult.OK then
      begin
        if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
          Continue;

        LOK := False;
        LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

        if (LValidatorItem is TDataValidatorJSONKeyIsRequired) then
          Break;

        if not ACheckAll then
          Break
        else
          if ATypeCheck = TDataValidatorCheckAll.tcFirst then
            Break;
      end
      else
        if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
          Break
    end;

    LValues := Concat(LValues, [TValueToString(LValueSanitizer)]);

    if not LOK then
      if not ACheckAll then
        Break;
  end;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

function TDataValidatorJSON.CheckValueArray(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
var
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  Enum: TPair<string, IDataValidatorJSONBaseContext>;
  LListValidatorItem: TList<IDataValidatorItem>;
  LName: string;
  I: Integer;
  LValueSanitizer: TValue;
  J: Integer;
  LValidatorItem: IDataValidatorItem;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  for Enum in FList do
  begin
    LListValidatorItem := (Enum.Value as IDataValidatorContextBase<IDataValidatorItem>).GetItem;

    LName := (Enum.Value as IDataValidatorJSONValues).GetName;

    for I := 0 to Pred((FJSON as TJSONArray).Count) do
    begin
      if not((FJSON as TJSONArray).Items[I] is TJSONObject) then
        Continue;

      LValueSanitizer := TValue.From<TJSONPair>(((FJSON as TJSONArray).Items[I] as TJSONObject).Get(Enum.Key));

      for J := 0 to Pred(LListValidatorItem.Count) do
      begin
        LValidatorItem := LListValidatorItem.Items[J];

        LValidatorItem.SetKey(Enum.Key);
        LValidatorItem.SetName(LName);
        LValidatorItem.SetValue(LValueSanitizer);
        LValidatorItem.SetIndex(IntToStr(I + 1));

        if (LValidatorItem is TDataValidatorItemBaseSanitizer) then
        begin
          LValueSanitizer := (LValidatorItem as TDataValidatorItemBaseSanitizer).Sanitize;
          Continue;
        end;

        LValidatorResult := LValidatorItem.Check;

        if not LValidatorResult.OK then
        begin
          if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
            Continue;

          LOK := False;
          LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

          if (LValidatorItem is TDataValidatorJSONKeyIsRequired) then
            Break;

          if not ACheckAll then
            Break
          else
            if ATypeCheck = TDataValidatorCheckAll.tcFirst then
              Break;
        end
        else
          if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
            Break;
      end;

      LValues := Concat(LValues, [TValueToString(LValueSanitizer)]);
    end;

    if not LOK then
      if not ACheckAll then
        Break;
  end;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

function TDataValidatorJSON.CheckItemValueObject(const AKey: string; const ACheckAll: Boolean): IDataValidatorResult;
var
  LValue: IDataValidatorJSONBaseContext;
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  LListValidatorItem: TList<IDataValidatorItem>;
  LName: string;
  LValueSanitizer: TValue;
  I: Integer;
  LValidatorItem: IDataValidatorItem;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  if not TryGetValue(AKey, LValue) then
    Exit(nil);

  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  LListValidatorItem := (LValue as IDataValidatorContextBase<IDataValidatorItem>).GetItem;

  LName := (LValue as IDataValidatorJSONValues).GetName;

  LValueSanitizer := TValue.From<TJSONPair>((FJSON as TJSONObject).Get(AKey));

  for I := 0 to Pred(LListValidatorItem.Count) do
  begin
    LValidatorItem := LListValidatorItem.Items[I];

    LValidatorItem.SetKey(AKey);
    LValidatorItem.SetName(LName);
    LValidatorItem.SetValue(LValueSanitizer);
    LValidatorItem.SetIndex('${index}');

    if (LValidatorItem is TDataValidatorItemBaseSanitizer) then
    begin
      LValueSanitizer := (LValidatorItem as TDataValidatorItemBaseSanitizer).Sanitize;
      Continue;
    end;

    LValidatorResult := LValidatorItem.Check;

    if not LValidatorResult.OK then
    begin
      if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
        Continue;

      LOK := False;
      LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

      if (LValidatorItem is TDataValidatorJSONKeyIsRequired) then
        Break;

      if not ACheckAll then
        Break;
    end
    else
      if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
        Break
  end;

  LValues := Concat(LValues, [TValueToString(LValueSanitizer)]);

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

function TDataValidatorJSON.CheckItemValueArray(const AKey: string; const ACheckAll: Boolean): IDataValidatorResult;
var
  LValue: IDataValidatorJSONBaseContext;
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  LListValidatorItem: TList<IDataValidatorItem>;
  LName: string;
  LValueSanitizer: TValue;
  I: Integer;
  J: Integer;
  LValidatorItem: IDataValidatorItem;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  if not TryGetValue(AKey, LValue) then
    Exit(nil);

  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  LListValidatorItem := (LValue as IDataValidatorContextBase<IDataValidatorItem>).GetItem;

  LName := (LValue as IDataValidatorJSONValues).GetName;

  for I := 0 to Pred((FJSON as TJSONArray).Count) do
  begin
    if not((FJSON as TJSONArray).Items[I] is TJSONObject) then
      Continue;

    LValueSanitizer := TValue.From<TJSONPair>(((FJSON as TJSONArray).Items[I] as TJSONObject).Get(AKey));

    for J := 0 to Pred(LListValidatorItem.Count) do
    begin
      LValidatorItem := LListValidatorItem.Items[J];

      LValidatorItem.SetKey(AKey);
      LValidatorItem.SetName(LName);
      LValidatorItem.SetValue(LValueSanitizer);
      LValidatorItem.SetIndex(IntToStr(I + 1));

      if (LValidatorItem is TDataValidatorItemBaseSanitizer) then
      begin
        LValueSanitizer := (LValidatorItem as TDataValidatorItemBaseSanitizer).Sanitize;
        Continue;
      end;

      LValidatorResult := LValidatorItem.Check;

      if not LValidatorResult.OK then
      begin
        if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
          Continue;

        LOK := False;
        LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

        if (LValidatorItem is TDataValidatorJSONKeyIsRequired) then
          Break;

        if not ACheckAll then
          Break;
      end
      else
        if (LValidatorItem is TDataValidatorJSONKeyIsOptional) or (LValidatorItem is TValidatorIsOptional) then
          Break
    end;

    LValues := Concat(LValues, [TValueToString(LValueSanitizer)]);
  end;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

function TDataValidatorJSON.TValueToString(const AValue: TValue): string;
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

function TDataValidatorJSON.TryGetValue(const AKey: string; var AValue: IDataValidatorJSONBaseContext): Boolean;
var
  LValues: TArray<TPair<string, IDataValidatorJSONBaseContext>>;
  I: Integer;
begin
  Result := False;
  AValue := nil;

  LValues := FList.ToArray;

  for I := 0 to Pred(Length(LValues)) do
    if LValues[I].Key = AKey then
    begin
      Result := True;
      AValue := LValues[I].Value;
      Break;
    end;
end;

end.
