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

unit DataValidator.Value;

interface

uses
  DataValidator.Types,
  DataValidator.Intf,
  System.Generics.Collections, System.Rtti, System.JSON;

type
  TDataValidatorValue = class(TInterfacedObject, IDataValidatorValue, IDataValidatorValueResult)
  private
    FList: TList<IDataValidatorValueBaseContext>;

    function CheckValue(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
    function TValueToString(const AValue: TValue): string;
  public
    function Validate(const AValue: string; const AName: string = ''): IDataValidatorValueBaseContext; overload;
    function Validate(const AValue: TArray<string>; const AName: string = ''): IDataValidatorValueBaseContext; overload;

    function Check: IDataValidatorResult;
    function CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DataValidator.Value.Base, DataValidator.Information, DataValidator.ItemBase.Sanitizer, DataValidator.ItemBase,
  Validator.IsOptional;

{ TDataValidatorValue }

constructor TDataValidatorValue.Create;
begin
  FList := TList<IDataValidatorValueBaseContext>.Create;
end;

destructor TDataValidatorValue.Destroy;
begin
  FList.Free;
end;

function TDataValidatorValue.Validate(const AValue: string; const AName: string = ''): IDataValidatorValueBaseContext;
begin
  Result := Validate([AValue], AName);
end;

function TDataValidatorValue.Validate(const AValue: TArray<string>; const AName: string = ''): IDataValidatorValueBaseContext;
begin
  FList.Add(TDataValidatorValueBase.Create(Self, AValue, AName));
  Result := FList.Last;
end;

function TDataValidatorValue.Check: IDataValidatorResult;
begin
  Result := CheckValue(False);
end;

function TDataValidatorValue.CheckAll(const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
begin
  Result := CheckValue(True, ATypeCheck);
end;

function TDataValidatorValue.CheckValue(const ACheckAll: Boolean; const ATypeCheck: TDataValidatorCheckAll = TDataValidatorCheckAll.tcAll): IDataValidatorResult;
var
  LOK: Boolean;
  LInfo: IDataValidatorInformations;
  I: Integer;
  LListValidatorItem: TList<IDataValidatorItem>;
  LValue: TArray<string>;
  LName: string;
  J: Integer;
  LValueSanitizer: TValue;
  K: Integer;
  LValidatorItem: IDataValidatorItem;
  LValidatorResult: IDataValidatorResult;
  LValues: TArray<string>;
begin
  LOK := True;
  LInfo := TDataValidatorInformations.Create;

  for I := 0 to Pred(FList.Count) do
  begin
    LListValidatorItem := (FList.Items[I] as IDataValidatorContextBase<IDataValidatorItem>).GetItem;
    LValue := (FList.Items[I] as IDataValidatorValueValues).GetValues;
    LName := (FList.Items[I] as IDataValidatorValueValues).GetName;

    for J := 0 to Pred(Length(LValue)) do
    begin
      LValueSanitizer := LValue[J];

      for K := 0 to Pred(LListValidatorItem.Count) do
      begin
        LValidatorItem := LListValidatorItem.Items[K];

        LValidatorItem.SetKey(LName);
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
          if LValidatorItem is TValidatorIsOptional then
            Continue;

          LOK := False;
          LInfo.Add(LValidatorResult.Informations as IDataValidatorInformations);

          if not ACheckAll then
            Break
          else
            if ATypeCheck = TDataValidatorCheckAll.tcFirst then
              Break;
        end
        else
          if LValidatorItem is TValidatorIsOptional then
            Break
      end;

      LValues := Concat(LValues, [TValueToString(LValueSanitizer)]);

      if not LOK then
        if not ACheckAll then
          Break;
    end;

    if not LOK then
      if not ACheckAll then
        Break;
  end;

  Result := TDataValidatorResult.Create(LOK, LInfo, LValues);
end;

function TDataValidatorValue.TValueToString(const AValue: TValue): string;
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

end.
