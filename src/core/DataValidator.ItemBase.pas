{
  ********************************************************************************

  Github - https://github.com/dliocode/datavalidator

  ********************************************************************************

  MIT License

  Copyright (c) 2021 Danilo Lucas

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

unit DataValidator.ItemBase;

interface

uses
  DataValidator.Types, DataValidator.ItemBase.Intf, DataValidator.Result.Intf, DataValidator.Result, DataValidator.Information,
  System.SysUtils, System.RTTI, System.JSON, System.StrUtils, System.TypInfo;

type
  TJSONPair = System.JSON.TJSONPair;
  TValue = System.RTTI.TValue;

  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;

  TDataValidatorCustomExecute = DataValidator.Types.TDataValidatorCustomExecute;
  TDataValidatorCustomMessageExecute = DataValidator.Types.TDataValidatorCustomMessageExecute;
  TDataValidatorCustomJSONValueExecute = DataValidator.Types.TDataValidatorCustomJSONValueExecute;
  TDataValidatorCustomJSONValueMessageExecute = DataValidator.Types.TDataValidatorCustomJSONValueMessageExecute;

  TDataValidatorInformationExecute = DataValidator.ItemBase.Intf.TDataValidatorInformationExecute;
  IDataValidatorItem = DataValidator.ItemBase.Intf.IDataValidatorItem;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBase = class(TInterfacedObject, IDataValidatorItemBase)
  protected
    FLocaleLanguage: TDataValidatorLocaleLanguage;
    FIsNot: Boolean;
    FValue: TValue;
    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
    function GetValueAsString: string;
    procedure SetValueAdapter(const AValue: TValue);
  public
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);

    procedure SetIsNot(const AIsNot: Boolean);
    procedure SetValue(const AValue: TValue);
    procedure SetMessage(const AMessage: string);
    procedure SetExecute(const AExecute: TDataValidatorInformationExecute);
  end;

implementation

{ TDataValidatorItemBase }

function TDataValidatorItemBase.GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
begin
  Result := FLocaleLanguage;
end;

procedure TDataValidatorItemBase.SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);
begin
  FLocaleLanguage := ALocaleLanguage;
end;

procedure TDataValidatorItemBase.SetValue(const AValue: TValue);
begin
  FValue := AValue;
end;

procedure TDataValidatorItemBase.SetIsNot(const AIsNot: Boolean);
begin
  FIsNot := AIsNot;
end;

procedure TDataValidatorItemBase.SetMessage(const AMessage: string);
begin
  if not AMessage.IsEmpty then
    FMessage := AMessage;
end;

procedure TDataValidatorItemBase.SetExecute(const AExecute: TDataValidatorInformationExecute);
begin
  FExecute := AExecute;
end;

function TDataValidatorItemBase.GetValueAsString: string;
var
  LJSONPair: TJSONPair;
begin
  Result := '';

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
      Result := LJSONPair.JsonValue.ToString.Trim(['"']);
  end
  else
    Result := FValue.AsString;
end;

procedure TDataValidatorItemBase.SetValueAdapter(const AValue: TValue);
var
  LJSONPair: TJSONPair;
  LJSONGetValue: TJSONValue;
  LValueInt: Int64;
begin
  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
    begin
      LJSONGetValue := LJSONPair.JsonValue;

      if LJSONGetValue is TJSONBool then
        FValue.AsType<TJSONPair>.JsonValue := TJSONBool.Create(AValue.AsBoolean)
      else
        if LJSONGetValue is TJSONNumber then
        begin
          if TryStrToInt64(LJSONGetValue.Value, LValueInt) then
            FValue.AsType<TJSONPair>.JsonValue := TJSONNumber.Create(AValue.AsInt64)
          else
            FValue.AsType<TJSONPair>.JsonValue := TJSONNumber.Create(AValue.AsType<Double>)
        end
        else
          if LJSONGetValue is TJSONString then
          begin
            if not MatchText(string(AValue.TypeInfo.Name), ['TDateTime', 'TDate', 'TTime', 'TTimeStamp']) then
              FValue.AsType<TJSONPair>.JsonValue := TJSONString.Create(AValue.AsString);
          end;

      Exit;
    end;
  end;

  FValue := AValue;
end;

end.
