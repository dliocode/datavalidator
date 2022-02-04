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
  DataValidator.Types,
  DataValidator.ItemBase.Intf, DataValidator.Result.Intf, DataValidator.Information.Intf,
  DataValidator.Result, DataValidator.Information,
  System.SysUtils, System.RTTI, System.JSON, System.StrUtils, System.TypInfo;

type
  TJSONPair = System.JSON.TJSONPair;
  TValue = System.RTTI.TValue;

  TDataValidatorLocaleLanguage = DataValidator.Types.TDataValidatorLocaleLanguage;
  TDataValidatorCustomResult = DataValidator.Types.TDataValidatorCustomResult;

  TDataValidatorMessage = DataValidator.Types.TDataValidatorMessage;
  TDataValidatorCustomValue = DataValidator.Types.TDataValidatorCustomValue;
  TDataValidatorCustomValueMessage = DataValidator.Types.TDataValidatorCustomValueMessage;
  TDataValidatorCustomMessage = DataValidator.Types.TDataValidatorCustomMessage;

  TDataValidatorCustomJSONValue = DataValidator.Types.TDataValidatorCustomJSONValue;
  TDataValidatorCustomJSONValueMessage = DataValidator.Types.TDataValidatorCustomJSONValueMessage;
  TDataValidatorCustomJSONMessage = DataValidator.Types.TDataValidatorCustomJSONMessage;

  TDataValidatorInformationExecute = DataValidator.Types.TDataValidatorInformationExecute;
  IDataValidatorItem = DataValidator.ItemBase.Intf.IDataValidatorItem;
  IDataValidatorResult = DataValidator.Result.Intf.IDataValidatorResult;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBase = class(TInterfacedObject, IDataValidatorItemBase)
  private
    FMessage: TDataValidatorMessage;

    function GetAdjustedMessage(const AMessage: string): string;
  protected
    FLocaleLanguage: TDataValidatorLocaleLanguage;
    FIsNot: Boolean;
    FName: string;
    FValue: TValue;
    FExecute: TDataValidatorInformationExecute;

    function GetMessage: TDataValidatorMessage;
    function GetValueAsString: string;
    procedure SetValueAdapter(const AValue: TValue);
  public
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = tl_en_US);
    procedure SetIsNot(const AIsNot: Boolean);
    procedure SetName(const AName: string);
    procedure SetValue(const AValue: TValue);
    procedure SetMessage(const AMessage: string); overload;
    procedure SetMessage(const AMessage: TDataValidatorMessage); overload;
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

procedure TDataValidatorItemBase.SetName(const AName: string);
begin
  FName := AName;
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
    FMessage.Message := AMessage;
end;

procedure TDataValidatorItemBase.SetMessage(const AMessage: TDataValidatorMessage);
begin
  FMessage := AMessage;
end;

procedure TDataValidatorItemBase.SetExecute(const AExecute: TDataValidatorInformationExecute);
begin
  FExecute := AExecute;
end;

function TDataValidatorItemBase.GetMessage: TDataValidatorMessage;
begin
  Result := Default (TDataValidatorMessage);

  Result := FMessage;
  Result.Title := GetAdjustedMessage(Result.Title);
  Result.Message := GetAdjustedMessage(Result.Message);
  Result.Detail := GetAdjustedMessage(Result.Detail);
  Result.Status := GetAdjustedMessage(Result.Status);
end;

function TDataValidatorItemBase.GetValueAsString: string;
var
  LJSONPair: TJSONPair;
  LValue: string;
begin
  Result := '';

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
      if not(LJSONPair.JsonValue is TJSONNull) then
      begin
        LValue := LJSONPair.JsonValue.ToString.Trim(['"']);
        Result := StringReplace(LValue, '\/', '/', [rfReplaceAll]);
      end;
  end
  else
    Result := FValue.AsString;
end;

procedure TDataValidatorItemBase.SetValueAdapter(const AValue: TValue);
var
  LJSONPair: TJSONPair;
begin
  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
    begin
      if LJSONPair.JsonValue is TJSONString then
        if not MatchText(string(AValue.TypeInfo.Name), ['TDateTime', 'TDate', 'TTime', 'TTimeStamp']) then
          FValue.AsType<TJSONPair>.JsonValue := TJSONString.Create(AValue.AsString);

      Exit;
    end;
  end;

  FValue := AValue;
end;

function TDataValidatorItemBase.GetAdjustedMessage(const AMessage: string): string;
var
  LKey: string;
  LValue: string;
  LMessage: string;
begin
  LKey := FName;
  LValue := GetValueAsString;

  LMessage := AMessage;
  LMessage := LMessage.Replace('${key}', LKey, [rfReplaceAll]);
  LMessage := LMessage.Replace('${keyupper}', UpperCase(LKey), [rfReplaceAll]);
  LMessage := LMessage.Replace('${keylower}', LowerCase(LKey), [rfReplaceAll]);
  LMessage := LMessage.Replace('${value}', LValue, [rfReplaceAll]);
  LMessage := LMessage.Replace('${valueupper}', UpperCase(LValue), [rfReplaceAll]);
  LMessage := LMessage.Replace('${valuelower}', LowerCase(LValue), [rfReplaceAll]);

  Result := LMessage;
end;

end.
