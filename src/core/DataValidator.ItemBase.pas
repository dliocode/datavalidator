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

unit DataValidator.ItemBase;

interface

uses
  DataValidator.Types,
  DataValidator.Intf,
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
  IDataValidatorItem = DataValidator.Intf.IDataValidatorItem;
  IDataValidatorResult = DataValidator.Intf.IDataValidatorResult;
  TDataValidatorResult = DataValidator.Result.TDataValidatorResult;
  TDataValidatorInformation = DataValidator.Information.TDataValidatorInformation;

  TDataValidatorItemBase = class(TInterfacedObject, IDataValidatorItemBase)
  private
    FMessage: TDataValidatorMessage;
    function GetAdjustedMessage(const AMessage: string; const AValue: string): string;
  protected
    FLocaleLanguage: TDataValidatorLocaleLanguage;
    FIsNot: Boolean;
    FKey: string;
    FName: string;
    FValue: TValue;
    FIndex: string;
    FExecute: TDataValidatorInformationExecute;

    function GetMessage: TDataValidatorMessage;
    function GetValueAsString: string;
    procedure SetValueAdapter(const AValue: TValue);
  public
    function GetDataValidatorLocaleLanguage: TDataValidatorLocaleLanguage;
    procedure SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US);
    procedure SetIsNot(const AIsNot: Boolean);
    procedure SetKey(const AKey: string);
    procedure SetName(const AName: string);
    procedure SetValue(const AValue: TValue);
    procedure SetIndex(const AValue: string);
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

procedure TDataValidatorItemBase.SetDataValidatorLocaleLanguage(const ALocaleLanguage: TDataValidatorLocaleLanguage = TDataValidatorLocaleLanguage.tl_en_US);
begin
  FLocaleLanguage := ALocaleLanguage;
end;

procedure TDataValidatorItemBase.SetKey(const AKey: string);
begin
  FKey := AKey;
end;

procedure TDataValidatorItemBase.SetName(const AName: string);
begin
  FName := AName;
end;

procedure TDataValidatorItemBase.SetValue(const AValue: TValue);
begin
  FValue := AValue;
end;

procedure TDataValidatorItemBase.SetIndex(const AValue: string);
begin
  FIndex := AValue;
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
  if not AMessage.Title.Trim.IsEmpty then
    FMessage.Title := AMessage.Title;

  if not AMessage.Message.Trim.IsEmpty then
    FMessage.Message := AMessage.Message;

  if not AMessage.Description.Trim.IsEmpty then
    FMessage.Description := AMessage.Description;

  if Length(AMessage.Details) > 0 then
    FMessage.Details := AMessage.Details;

  if not AMessage.Solution.Trim.IsEmpty then
    FMessage.Solution := AMessage.Solution;

  if not AMessage.Source.Trim.IsEmpty then
    FMessage.Source := AMessage.Source;

  if not AMessage.Code.Trim.IsEmpty then
    FMessage.Code := AMessage.Code;

  if not AMessage.CodeName.Trim.IsEmpty then
    FMessage.CodeName := AMessage.CodeName;

  if not AMessage.Uri.Trim.IsEmpty then
    FMessage.Uri := AMessage.Uri;

  if not AMessage.Data.Trim.IsEmpty then
    FMessage.Data := AMessage.Data;

  if not AMessage.StatusCode.Trim.IsEmpty then
    FMessage.StatusCode := AMessage.StatusCode;
end;

procedure TDataValidatorItemBase.SetExecute(const AExecute: TDataValidatorInformationExecute);
begin
  FExecute := AExecute;
end;

function TDataValidatorItemBase.GetMessage: TDataValidatorMessage;
var
  LValue: string;
begin
  LValue := GetValueAsString;

  Result := Default (TDataValidatorMessage);

  Result := FMessage;
  Result.Title := GetAdjustedMessage(Result.Title, LValue);
  Result.Message := GetAdjustedMessage(Result.Message, LValue);
  Result.Description := GetAdjustedMessage(Result.Description, LValue);
  Result.Solution := GetAdjustedMessage(Result.Solution, LValue);
  Result.Source := GetAdjustedMessage(Result.Source, LValue);
  Result.Code := GetAdjustedMessage(Result.Code, LValue);
  Result.CodeName := GetAdjustedMessage(Result.CodeName, LValue);
  Result.Uri := GetAdjustedMessage(Result.Uri, LValue);
  Result.Data := GetAdjustedMessage(Result.Data, LValue);
  Result.StatusCode := GetAdjustedMessage(Result.StatusCode, LValue);
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
  LFloat: Double;
begin
  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    if Assigned(LJSONPair) then
    begin
      if LJSONPair.JsonValue is TJSONNumber then
      begin
        if TryStrToFloat(AValue.AsString, LFloat) then
          LJSONPair.JsonValue := TJSONNumber.Create(AValue.AsString)
        else
          LJSONPair.JsonValue := TJSONString.Create(AValue.AsString);
      end
      else
        if LJSONPair.JsonValue is TJSONString then
          if not MatchText(string(AValue.TypeInfo.Name), ['TDateTime', 'TDate', 'TTime', 'TTimeStamp']) then
            LJSONPair.JsonValue := TJSONString.Create(AValue.AsString);
    end;
  end
  else
    FValue := AValue;
end;

function TDataValidatorItemBase.GetAdjustedMessage(const AMessage: string; const AValue: string): string;
var
  LMessage: string;
begin
  if FName.Trim.IsEmpty then
    FName := FKey;

  LMessage := AMessage;

  LMessage := LMessage.Replace('${name}', FName, [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${nameupper}', UpperCase(FName), [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${namelower}', LowerCase(FName), [rfReplaceAll, rfIgnoreCase]);

  LMessage := LMessage.Replace('${key}', FKey, [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${keyupper}', UpperCase(FKey), [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${keylower}', LowerCase(FKey), [rfReplaceAll, rfIgnoreCase]);

  LMessage := LMessage.Replace('${value}', AValue, [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${valueupper}', UpperCase(AValue), [rfReplaceAll, rfIgnoreCase]);
  LMessage := LMessage.Replace('${valuelower}', LowerCase(AValue), [rfReplaceAll, rfIgnoreCase]);

  LMessage := LMessage.Replace('${index}', FIndex, [rfReplaceAll, rfIgnoreCase]);

  Result := LMessage;
end;

end.
