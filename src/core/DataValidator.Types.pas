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

unit DataValidator.Types;

interface

uses
  System.JSON, System.SysUtils;

type
  TDataValidatorMessage = record
    Title: string;
    Message: string;
    Description: string;
    Status: string;
    Uri: string;

    constructor Create(const ATitle: string; const AMessage: string; const ADescription: string; const AStatus: string; const AUri: string); overload;
    constructor Create(const AMessage: string; const ADescription: string = ''); overload;
    constructor Create(const AJSONObject: TJSONObject; const AOwner: Boolean = False); overload;

    function ToJSONObject(const AIncludeAll: Boolean = True): TJSONObject; overload;
    function ToJSONString(const AIncludeAll: Boolean = True): string; overload;
  end;

  TDataValidatorLocaleLanguage = (tl_en_US, tl_de_DE, tl_fr_FR, tl_it_IT, tl_es_ES, tl_ru_RU, tl_pt_BR);

  TDataValidatorCustomResult = reference to function: Boolean;
  TDataValidatorCustomSanitizer = reference to function(const AValue: string): string;

  TDataValidatorCustomValue = reference to function(const AValue: string): Boolean;
  TDataValidatorCustomValueMessage = reference to function(const AValue: string; var AMessage: string): Boolean;
  TDataValidatorCustomMessage = reference to function(const AValue: string; var AMessage: TDataValidatorMessage): Boolean;

  TDataValidatorCustomJSONValue = reference to function(const AValue: TJSONValue): Boolean;
  TDataValidatorCustomJSONValueMessage = reference to function(const AValue: TJSONValue; var AMessage: string): Boolean;
  TDataValidatorCustomJSONMessage = reference to function(const AValue: TJSONValue; var AMessage: TDataValidatorMessage): Boolean;

  TDataValidatorInformationExecute = reference to procedure;
  TDataValidatorWithMessage = reference to procedure(var AMessage: TDataValidatorMessage);

  EDataValidatorException = class(Exception)
  end;

implementation

{ TDataValidatorMessage }

constructor TDataValidatorMessage.Create(const ATitle: string; const AMessage: string; const ADescription: string; const AStatus: string; const AUri: string);
begin
  Self.Title := ATitle;
  Self.Title := AMessage;
  Self.Title := ADescription;
  Self.Title := AStatus;
  Self.Title := AUri;
end;

constructor TDataValidatorMessage.Create(const AMessage: string; const ADescription: string = '');
begin
  Self.Message := AMessage;
  Self.Description := ADescription;
end;

constructor TDataValidatorMessage.Create(const AJSONObject: TJSONObject; const AOwner: Boolean = False);
var
  LValue: TJSONValue;
begin
  try
    LValue := AJSONObject.GetValue('title');
    if Assigned(LValue) then
      Self.Title := LValue.Value;

    LValue := AJSONObject.GetValue('message');
    if Assigned(LValue) then
      Self.Message := LValue.Value;

    LValue := AJSONObject.GetValue('description');
    if Assigned(LValue) then
      Self.Description := LValue.Value;

    LValue := AJSONObject.GetValue('status');
    if Assigned(LValue) then
      Self.Status := LValue.Value;

    LValue := AJSONObject.GetValue('uri');
    if Assigned(LValue) then
      Self.Uri := LValue.Value;
  finally
    if AOwner then
      AJSONObject.Free;
  end;
end;

function TDataValidatorMessage.ToJSONObject(const AIncludeAll: Boolean = True): TJSONObject;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;

  if not Self.Title.IsEmpty or AIncludeAll then
    LJO.AddPair('title', Self.Title);

  if not Self.Message.IsEmpty or AIncludeAll then
    LJO.AddPair('message', Self.Message);

  if not Self.Description.IsEmpty or AIncludeAll then
    LJO.AddPair('description', Self.Description);

  if not Self.Status.IsEmpty or AIncludeAll then
    LJO.AddPair('status', Self.Status);

  if not Self.Uri.IsEmpty or AIncludeAll then
    LJO.AddPair('uri', Self.Uri);

  Result := LJO;
end;

function TDataValidatorMessage.ToJSONString(const AIncludeAll: Boolean = True): string;
var
  LJO: TJSONObject;
begin
  LJO := ToJSONObject(AIncludeAll);
  try
    Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

end.
