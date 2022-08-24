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

unit DataValidator.Types;

interface

uses
  System.JSON, System.SysUtils;

type
  TDataValidatorMessage = record
    Title: string; // Error creating product
    Message: string; // Could not create product
    Description: string; // Exception Message
    Solution: string; // The solution is to fill in all the data
    Source: string; // VIEW PRODUCT
    Code: string; // P001
    CodeName: string; // ERROR_PRODUCT_CREATOR
    Uri: string; // http://help.developer.org/error_product_creator
    Data: string; // Anything

    constructor Create(const ATitle: string; const AMessage: string; const ADescription: string; const ASolution: string; const ASource: string = ''; const ACode: string = ''; const ACodeName: string = ''; const AUri: string = ''; const AData: string = ''); overload;
    constructor Create(const AMessage: string; const ADescription: string = ''); overload;
    constructor Create(const AJSONObject: TJSONObject; const AOwner: Boolean = False); overload;

    function ToJSONObject(const AIncludeAll: Boolean = True): TJSONObject; overload;
    function ToJSONString(const AIncludeAll: Boolean = True): string; overload;
  end;

{$SCOPEDENUMS ON}
  TDataValidatorLocaleLanguage = (tl_en_US, tl_de_DE, tl_fr_FR, tl_it_IT, tl_es_ES, tl_ru_RU, tl_pt_BR);
  TDataValidatorCheckAll = (tcAll, tcFirst);
{$SCOPEDENUMS OFF}

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

constructor TDataValidatorMessage.Create(const ATitle: string; const AMessage: string; const ADescription: string; const ASolution: string; const ASource: string = ''; const ACode: string = ''; const ACodeName: string = ''; const AUri: string = ''; const AData: string = '');
begin
  Self.Title := ATitle;
  Self.Message := AMessage;
  Self.Description := ADescription;
  Self.Solution := ASolution;

  Self.Source := ASource;
  Self.Code := ACode;
  Self.CodeName := ACodeName;
  Self.Uri := AUri;

  Self.Data := AData;
end;

constructor TDataValidatorMessage.Create(const AMessage: string; const ADescription: string = '');
begin
  Self.Message := AMessage;
  Self.Description := ADescription;
end;

constructor TDataValidatorMessage.Create(const AJSONObject: TJSONObject; const AOwner: Boolean = False);
begin
  if not Assigned(AJSONObject) then
    Exit;

  try
    Self.Title := AJSONObject.GetValue<string>('title', Self.Title);
    Self.Message := AJSONObject.GetValue<string>('message', Self.Message);
    Self.Description := AJSONObject.GetValue<string>('description', Self.Description);
    Self.Solution := AJSONObject.GetValue<string>('solution', Self.Solution);
    Self.Source := AJSONObject.GetValue<string>('source', Self.Source);
    Self.Code := AJSONObject.GetValue<string>('code', Self.Code);
    Self.CodeName := AJSONObject.GetValue<string>('code_name', Self.CodeName);
    Self.Uri := AJSONObject.GetValue<string>('uri', Self.Uri);
    Self.Data := AJSONObject.GetValue<string>('data', Self.Data);
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

  if not Self.Solution.IsEmpty or AIncludeAll then
    LJO.AddPair('solution', Self.Solution);

  if not Self.Source.IsEmpty or AIncludeAll then
    LJO.AddPair('source', Self.Source);

  if not Self.Code.IsEmpty or AIncludeAll then
    LJO.AddPair('code', Self.Code);

  if not Self.CodeName.IsEmpty or AIncludeAll then
    LJO.AddPair('code_name', Self.CodeName);

  if not Self.Uri.IsEmpty or AIncludeAll then
    LJO.AddPair('uri', Self.Uri);

  if not Self.Data.IsEmpty or AIncludeAll then
    LJO.AddPair('data', Self.Data);

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
