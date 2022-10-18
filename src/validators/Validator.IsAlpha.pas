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

unit Validator.IsAlpha;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.StrUtils, System.RegularExpressions;

type
  TValidatorIsAlpha = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FAllowedCharacters: TArray<Char>;
    function GetCharactersAllow: string;
    function GetPattern: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AAllowedCharacters: TArray<Char>; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsAlpha }

constructor TValidatorIsAlpha.Create(const AAllowedCharacters: TArray<Char>; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  inherited Create;

  FAllowedCharacters := AAllowedCharacters;
  SetMessage(AMessage);
  SetExecute(AExecute);
end;

function TValidatorIsAlpha.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, GetPattern);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(FKey, FName, LValue, GetMessage, FExecute));
end;

function TValidatorIsAlpha.GetCharactersAllow: string;
var
  I: Integer;
begin
  Result := '';

  for I := Low(FAllowedCharacters) to High(FAllowedCharacters) do
  begin
    if MatchText(FAllowedCharacters[I], ['\', ']', '-']) then
      Result := Result + '\';

    Result := Result + FAllowedCharacters[I];
  end;
end;

function TValidatorIsAlpha.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := '^[A-Za-z\s%s]+$';
    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := '^[A-ZÄÖÜßa-zäöüß\s%s]+$';
    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := '^[A-ZÀÂÆÇÉÈÊËÏÎÔŒÙÛÜŸa-zàâæçéèêëïîôœùûüÿ\s%s]+$';
    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := '^[A-ZÀÉÈÌÎÓÒÙa-zàéèìîóòù\s%s]+$';
    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := '^[A-ZÁÉÍÑÓÚÜa-záéíñóúü\s%s]+$';
    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := '^[А-ЯЁа-яё\s%s]+$';
    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := '^[A-ZÃÁÀÂÄÇÉÊËÍÏÕÓÔÖÚÜa-zãáàâäçéêëíïõóôöúü\s%s]+$';
  end;

  Result := Format(Result, [GetCharactersAllow]);
end;

end.
