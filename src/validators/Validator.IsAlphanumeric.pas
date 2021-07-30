{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsAlphanumeric;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsAlphanumeric = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function GetPattern: string;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsAlphanumeric }

constructor TValidatorIsAlphanumeric.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsAlphanumeric.Checked: IDataValidatorResult;
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

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

function TValidatorIsAlphanumeric.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := '^[0-9A-Za-z\s]+$';
    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := '^[0-9A-ZÄÖÜßa-zäöüß\s]+$';
    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := '[0-9A-ZÀÂÆÇÉÈÊËÏÎÔŒÙÛÜŸa-zàâæçéèêëïîôœùûüÿ\s]+$';
    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := '[0-9A-ZÀÉÈÌÎÓÒÙa-zàéèìîóòù\s]+$';
    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := '^[0-9A-ZÁÉÍÑÓÚÜa-záéíñóúü\s]+$';
    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := '^[0-9А-ЯЁа-яё\s]+$';
    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := '^[0-9A-ZÃÁÀÂÄÇÉÊËÍÏÕÓÔÖÚÜa-zãáàâäçéêëíïõóôöúü\s]+$';
  end;
end;

end.
