{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsAlpha;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsAlpha = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function GetPattern: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsAlpha }

constructor TValidatorIsAlpha.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
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

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

function TValidatorIsAlpha.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := '^[A-Za-z\s]+$';
    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := '^[A-ZÄÖÜßa-zäöüß\s]+$';
    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := '^[A-ZÀÂÆÇÉÈÊËÏÎÔŒÙÛÜŸa-zàâæçéèêëïîôœùûüÿ\s]+$';
    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := '^[A-ZÀÉÈÌÎÓÒÙa-zàéèìîóòù\s]+$';
    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := '^[A-ZÁÉÍÑÓÚÜa-záéíñóúü\s]+$';
    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := '^[А-ЯЁа-яё\s]+$';
    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := '^[A-ZÃÁÀÂÄÇÉÊËÍÏÕÓÔÖÚÜa-zãáàâäçéêëíïõóôöúü\s]+$';
  end;
end;

end.
