{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsPostalCode;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsPostalCode = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function GetPattern: string;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsPostalCode }

constructor TValidatorIsPostalCode.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsPostalCode.Checked: IDataValidatorResult;
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

function TValidatorIsPostalCode.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := '^\d{5}(-\d{4})?$';
    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := '^\d{5}$';
    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := '^\d{2}\s?\d{3}$';
    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := '^\d{5}$';
    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := '^(5[0-2]{1}|[0-4]{1}\d{1})\d{3}$';
    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := '^\d{6}$';
    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := '^\d{5}\-?\d{3}$';
  end;
end;

end.
