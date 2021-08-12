{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsPassportNumber;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsPassportNumber = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    function GetPattern: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsPassportNumber }

constructor TValidatorIsPassportNumber.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsPassportNumber.Check: IDataValidatorResult;
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

function TValidatorIsPassportNumber.GetPattern: string;
begin
  case FLocaleLanguage of
    TDataValidatorLocaleLanguage.tl_en_US:
      Result := '^\d{9}$';
    TDataValidatorLocaleLanguage.tl_de_DE:
      Result := '^[CFGHJKLMNPRTVWXYZ0-9]{9}$';
    TDataValidatorLocaleLanguage.tl_fr_FR:
      Result := '^\d{2}[A-Z]{2}\d{5}$';
    TDataValidatorLocaleLanguage.tl_it_IT:
      Result := '^[A-Z0-9]{2}\d{7}$';
    TDataValidatorLocaleLanguage.tl_es_ES:
      Result := '^[A-Z0-9]{2}([A-Z0-9]?)\d{6}$';
    TDataValidatorLocaleLanguage.tl_ru_RU:
      Result := '^\d{2}\d{2}\d{6}$';
    TDataValidatorLocaleLanguage.tl_pt_BR:
      Result := '^[A-Z]{2}\d{6}$';
  end;
end;

end.
