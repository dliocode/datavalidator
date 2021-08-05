{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Types;

interface

uses
  DataValidator.Result.Intf,
  System.SysUtils;

type
  TDataValidatorLocaleLanguage = (tl_en_US, tl_de_DE, tl_fr_FR, tl_it_IT, tl_es_ES, tl_ru_RU, tl_pt_BR);

  TDataValidatorCustomExecute = reference to function(const AValue: string): Boolean;
  TDataValidatorCustomMessageExecute = reference to function(const AValue: string; var AMessage: string): Boolean;

  TDataValidatorCustomSanitizerExecute = reference to function(const AValue: string): string;

implementation

end.
