{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Types;

interface

uses
  System.SysUtils;

type
  TDataValidatorLocaleLanguage = (tl_en_US, tl_de_DE, tl_fr_FR, tl_it_IT, tl_es_ES, tl_ru_RU, tl_pt_BR);

  TDataValidatorInformationExecute = TProc;
  TDataValidatorCustomExecute = reference to function(const AValue: string): Boolean;

implementation

end.
