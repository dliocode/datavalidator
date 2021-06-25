{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Sanitizer.NormalizeEmail;

interface

uses
  DataValidator.ItemBase.Sanitizer,
  System.SysUtils;

type
  TSanitizerNormalizeEmail = class(TDataValidatorItemBaseSanitizer)
  private
    FAllLowercase: Boolean;
    FGmailRemoveDots: Boolean;
  public
    function Sanitize: TValue; override;
    constructor Create(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True);
  end;

implementation

{ TSanitizerNormalizeEmail }

constructor TSanitizerNormalizeEmail.Create(const AAllLowercase: Boolean = True; const AGmailRemoveDots: Boolean = True);
begin
  FAllLowercase := AAllLowercase;
  FGmailRemoveDots := AGmailRemoveDots;
end;

function TSanitizerNormalizeEmail.Sanitize: TValue;
var
  LValue: string;
  LEmail: TArray<string>;
  LUser: string;
  LDomain: string;
begin
  LValue := GetValueAsString;

  LEmail := Trim(LValue).Split(['@']);

  if Length(LEmail) <> 2 then
    Exit;

  LUser := LEmail[0];
  LDomain := LEmail[1];

  if FAllLowercase then
  begin
    LUser := LowerCase(LUser);
    LDomain := LowerCase(LDomain);
  end;

  if FGmailRemoveDots then
    if LDomain.ToLower.Contains('gmail') then
      LUser := LUser.Replace('.', '');

  LValue := Format('%s@%s', [LUser, LDomain]);

  SetValueAdapter(LValue);

  Result := FValue;
end;

end.
