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
