{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.Regex.IsMatch; // REGEX (Regular Expression)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorRegexIsMatch = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FRegex: string;
  public
    function Check: IDataValidatorResult;
    constructor Create(const ARegex: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorRegexIsMatch }

constructor TValidatorRegexIsMatch.Create(const ARegex: string; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FRegex := ARegex;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorRegexIsMatch.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, FRegex);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
