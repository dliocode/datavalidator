{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsRGBColor; // RGB (Red, Green, Blue)

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsRGBColor = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsRGBColor }

constructor TValidatorIsRGBColor.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsRGBColor.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LRegexRGBColor: string;
  LRegexRGBAColor: string;
  LRegexRGBColorPercent: string;
  LRegexRGBAColorPercent: string;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LRegexRGBColor := '^rgb\((([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]),){2}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\)$';
    LRegexRGBAColor := '^rgba\((([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]),){3}(0?\.\d|1(\.0)?|0(\.0)?)\)$';
    LRegexRGBColorPercent := '^rgb\((([0-9]%|[1-9][0-9]%|100%),){2}([0-9]%|[1-9][0-9]%|100%)\)';
    LRegexRGBAColorPercent := '^rgba\((([0-9]%|[1-9][0-9]%|100%),){3}(0?\.\d|1(\.0)?|0(\.0)?)\)';

    R := TRegEx.IsMatch(LValue, LRegexRGBColor) or
         TRegEx.IsMatch(LValue, LRegexRGBAColor) or
         TRegEx.IsMatch(LValue, LRegexRGBColorPercent) or
         TRegEx.IsMatch(LValue, LRegexRGBAColorPercent);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
