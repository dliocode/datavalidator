{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsEquals;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Variants;

type
  TValidatorIsEquals = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueEquals: string;
    FCaseSensitive: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AValueEquals: string; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsEquals }

constructor TValidatorIsEquals.Create(const AValueEquals: string; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueEquals := AValueEquals;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsEquals.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;

  if FCaseSensitive then
    R := VarCompareValue(LValue, FValueEquals) = vrEqual
  else
    R := VarCompareValue(LowerCase(LValue), LowerCase(FValueEquals)) = vrEqual;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
