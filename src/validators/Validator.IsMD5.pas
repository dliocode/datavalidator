{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsMD5;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsMD5 = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsMD5 }

constructor TValidatorIsMD5.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsMD5.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, '^(?!^[\d]*$)(?!^[a-fA-F]*$)([a-f\d]{32}|[A-F\d]{32})+$');

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
