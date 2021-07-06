{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsMACAddress; // MAC (Media Access Control) address

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.RegularExpressions;

type
  TValidatorIsMACAddress = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsMACAddress }

constructor TValidatorIsMACAddress.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsMACAddress.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TRegEx.IsMatch(LValue, '^(?:[0-9a-fA-F]{2}([-:]))([0-9a-fA-F]{2}\1){4}([0-9a-fA-F]{2})$');

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
