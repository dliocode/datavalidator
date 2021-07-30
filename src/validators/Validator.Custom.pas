{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.Custom;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorCustom = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCustomExecute: TDataValidatorCustomExecute;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const ACustomExecute: TDataValidatorCustomExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorCustom }

constructor TValidatorCustom.Create(const ACustomExecute: TDataValidatorCustomExecute; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCustomExecute := ACustomExecute;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorCustom.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if Assigned(FCustomExecute) then
    try
      R := FCustomExecute(LValue);
    except
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
