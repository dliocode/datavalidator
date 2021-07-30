{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.Contains;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorContains = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueContains: string;
    FCaseSensitive: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AValueContains: string; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorContains }

constructor TValidatorContains.Create(const AValueContains: string; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueContains := AValueContains;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorContains.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
begin
  LValue := GetValueAsString;

  if FCaseSensitive then
    R := Pos(FValueContains, LValue) > 0
  else
    R := Pos(LowerCase(FValueContains), LowerCase(LValue)) > 0;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
