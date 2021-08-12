{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsPort;

interface

uses
  DataValidator.ItemBase,
  System.Math, System.SysUtils;

type
  TValidatorIsPort = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsPort }

constructor TValidatorIsPort.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsPort.Check: IDataValidatorResult;
var
  LValue: Variant;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  try
    if not Trim(LValue).IsEmpty then
      R := InRange(LValue, 0, 65535);
  except
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
