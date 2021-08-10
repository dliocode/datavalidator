{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.Key.IsOptional;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TDataValidatorJSONKeyIsOptional = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONKeyIsOptional }

constructor TDataValidatorJSONKeyIsOptional.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TDataValidatorJSONKeyIsOptional.Checked: IDataValidatorResult;
var
  R: Boolean;
  LValue: string;
  LJSONPair: TJSONPair;
begin
  R := True;
  LValue := GetValueAsString;

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    R := not Assigned(LJSONPair);
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
