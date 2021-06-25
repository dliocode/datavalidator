{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsRequired;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TValidatorIsRequired = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsRequired }

constructor TValidatorIsRequired.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsRequired.Checked: IDataValidatorResult;
var
  R: Boolean;
  LValue : string;
  LJSONPair: TJSONPair;
begin
  LValue := GetValueAsString;

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    R := Assigned(LJSONPair);
  end
  else
    R := not Trim(GetValueAsString).IsEmpty;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
