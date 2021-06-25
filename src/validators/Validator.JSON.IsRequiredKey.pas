{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.JSON.IsRequiredKey;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils;

type
  TDataValidatorJSONIsRequiredKey = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TDataValidatorJSONIsRequiredKey }

constructor TDataValidatorJSONIsRequiredKey.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TDataValidatorJSONIsRequiredKey.Checked: IDataValidatorResult;
var
  R: Boolean;
  LValue : string;
  LJSONPair: TJSONPair;
begin
  R := True;
  LValue := GetValueAsString;

  if FValue.IsType<TJSONPair> then
  begin
    LJSONPair := FValue.AsType<TJSONPair>;

    R := Assigned(LJSONPair);
  end;
//  else
//    R := not Trim(GetValueAsString).IsEmpty;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(LValue, FMessage, FExecute));
end;

end.
