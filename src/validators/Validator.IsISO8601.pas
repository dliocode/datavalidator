{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsISO8601; // https://en.wikipedia.org/wiki/ISO_8601

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.DateUtils;

type
  TValidatorIsISO8601 = class(TDataValidatorItemBase, IDataValidatorItem)
  private
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsISO8601 }

constructor TValidatorIsISO8601.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsISO8601.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  LDateTime: TDateTime;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    R := TryISO8601ToDate(LValue, LDateTime);

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
