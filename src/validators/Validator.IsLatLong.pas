{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsLatLong;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils,System.StrUtils, System.RegularExpressions;

type
  TValidatorIsLatLong = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FCheckDMS: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const ACheckDMS: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsLatLong }

constructor TValidatorIsLatLong.Create(const ACheckDMS: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FCheckDMS := ACheckDMS;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsLatLong.Checked: IDataValidatorResult;
const
  C_LAT = '^\(?[+-]?(90(\.0+)?|[1-8]?\d(\.\d+)?)$';
  C_LONG = '^\s?[+-]?(180(\.0+)?|1[0-7]\d(\.\d+)?|\d{1,2}(\.\d+)?)\)?$';
  C_LAT_DMS = '^(([1-8]?\d)\D+([1-5]?\d|60)\D+([1-5]?\d|60)(\.\d+)?|90\D+0\D+0)\D+[NSns]?$';
  C_LONG_DMS = '^\s*([1-7]?\d{1,2}\D+([1-5]?\d|60)\D+([1-5]?\d|60)(\.\d+)?|180\D+0\D+0)\D+[EWew]?$';
var
  LValue: string;
  R: Boolean;
  LValueLatLong: TArray<string>;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValueLatLong := LValue.Split([',']);

    if Length(LValueLatLong) = 2 then
    begin
      if FCheckDMS then
        R := TRegEx.IsMatch(LValueLatLong[0], C_LAT_DMS) and  TRegEx.IsMatch(LValueLatLong[1], C_LONG_DMS)
      else
        R := TRegEx.IsMatch(LValueLatLong[0], C_LAT) and  TRegEx.IsMatch(LValueLatLong[1], C_LONG);
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
