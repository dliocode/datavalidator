{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.EndsWith;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.StrUtils;

type
  TValidatorEndsWith = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueEndsWith: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Checked: IDataValidatorResult;
    constructor Create(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorEndsWith }

constructor TValidatorEndsWith.Create(const AValueEndsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute);
begin
  FValueEndsWith := AValueEndsWith;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorEndsWith.Checked: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  for I := Low(FValueEndsWith) to High(FValueEndsWith) do
  begin
    if FCaseSensitive then
      R := EndsStr(FValueEndsWith[I], LValue)
    else
      R := EndsStr(LowerCase(FValueEndsWith[I]), LowerCase(LValue));

    if R then
      Break;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
