{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsEquals;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.Variants;

type
  TValidatorIsEquals = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueEquals: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsEquals }

constructor TValidatorIsEquals.Create(const AValueEquals: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueEquals := AValueEquals;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsEquals.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    for I := Low(FValueEquals) to High(FValueEquals) do
    begin
      if FCaseSensitive then
        R := VarCompareValue(LValue, FValueEquals[I]) = vrEqual
      else
        R := VarCompareValue(LowerCase(LValue), LowerCase(FValueEquals[I])) = vrEqual;

      if R then
        Break;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
