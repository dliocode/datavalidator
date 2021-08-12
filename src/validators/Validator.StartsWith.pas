{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.StartsWith;

interface

uses
  DataValidator.ItemBase,
  System.SysUtils, System.StrUtils;

type
  TValidatorStartsWith = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueStartsWith: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AValueStartsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorStartsWith }

constructor TValidatorStartsWith.Create(const AValueStartsWith: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute);
begin
  FValueStartsWith := AValueStartsWith;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorStartsWith.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    for I := Low(FValueStartsWith) to High(FValueStartsWith) do
    begin
      if FCaseSensitive then
        R := StartsStr(FValueStartsWith[I], LValue)
      else
        R := StartsStr(LowerCase(FValueStartsWith[I]), LowerCase(LValue));

      if R then
        Break;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
