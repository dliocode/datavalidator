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
    FValueContains: TArray<string>;
    FCaseSensitive: Boolean;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AValueContains: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorContains }

constructor TValidatorContains.Create(const AValueContains: TArray<string>; const ACaseSensitive: Boolean; const AMessage: string; const AExecute: TDataValidatorInformationExecute);
begin
  FValueContains := AValueContains;
  FCaseSensitive := ACaseSensitive;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorContains.Check: IDataValidatorResult;
var
  LValue: string;
  R: Boolean;
  I: Integer;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
    for I := Low(FValueContains) to High(FValueContains) do
    begin
      if FCaseSensitive then
        R := Pos(FValueContains[I], LValue) > 0
      else
        R := Pos(LowerCase(FValueContains[I]), LowerCase(LValue)) > 0;

      if R then
        Break;
    end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
