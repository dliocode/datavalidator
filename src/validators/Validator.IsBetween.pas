{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsBetween;

interface

uses
  DataValidator.ItemBase,
  System.Math, System.SysUtils;

type
  TValidatorIsBetween = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValueA: TValue;
    FValueB: TValue;
  public
    function Check: IDataValidatorResult;

    constructor Create(const AValueA: TValue; const AValueB: TValue; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil); overload;
  end;

implementation

{ TValidatorIsBetween }

constructor TValidatorIsBetween.Create(const AValueA: TValue; const AValueB: TValue; const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FValueA := AValueA;
  FValueB := AValueB;
  FMessage := AMessage;
  FExecute := AExecute;
end;

function TValidatorIsBetween.Check: IDataValidatorResult;
var
  LValue: Variant;
  LValueA: Variant;
  LValueB: Variant;
  R: Boolean;
begin
  LValue := GetValueAsString;
  R := False;

  if not Trim(LValue).IsEmpty then
  begin
    LValueA := FValueA.AsVariant;
    LValueB := FValueB.AsVariant;

    try
      R := InRange(LValue, LValueA, LValueB);
    except
    end;
  end;

  if FIsNot then
    R := not R;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
