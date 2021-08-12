{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit Validator.IsCPFCNPJ;

interface

uses
  DataValidator.ItemBase, Validator.IsCPF, Validator.IsCNPJ,
  System.SysUtils;

type
  TValidatorIsCPFCNPJ = class(TDataValidatorItemBase, IDataValidatorItem)
  private
    FValidatorCPF: IDataValidatorItem;
    FValidatorCNPJ: IDataValidatorItem;
  public
    function Check: IDataValidatorResult;
    constructor Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
  end;

implementation

{ TValidatorIsCPFCNPJ }

constructor TValidatorIsCPFCNPJ.Create(const AMessage: string; const AExecute: TDataValidatorInformationExecute = nil);
begin
  FMessage := AMessage;
  FExecute := AExecute;

  FValidatorCPF := TValidatorIsCPF.Create(AMessage, AExecute);
  FValidatorCNPJ := TValidatorIsCNPJ.Create(AMessage, AExecute);
end;

function TValidatorIsCPFCNPJ.Check: IDataValidatorResult;
var
  R: Boolean;
  LResult: IDataValidatorResult;
  LValue: string;
begin
  FValidatorCPF.SetIsNot(FIsNot);
  FValidatorCPF.SetValue(FValue);

  LResult := FValidatorCPF.Check;
  R := LResult.OK;
  LValue := LResult.Values[0];

  if not R then
  begin
    FValidatorCNPJ.SetIsNot(FIsNot);
    FValidatorCNPJ.SetValue(FValue);

    LResult := FValidatorCNPJ.Check;
    R := LResult.OK;
    LValue := LResult.Values[0];
  end;

  Result := TDataValidatorResult.Create(R, TDataValidatorInformation.Create(LValue, FMessage, FExecute));
end;

end.
