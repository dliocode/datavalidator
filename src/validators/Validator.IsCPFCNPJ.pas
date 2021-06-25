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
    FMessage: string;
    FExecute: TDataValidatorInformationExecute;
    FValidatorCPF: IDataValidatorItem;
    FValidatorCNPJ: IDataValidatorItem;
  public
    function Checked: IDataValidatorResult;
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

function TValidatorIsCPFCNPJ.Checked: IDataValidatorResult;
var
  R: Boolean;
begin
  FValidatorCPF.SetIsNot(FIsNot);
  FValidatorCPF.SetValue(FValue);

  FValidatorCNPJ.SetIsNot(FIsNot);
  FValidatorCNPJ.SetValue(FValue);

  R := FValidatorCPF.Checked.OK;

  if not R then
    R := FValidatorCNPJ.Checked.OK;

  Result := TDataValidatorResult.New(R, TDataValidatorInformation.New(GetValueAsString, FMessage, FExecute));
end;

end.
