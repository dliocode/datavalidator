{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Value.Base;

interface

uses
  DataValidator.Intf, DataValidator.Context;

type
  TDataValidatorValueBase = class(TDataValidatorContext<IDataValidatorValueBase>, IDataValidatorValueBase, IDataValidatorValueValues)
  private
    [weak]
    FResult: IDataValidatorValueResult;
    FValue: TArray<string>;
  public
    function &End(): IDataValidatorValueResult;

    function GetValues: TArray<string>;

    constructor Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorValueBase }

constructor TDataValidatorValueBase.Create(const AResult: IDataValidatorValueResult; const AValue: TArray<string>);
begin
  inherited Create(Self, '');
  FValue := AValue;
  FResult := AResult;
end;

destructor TDataValidatorValueBase.Destroy;
begin
  inherited Destroy;
end;

function TDataValidatorValueBase.&End(): IDataValidatorValueResult;
begin
  Result := FResult;
end;

function TDataValidatorValueBase.GetValues: TArray<string>;
begin
  Result := FValue;
end;

end.
