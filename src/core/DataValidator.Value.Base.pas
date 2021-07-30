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
  TDataValidatorValueBase = class(TDataValidatorContext<IDataValidatorValueBase>, IDataValidatorValueBase)
  private
    [weak]
    FResult: IDataValidatorValueResult;
  public
    function &End(): IDataValidatorValueResult;

    constructor Create(const AResult: IDataValidatorValueResult; const AValue: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorValueBase }

constructor TDataValidatorValueBase.Create(const AResult: IDataValidatorValueResult; const AValue: string);
begin
  inherited Create(Self, AValue);
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

end.
