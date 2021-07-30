{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.JSON.Base;

interface

uses
  DataValidator.Intf, DataValidator.JSON.Context,
  System.JSON;

type
  TDataValidatorJSONBase = class(TDataValidatorJSONContext<IDataValidatorJSONBase>, IDataValidatorJSONBase)
  private
    [weak]
    FResult: IDataValidatorJSONResult;
  public
    function &End(): IDataValidatorJSONResult;

    constructor Create(const AResult: IDataValidatorJSONResult; const AValue: TJSONPair); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorJSONBase }

constructor TDataValidatorJSONBase.Create(const AResult: IDataValidatorJSONResult; const AValue: TJSONPair);
begin
  inherited Create(Self, AValue);
  FResult := AResult;
end;

destructor TDataValidatorJSONBase.Destroy;
begin
  inherited Destroy;
end;

function TDataValidatorJSONBase.&End(): IDataValidatorJSONResult;
begin
  Result := FResult;
end;

end.
