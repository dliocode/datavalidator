{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Schema.Base;

interface

uses
  DataValidator.Intf, DataValidator.Context, DataValidator.Context.Intf;

type
  TDataValidatorSchemaBase = class(TDataValidatorContext<IDataValidatorSchemaBase>, IDataValidatorSchemaBase, IDataValidatorSchemaContext)
  private
    [weak]
    FDataValidatorSchema: IDataValidatorSchema;
  public
    function &End(): IDataValidatorSchemaContext;

    constructor Create(const ADataValidatorSchema: IDataValidatorSchema); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorSchemaBase }

constructor TDataValidatorSchemaBase.Create(const ADataValidatorSchema: IDataValidatorSchema);
begin
  inherited Create(Self, '');
  FDataValidatorSchema := ADataValidatorSchema;
end;

destructor TDataValidatorSchemaBase.Destroy;
begin
  inherited Destroy;
end;

function TDataValidatorSchemaBase.&End: IDataValidatorSchemaContext;
begin
  Result := Self;
end;

end.
