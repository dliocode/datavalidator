{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataValidator.Schema;

interface

uses
  DataValidator.Intf, DataValidator.Schema.Base;

type
  TDataValidatorSchema = class(TInterfacedObject, IDataValidatorSchema)
  private
  public
    function Validate: IDataValidatorSchemaBaseContext;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDataValidatorSchema }

constructor TDataValidatorSchema.Create;
begin
end;

destructor TDataValidatorSchema.Destroy;
begin
  inherited;
end;

function TDataValidatorSchema.Validate: IDataValidatorSchemaBaseContext;
begin
  Result := TDataValidatorSchemaBase.Create(Self);
end;

end.
