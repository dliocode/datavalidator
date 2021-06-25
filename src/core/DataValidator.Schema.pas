unit DataValidator.Schema;

interface

uses
  DataValidator.ItemBase.Intf, DataValidator.Base.Intf, DataValidator.Types, DataValidator.Base,
  System.SysUtils, System.Variants, System.Generics.Collections, System.JSON;

type
  TDataValidatorSchema = class(TInterfacedObject, IDataValidatorSchema)
  private
    FListSchema: TList<IDataValidatorsBase<IDataValidatorSchema>>;
  public
    function ListSchema: TList<IDataValidatorsBase<IDataValidatorSchema>>;

    constructor Create;
    destructor Destroy; override;

    class function New: IDataValidatorsBase<IDataValidatorSchema>;
  end;

implementation

{ TDataValidatorSchema }

class function TDataValidatorSchema.New: IDataValidatorsBase<IDataValidatorSchema>;
var
  LSchema: IDataValidatorSchema;
begin
  LSchema := TDataValidatorSchema.Create;

  LSchema.ListSchema.Add(TDataValidatorsBase<IDataValidatorSchema>.New(LSchema, ''));
  Result := LSchema.ListSchema.Last;
end;

constructor TDataValidatorSchema.Create;
begin
  FListSchema := TList < IDataValidatorsBase < IDataValidatorSchema >>.Create;
end;

destructor TDataValidatorSchema.Destroy;
begin
  FListSchema.Clear;
  FListSchema.DisposeOf;
  inherited;
end;

function TDataValidatorSchema.ListSchema: TList<IDataValidatorsBase<IDataValidatorSchema>>;
begin
  Result := FListSchema;
end;

end.
