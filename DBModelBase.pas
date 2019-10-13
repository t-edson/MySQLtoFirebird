{Unit with the basic class definition for the DataBase Model }
unit DBModelBase;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, types;   //This unit must have the minimal dependencies.
type
  //Column definition
  TDBColumn = class
    name      : string;  //Column name.
    datatype  : string;  //Column data type.
    dataTipeAd: string;  //Column data type additional
    nullable  : boolean; //Indicates if the column can be MULL.
    defaultStr: string;  //Default value for the column.
    comment   : string;  //Comment for the column.
    enumItems : TStringDynArray;  //Items for ENUM values
  end;
  TDBColumns = specialize TFPGObjectList<TDBColumn>;

  //Primary Key

  { TDBPrimaryKey }

  TDBPrimaryKey = class
  public
    pkColumns: TDBColumns;  //Reference to the columns of the PK
    procedure Clear;
    procedure AddColumn(dbCol: TDBColumn);
    function nCols: integer;
    function columnsStr: string;
  public  //Initialization
    constructor Create;
    destructor Destroy; override;
  end;

  //Index definition
  TDBIndex = class
    columIndex: string;
  end;
  TDBIndexes = specialize TFPGObjectList<TDBIndex>;

  TConstraintType = (
    ctypFK   //Foreign Key
  );

  TDBTable = class;   //Forward

  { TDBConstraint }

  TDBConstraint = class
    name: string;
    constType: TConstraintType;
    localColumns: TDBColumns;  //Reference to the local columns of the FK
    foreignTable: TDBTable;
    foreignColumns: TDBColumns;  //Reference to the foreign columns of the FK
    procedure AddLocalColumn(dbCol: TDBColumn);
    procedure AddForeignColumn(dbCol: TDBColumn);
    function localColumnsStr: string;
    function foreignColumnsStr: string;
  public  //Initialization
    constructor Create;
    destructor Destroy; override;
  end;
  TDBConstraints = specialize TFPGObjectList<TDBConstraint>;

  TDBObject = class
    name: string;
  end;
  TDBObjects = specialize TFPGObjectList<TDBObject>;

  { TDBTable }

  TDBTable = class(TDBObject)
  private
  public  //Elements
    columns: TDBColumns;
    primary: TDBPrimaryKey;
    indexes: TDBIndexes;  //Not used by now
    constraints: TDBConstraints;
    function AddColumn(colName: string): TDBColumn;
    function AddIndex(columIndex: string): TDBIndex;
    function AddConstraint(conName: string; conType: TConstraintType): TDBConstraint;
  public  //Find
    function GetColumnByName(colName: string): TDBColumn;
  public  //Initialization
    constructor Create;
    destructor Destroy; override;
  end;

  { TDBModelBase }
  TDBModelBase = class
  public  //Database properties.
    dataBaseName: string;
    defaultCharset: string;
  public
    //List of tables, views, ...
    dbObjects: TDBObjects;
    //List of scripts as text (INSERT ... SET ...)
    scripts: TStringList;
  public
    function AddTable(name: string): TDBTable;
    function GetTableByName(tabName: string): TDBTable;
  public  //Initialization
    procedure Clear;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TDBPrimaryKey }
procedure TDBPrimaryKey.Clear;
begin
  pkColumns.Clear;
end;
procedure TDBPrimaryKey.AddColumn(dbCol: TDBColumn);
begin
  pkColumns.Add(dbCol);
end;
function TDBPrimaryKey.nCols: integer;
begin
  exit(pkColumns.Count);
end;
function TDBPrimaryKey.columnsStr: string;
{Returns the list of PK columns separtaed by commas.}
var
  i: Integer;
begin
  if pkColumns.Count = 0 then exit('');
  Result := pkColumns[0].name;
  for i:=1 to pkColumns.Count-1 do begin
    Result := Result + ',' + pkColumns[i].name;
  end;
end;
constructor TDBPrimaryKey.Create;
begin
  pkColumns:= TDBColumns.Create(false);  //Only references
end;
destructor TDBPrimaryKey.Destroy;
begin
  pkColumns.Destroy;
  inherited Destroy;
end;

{ TDBConstraint }
procedure TDBConstraint.AddLocalColumn(dbCol: TDBColumn);
begin
  localColumns.Add(dbCol);
end;
procedure TDBConstraint.AddForeignColumn(dbCol: TDBColumn);
begin
  foreignColumns.Add(dbCol);
end;
function TDBConstraint.localColumnsStr: string;
{Returns the list of PK columns separtaed by commas.}
var
  i: Integer;
begin
  if localColumns.Count = 0 then exit('');
  Result := localColumns[0].name;
  for i:=1 to localColumns.Count-1 do begin
    Result := Result + ',' + localColumns[i].name;
  end;
end;
function TDBConstraint.foreignColumnsStr: string;
{Returns the list of PK columns separtaed by commas.}
var
  i: Integer;
begin
  if foreignColumns.Count = 0 then exit('');
  Result := foreignColumns[0].name;
  for i:=1 to foreignColumns.Count-1 do begin
    Result := Result + ',' + foreignColumns[i].name;
  end;
end;
constructor TDBConstraint.Create;
begin
  localColumns  := TDBColumns.Create(false);
  foreignColumns:= TDBColumns.Create(false);
end;
destructor TDBConstraint.Destroy;
begin
  foreignColumns.Destroy;
  localColumns.Destroy;
  inherited Destroy;
end;

{ TDBTable }
function TDBTable.AddColumn(colName: string): TDBColumn;
{Add a column object to the table}
begin
  Result := TDBColumn.Create;
  Result.name := colName;
  columns.Add(Result);
end;
function TDBTable.AddIndex(columIndex: string): TDBIndex;
{Add an index object to the table}
begin
  Result := TDBIndex.Create;
  Result.columIndex := columIndex;
  indexes.Add(Result);
end;
function TDBTable.AddConstraint(conName: string; conType: TConstraintType
  ): TDBConstraint;
begin
  Result := TDBConstraint.Create;
  Result.name := conName;
  Result.constType := conType;
  constraints.Add(Result);
end;
function TDBTable.GetColumnByName(colName: string): TDBColumn;
{Returns teh reference to a column from teh name. Is case insensitive.
If no found, retuns NIL.}
var
  col: TDBColumn;
begin
  colName := UpperCase(colName);
  for col in columns do begin
    if UpperCase(col.name) = colname then exit(col);
  end;
  //Not found
  exit(nil);
end;
constructor TDBTable.Create;
begin
  columns := TDBColumns.Create(true);
  primary := TDBPrimaryKey.Create;
  indexes := TDBIndexes.Create(true);
  constraints:= TDBConstraints.Create(true);
end;
destructor TDBTable.Destroy;
begin
  constraints.Destroy;
  indexes.Destroy;
  primary.Destroy;
  columns.Destroy;
  inherited Destroy;
end;

{ TDBModelBase }
function TDBModelBase.AddTable(name: string): TDBTable;
{Add a table to the model.}
begin
  Result := TDBTable.Create;
  Result.name := name;
  dbObjects.Add(Result);
end;
function TDBModelBase.GetTableByName(tabName: string): TDBTable;
{Returns the reference of a table with the specified name.
If not found, returns NIL.}
var
  obj: TDBObject;
begin
  tabName := UpperCase(tabName);
  for obj in dbObjects do begin
    if (obj.ClassType = TDBTable) and (UpperCase(obj.name) = tabName) then begin
      exit(TDBTable(obj));
    end;
  end;
  //Not found
  exit(nil);
end;
procedure TDBModelBase.Clear;
{Clear elements of the model}
begin
  dbObjects.Clear;
  scripts.Clear;
end;
constructor TDBModelBase.Create;
begin
  dbObjects  := TDBObjects.Create(true);
  scripts    := TStringList.Create;
end;
destructor TDBModelBase.Destroy;
begin
  scripts.Destroy;
  dbObjects.Destroy;
end;

end.

