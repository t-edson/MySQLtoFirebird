{Unit with definition of the class TDBModel wich is the class that represent a DataBase
Model in the Model-View architecture.}
unit DBModel;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, DBModelBase, DBModelMySQL, DBModelFirebird, SynFacilHighlighter;

type

    { TDBModel }
    TDBModel = class(TDBModelBase)
    private
      sqlLex: TSynFacilSyn;   //Reference to lexer for parser SQL
    public  //Input/Ouput
      MySQLScript: TMySQLScript;
      FireBirdScript: TFirebirdScript;
      procedure ReadScriptFromMySQL(sql: TSTrings);
      procedure WriteScriptFirebird(sql: TSTrings);
    public  //Initialization
      constructor Create(sqlLex0: TSynFacilSyn); reintroduce;
      destructor Destroy; override;
    end;

implementation

procedure TDBModel.ReadScriptFromMySQL(sql: TSTrings);
{Read a SQL script in teh syntax os MySQL. More properly, would be in the syntax of
MySQL Workbench.}
begin
  MySQLScript.readScript(sql);
end;

procedure TDBModel.WriteScriptFirebird(sql: TSTrings);
{Write the model as a script in the Firebird syntax.}
begin
  FireBirdScript.writeScript(sql);
end;

constructor TDBModel.Create(sqlLex0: TSynFacilSyn);
begin
  inherited Create;
  sqlLex := sqlLex0;
  MySQLScript    := TMySQLScript.Create(self, sqlLex);
  FireBirdScript := TFirebirdScript.Create(self);
end;

destructor TDBModel.Destroy;
begin
  FireBirdScript.Destroy;
  MySQLScript.Destroy;
  inherited Destroy;
end;


end.

