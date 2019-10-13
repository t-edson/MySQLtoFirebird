{Unit to work with MYSQL Scripts.
By the moment, only one routine was implemented (to read a SQL script in the syntax
of MySQL and load to the Model).}
unit DBModelMySQL;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, SynFacilHighlighter, SynFacilBasic, XpresBas,
  MisUtils, LazLogger, SynEditHighlighter, DBModelBase;
type

  { TMySQLScript }

  TMySQLScript = class
  private  //Token attributes
    TNIDENTIF, TNKEYWORD, TNSTRING: Integer;
    TNDELIMITER: Integer;
    TNQIDENTIF: Integer;
    function getValueBrackets(out content: string; out lstItems: TStringDynArray
      ): string;
  private
    model: TDBModelBase;  //Reference to the model
    xCon: TContext;
    xLex: TSynFacilSyn;   //Reference to a MySQL lexer
  private //Parser functions
    procedure GetUntilNextLine;
    function TrimQuotesInv(tok: string): string;
    function TrimQuotesInCurLine: string;
    function ReadLine(out lin: string): boolean;
    function ExtractUntilTok(tok: string): boolean;
    function getIdentfier(out ident: string): boolean;
    function getToken(tok: string): boolean;
    function getTokenStr(tok: string): boolean;
    function getTokenStr(tok1, tok2: string): boolean;
    function getTokenStr(tok1, tok2, tok3: string): boolean;
    function getTableReference(out database, table: string): boolean;
  private  //Read string
    function getCreateSchemaSQL(): string;
    function getCreateTableSQL(): string;
    function getInsertSQL(): string;
  public
    procedure readScript(sqlIn: TStrings );
    constructor Create(model0: TDBModelBase; mysqlLex: TSynFacilSyn);
    destructor Destroy; override;
  end;


implementation

procedure TMySQLScript.GetUntilNextLine;
{Toma los tokens hasta el token de fin de línea, de modo que el siguiente token
sea el inicio de la siguiente línea.}
begin
  if xCon.Eof then exit;
  repeat until xCon.Next;
end;
function TMySQLScript.TrimQuotesInv(tok: string): string;
{Extrae, de una palabra, las comillas invertidas que la delimitan, típicas de MySQL:
`nombre_tabla` -> nombre_tabla
`nombre_campo` -> nombre_campo
}
begin
  if tok[1] = '`' then begin
    Result := copy(tok, 2, length(tok)-2);  //quita comillas
  end else begin
    Result := tok;
  end;
end;
function TMySQLScript.TrimQuotesInCurLine: string;
{Extrae, las comillas invertidas, de todos los tokens de la línea actual (apuntada pro
xCon) que tengan esas comillas.
select `nombre_campo` from `nombre_tabla` -> select nombre_campo from nombre_tabla
}
begin
  Result := '';
  repeat
    Result := Result + TrimQuotesInv(xCon.Token);
    xCon.Next;
  until xCon.Lex.GetEol;
end;
function TMySQLScript.ReadLine(out lin: string): boolean;
{Lee una línea del contexto de entrada y deja el contexto apuntando al primer token
de la línea. Si ya no hay más líneas, devuelve FALSE}
begin
  if xCon.Eof then begin
    lin := '';   //Llegó al final del archivo
    exit(false);
  end;
  if xCon.lex.State.posTok = 1 then begin
    //Está al inicio de la línea
    lin := xCon.CurLine;
    exit(true);
  end else begin
    repeat
    until xCon.Next or xCOn.Eof;
    if xCOn.Eof then begin
      lin := '';   //Llegó al final del archivo
      exit(false);
    end else begin
      lin := xCon.CurLine;  //Ya está en el primer token
      exit(true);
    end;
  end;
end;
function TMySQLScript.ExtractUntilTok(tok: string): boolean;
{Extrae tokens hasta encontrar el que se indica. Si no lo enucentra, extrae hasta el
final de la línea, y devuelve FALSE.}
begin
  repeat
    xCon.Next;
  until xCon.lex.GetEol or (xCon.Token = tok);
  Result := (xCon.Token = tok);
end;
function TMySQLScript.getIdentfier(out ident: string): boolean;
{Get an identifier in the current position of the cotext "xCon".
If not identifier found, returns FALSE.}
begin
  xCon.SkipWhites;
  if xCon.Eof then exit(false);
  if (xCon.TokenType = TNIDENTIF) or
     (xCon.TokenType = TNKEYWORD)  then begin
    ident := xCon.Token;
  end else if xCon.TokenType = TNQIDENTIF then begin
    ident := TrimQuotesInv(xCon.Token);  //trim quotes
  end else begin
    //Not an identifier
    exit(false);
  end;
  xCon.Next;
  xCon.SkipWhites;
  exit(true);
end;
function TMySQLScript.getToken(tok: string): boolean;
{Get the specified token in the current position of the context "xCon".
If the token doesn't match, returns FALSE.}
begin
  xCon.SkipWhites;
  if xCon.Token<>tok then begin
    exit(false);
  end;
  xCon.Next;
  xCon.SkipWhites;
  exit(true);
end;
function TMySQLScript.getTokenStr(tok: string): boolean;
{Get the specified string token (case insensitive) in the current position of the
context "xCon". If the token doesn't match, returns FALSE.}
begin
  xCon.SkipWhites;
  if UpperCase(xCon.Token)<>tok then begin
    exit(false);
  end;
  xCon.Next;
  xCon.SkipWhites;
  exit(true);
end;
function TMySQLScript.getTokenStr(tok1, tok2: string): boolean;
{Version of getTokenStr() with two identifiers. If not found all the identifiers,
returns FALSE and the context returns to the initial position.}
var
  st: TFaLexerState;
begin
  st := xCon.lex.State;  //Save initial position.
  if getTokenStr(tok1) then begin
    if getTokenStr(tok2) then begin
      exit(true);
    end else begin
      xCon.lex.State := st;  //Restore state
      exit(false);
    end;
  end else begin
    exit(false);
  end;
end;
function TMySQLScript.getTokenStr(tok1, tok2, tok3: string): boolean;
{Version of getTokenStr() with three identifiers. If not found all the identifiers,
returns FALSE and the context returns to the initial position.}
var
  st: TFaLexerState;
begin
  st := xCon.lex.State;  //Save initial position.
  if getTokenStr(tok1) then begin
    if getTokenStr(tok2) then begin
      if getTokenStr(tok3) then begin
        exit(true);
      end else begin
        xCon.lex.State := st;  //Restore state
        exit(false);
      end;
    end else begin
      xCon.lex.State := st;  //Restore state
      exit(false);
    end;
  end else begin
    exit(false);
  end;
end;
function TMySQLScript.getTableReference(out database, table: string): boolean;
{Get an identifier for a table from the current position of the context "xCon".
The reference for a table could be any of the following cases:
- table_name
- database.table_name
- `database`.`table_name`

If not table identifier found, returns FALSE.}
var
  tmp: String;
begin
//tmp := xCon.Token;
  if not getIdentfier(tmp) then exit(false);
  if getToken('.') then begin
    database := tmp;
    if not getIdentfier(table) then exit(false);
    exit(true);
  end else begin
    //Only table name.
    database := '';
    table := tmp;
    exit(true);
  end;
end;
function TMySQLScript.getValueBrackets(out content: string; out lstItems: TStringDynArray): string;
{Get the value of the text surrounded by '(' and ')'. Includes '(' and ')'.
If error found, returns the error message.}
var
  nItems: Integer;
begin
  nItems := 0;
  content := '(';
  while not xCon.Eof and (xCon.Token<>')') do begin
    inc(nItems);
    SetLength(lstItems, nItems);
    lstItems[nItems-1] := xCon.Token;
    content := content + xCon.Token;
    xCon.Next;
    if getToken(',') then begin
      //Folows item, normal.
      content := content + ',';
      continue;
    end else begin
      //Must be the end of the list
      if getToken(')') then begin
        content := content + ')';
        break;
      end;
    end;
  end;
  if xCon.Eof then begin
    exit('Unexpected End of file. Not found ")".');
  end;
  exit('');
end;
//Read string
function TMySQLScript.getCreateSchemaSQL(): string;
{Read scheme creation sentence, in the form CREATE SCHEMA...
If error found, returns the error message.}
var
  tmp, schName: string;
begin
  getIdentfier(tmp);  //Get CREATE
  getIdentfier(tmp);  //Get TABLE
  if getTokenStr('IF') and not getTokenStr('NOT', 'EXISTS') then begin
    exit('Expected NOT EXISTS');
  end;  //Get TABLE
  if getTableReference(tmp, schName) then begin
    model.dataBaseName := schName;
  end else begin  //No encontró
    exit('Table name expected');
  end;
  if getTokenStr('DEFAULT') and not getTokenStr('CHARACTER', 'SET') then begin
    exit('Expected NOT EXISTS');
  end;  //Get TABLE
  model.defaultCharset := xCon.Token;
  xCon.Next;
  if not getToken(';') then exit('Expected ";"');
end;
function TMySQLScript.getCreateTableSQL(): string;
{Read table definition, in the form CREATE TABLE...
If error found, returns the error message.}
  function ReadConstraintEvent(out event: string): string;
  begin
    if getTokenStr('ON', 'DELETE') then begin
       if getTokenStr('NO', 'ACTION') then begin
         //OK
         event := 'ON DELETE NO ACTION';
       end else begin
         exit('Expected NO ACTION');
       end;
    end else if getTokenStr('ON', 'UPDATE') then begin
       if getTokenStr('NO', 'ACTION') then begin
         //OK
         event := 'ON UPDATE NO ACTION';
       end else begin
         exit('Expected NO ACTION');
       end;
    end;
    exit('');
  end;

var
  tabName, tmp, refTab, Err, event, colName, colType, constName: string;
  table, forTable: TDBTable;
  col, columnPK, columnFK: TDBColumn;
  cns: TDBConstraint;
  idx: TDBIndex;
begin
  getIdentfier(tmp);  //Get CREATE
  getIdentfier(tmp);  //Get TABLE
  if getTokenStr('IF') and not getTokenStr('NOT', 'EXISTS') then begin
    exit('Expected NOT EXISTS');
  end;  //Get TABLE
  if getTableReference(tmp, tabName) then begin
    //Encontró nombre de tabla
    table := model.AddTable(tabName);
    if not getToken('(') then exit('Expected "(".');
    while not xCon.Eof and (xCon.token <> ')') do begin
      xCon.SkipWhites;
      //Puede seguir nombre de columna, índice, o constraint
      if getTokenStr('PRIMARY') then begin
        if not getIdentfier(tmp) then exit('Identifier expected');  //Get "KEY"
        if not getToken('(') then exit('Expected "(".');
        if not getIdentfier(tmp) then exit('Identifier expected');
        if not getToken(')') then exit('Expected ")"');
        //Add PK to the table
        columnPK := table.GetColumnByName(tmp);
        if columnPK = nil then exit('PK column doesn''t exist.');
        table.primary.AddColumn(columnPK);
        //Finish sentence
        getToken(',');  //It could fails if if the last sentence
      end else if getTokenStr('INDEX') then begin
        if not getIdentfier(tmp) then exit('Identifier expected');  //Get index name
        if not getToken('(') then exit('Expected "(".');
        if not getIdentfier(tmp) then exit('Identifier expected');  //Index field
        idx := table.AddIndex(tmp);
        getIdentfier(tmp);  //ASC or DESC
        if not getToken(')') then exit('Expected ")"');
        getIdentfier(tmp);  //"VISIBLE"
        getToken(',');  //It could fails if if the last sentence
      end else if getTokenStr('CONSTRAINT') then begin
        if not getIdentfier(constName) then exit('Identifier expected');  //Get constraint name
        if getTokenStr('FOREIGN') then begin
           if not getIdentfier(tmp) then exit('Identifier expected');  //get KEY
           cns := table.AddConstraint(constName, ctypFK);  //Add constraint
           //Add local columns
           if not getToken('(') then exit('Expected "(".');
           if not getIdentfier(tmp) then exit('Identifier expected');
           if not getToken(')') then exit('Expected ")"');
           columnFK := table.GetColumnByName(tmp);
           if columnFK = nil then exit('PK column doesn''t exist.');
           cns.AddLocalColumn(columnFK);
           //Add foreign table
           if not getTokenStr('REFERENCES') then exit('Expected "REFERENCES"');
           if not getTableReference(tmp, refTab) then exit('Table name expected');
           forTable := model.GetTableByName(refTab);
           if forTable = nil then exit('Table referenced doesn''t exist.');
           cns.foreignTable := forTable;
           //Add foreign columns
           if not getToken('(') then exit('Expected "(".');
           if not getIdentfier(tmp) then exit('Identifier expected');
           if not getToken(')') then exit('Expected ")"');
           columnFK := table.GetColumnByName(tmp);
           if columnFK = nil then exit('PK column doesn''t exist.');
           cns.AddForeignColumn(columnFK);

           while (xCon.Token<>',') and (xCon.Token<>')') do begin
             Err := ReadConstraintEvent(event);
             if Err<>'' then exit(Err);
           end;
           getToken(',');  //It could fails if if the last sentence
        end else begin
          //Not recognized other constraint by now.
          exit('Unknown constraint: ' + xCon.Token);
        end;
      end else begin
        //Debe ser una columna común
        if not getIdentfier(colName) then exit('Colum nname expected');  //Column name
        col := table.AddColumn(colName);  //Add column
        //Get the column type
        if not getIdentfier(colType) then exit('Column type expected');  //Column name
        col.datatype := colType;
        tmp := '';
        if getToken('(') then begin
          Err := getValueBrackets(tmp, col.enumItems);
          if Err<>'' then exit(Err);
        end;
        col.dataTipeAd := tmp;
        //Get de Nullable
        if getTokenStr('NULL') then begin
          col.nullable := true;
        end else if getTokenStr('NOT','NULL') then begin
          col.nullable := false;
        end else begin
          col.nullable := true;
        end;
        if getTokenStr('DEFAULT') then begin
           col.defaultStr := xCon.Token;
           xCon.Next;
        end;
        if getTokenStr('COMMENT') then begin
           col.comment := xCon.Token;
           xCon.Next;
        end;
        //Finish sentence
        getToken(',');  //It could fails if if the last sentence
      end;
    end;
    if xCon.token = ')' then begin
      exit('');
    end else begin
      exit('');
    end;
  end else begin  //No encontró
    exit('Table name expected');
  end;
end;
function TMySQLScript.getInsertSQL(): string;
var
  tmp, tabName, Err, listColumns, listValues: string;
  lstItems: TStringDynArray;
begin
  getIdentfier(tmp);  //Get INSERT
  getIdentfier(tmp);  //Get INTO
  if NOT getTableReference(tmp, tabName) then begin
    exit('Table name expected');
  end;

  if getToken('(') then begin
    Err := getValueBrackets(listColumns, lstItems);
    if Err<>'' then exit(Err);
  end else begin
    listColumns := '';
  end;
  if not getTokenStr('VALUES') then exit('Expected "VALUES"');
  if getToken('(') then begin
    Err := getValueBrackets(listValues, lstItems);
    if Err<>'' then exit(Err);
  end else begin
    exit('Expected "(".');
  end;

  model.scripts.Add('INSERT INTO ' + tabName + listColumns + ' VALUES ' + listValues + ';');
  if not getToken(';') then exit('Expected ";".');

end;
procedure TMySQLScript.readScript(sqlIn: TStrings);
{Read a script in the syntax of MySQL and losd it to the model.}
var
  linea, Err: string;
begin
  model.Clear;
  //Se usará "xCon", para extraer tokens.
  xCon.SetSource(sqlIn, true);  //Crea copia para no interferir
  while ReadLine(linea) do begin
    //Inicio de línea
    {El token actual es “xCon.Token”
    El tipo de token actual es “xCon.TokenType”
    El atributo de token actual es “xCon.TokenAttrib”
    El bloque actual es “xCon.Block”}
    if StringLike(linea, '--*') then begin     //Filtra comentarios
      GetUntilNextLine;
    end else if StringLike(linea, 'SET *') then begin  //Filtra SET
      GetUntilNextLine;
    end else if StringLike(linea, 'CREATE SCHEMA*') then begin     //Filtra comentarios
      Err := getCreateSchemaSQL();
      if Err<>'' then begin
        MsgErr('Error en formato MySQL: ' + Err + '(line: %d)', [xCon.lex.GetY]);
        exit;
      end;
    end else if StringLike(linea, 'USE *') then begin     //Filtra comentarios
      GetUntilNextLine;
    end else if StringLike(linea, 'START *') then begin     //Start transaction
      GetUntilNextLine;
    end else if StringLike(linea, 'CREATE TABLE *') then begin  //Creación de tabla
      Err := getCreateTableSQL();
      if Err<>'' then begin
        MsgErr('Error en formato MySQL: ' + Err + '(line: %d)', [xCon.lex.GetY]);
        exit;
      end;
    end else if StringLike(linea, 'INSERT *') then begin     //INSERT sentence
      Err := getInsertSQL();
      if Err<>'' then begin
        MsgErr('Error en formato MySQL: ' + Err + '(line: %d)', [xCon.lex.GetY]);
        exit;
      end;
    end else begin
      GetUntilNextLine;
    end;
  end;

end;

constructor TMySQLScript.Create(model0: TDBModelBase; mysqlLex: TSynFacilSyn);
begin
  model := model0;  //We need the reference to the model
  xLex  := mysqlLex;  //We need a MySQL lexer to read the SQL scripts.
  xCon := TContext.Create;
  xCon.DefSyn(xLex);

  TNIDENTIF  := xCon.lex.tnIdentif;
  TNKEYWORD  := xCon.lex.tnKeyword;
  TNSTRING   := xCon.lex.tnString ;
  TNDELIMITER:= xCon.lex.GetAttribIDByName('Delimiter');
  TNQIDENTIF := xCon.lex.GetAttribIDByName('Qidentif');
end;
destructor TMySQLScript.Destroy;
begin
  xCon.Destroy;
  inherited Destroy;
end;

end.

