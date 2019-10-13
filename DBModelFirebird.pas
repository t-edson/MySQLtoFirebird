{Unit to work with Firebird Scripts.
By the moment, only one routine was implemented (to write a SQL script in the syntax
of Firebird from the Model).}
unit DBModelFirebird;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, DBModelBase;

type

  { TFirebirdScript }

  TFirebirdScript = class
  private
    model: TDBModelBase;  //Reference to the model
    //xCon: TContext;
  public
    procedure writeScript(sqlOut: TStrings);
    constructor Create(model0: TDBModelBase);
  end;

implementation

{ TFirebirdScript }
procedure TFirebirdScript.writeScript(sqlOut: TStrings);

  procedure AddCommaToList(lst: TStrings);
  begin
    lst[lst.Count-1] := lst[lst.Count-1] + ',';  //Add comma to the last line
  end;
  function ColumnDefinition(col: TDBColumn): string;
  {Get the SQL definition for a column.}
  var
    nulStr, defStr, colType: String;
  begin
    if col.nullable then nulStr := '' else nulStr := ' NOT NULL';
    if col.defaultStr='' then defStr := '' else defStr := ' DEFAULT ' + col.defaultStr;
    colType := col.datatype;
    if UpCase(colType) = 'DATETIME' then colType := 'TIMESTAMP';
    if UpCase(colType) = 'TINYINT' then colType := 'SMALLINT';
    Result := col.name + ' ' + colType + col.dataTipeAd + defStr + nulStr;
  end;
  function TypeOfItem(item: String): string;
  begin
    if item[1] = '''' then exit('VARCHAR');
    if item[1] = '"' then exit('VARCHAR');
    exit('INT');
  end;
var
  obj: TDBObject;
  tab: TDBTable;
  col: TDBColumn;
  con: TDBConstraint;
  i, maxSize, nEnum: Integer;
  str, enumType, domainName, comment, tmp: String;
begin
  sqlOut.BeginUpdate;
  sqlOut.Clear;

  sqlOut.Add('--SQL converted from MySQL script, generated from MySQL Workbench, to the Firebird syntax.');
  sqlOut.Add('');

  if model.defaultCharset<>'' then begin
    sqlOut.Add('CREATE DATABASE ''C:/' + model.dataBaseName + ''' ' +
               'DEFAULT CHARACTER SET ' + model.defaultCharset + ';');
    sqlOut.Add('');
  end else begin
    sqlOut.Add('CREATE DATABASE ''C:/' + model.dataBaseName + ''';');
    sqlOut.Add('');
  end;
  //Database objects
  for obj in model.dbObjects do begin
    if obj.ClassType = TDBTable then begin
      tab:= TDBTable(obj);
      sqlOut.Add('--------------- Table "' + obj.name + '"----------------------');

      //Add domains for columns ENUM
      nEnum := 0;
      for col in tab.columns do begin
        if UpCase(col.datatype)= 'ENUM' then begin
          inc(nEnum);
          //Explore the item list to get the size and type
          enumType := TypeOfItem(col.enumItems[0]);
          if enumType = 'VARCHAR' then begin
            //Is VARCHAR. We need the max size.
            maxSize := 0;
            for i:=0 to high(col.enumItems) do begin
              if length(col.enumItems[i]) > maxSize then maxSize := length(col.enumItems[i]);
            end;
            enumType := enumType + '(' + IntToStr(maxSize) + ')';
          end;
          //There is not ENUM in Firebird. We need to create domains.
          domainName := 'en' + UpCase(tab.name) + IntToStr(nEnum);
          sqlOut.Add('CREATE DOMAIN '+ domainName + ' AS ' + enumType);
          sqlOut.Add('   CHECK ( value IS NULL or VALUE IN ' + col.dataTipeAd + ');');
          //Replace the Data  type
          col.datatype := domainName;
          col.dataTipeAd := '';
        end;
      end;

      //Table creation sentence
      sqlOut.Add('CREATE TABLE ' + obj.name + '(');
      //Write first column definition. It must be at least one column,
      col := tab.columns[0];
      sqlOut.Add('  ' + ColumnDefinition(col));
      //Write other columns definition
      for i:=1 to tab.columns.Count-1 do begin
        col := tab.columns[i];
        AddCommaToList(sqlOut);
        sqlOut.Add('  ' + ColumnDefinition(col));
      end;
      //Write PK information
      if tab.primary.nCols>0 then begin
        AddCommaToList(sqlOut);
        sqlOut.Add('  PRIMARY KEY (' + tab.primary.columnsStr + ')');
      end;
      //Write constraint information
      for con in tab.constraints do begin
        AddCommaToList(sqlOut);
        sqlOut.Add('  CONSTRAINT ' + con.name +' ');
        if con.constType = ctypFK then begin
           sqlOut.Add('    FOREIGN KEY(' + con.localColumnsStr + ') ' +
           'REFERENCES ' + con.foreignTable.name + '(' + con.foreignColumnsStr + ')');
        end else begin
          //Not implemented for other constraint type
        end;
      end;
      //Write las part of table definition
      sqlOut.Add(');');
      //Add coments for columns
      for col in tab.columns do begin
        comment := col.comment;
        if comment<>'' then begin
          comment := StringReplace(comment, '\''', '''''', [rfReplaceAll]); // \' -> ''
          //Line ending \n cannot translate to Firebird?
          sqlOut.Add('COMMENT ON COLUMN ' + tab.name + '.' + col.name +
                  ' IS ' + comment + ';');
        end;
      end;
      sqlOut.Add('');
    end;
  end;
  sqlOut.Add('COMMIT;');  //We need to commit table creation before DML's

  //Additional Script
  for str in model.scripts do begin
    tmp := StringReplace(str, '`', '', [rfReplaceAll]);
    sqlOut.Add(tmp);
  end;
  //Final commit
  sqlOut.Add('COMMIT;');

  sqlOut.EndUpdate;
end;

constructor TFirebirdScript.Create(model0: TDBModelBase);
begin
  model := model0;
end;

end.

