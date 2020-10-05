unit FrmPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, SynEdit, SynFacilBasic, SynFacilHighlighter, UnTerminal, DBModelMySQL,
  DBModel, DBModelBase, MisUtils, FormConfig;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConvert: TButton;
    btnPaste: TButton;
    btnRunScript: TButton;
    btnSettings: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    genCopy: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupFirebird: TPopupMenu;
    Splitter1: TSplitter;
    edMySQL: TSynEdit;
    edFirebird: TSynEdit;
    procedure btnConvertClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnRunScriptClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    model: TDBModel;
    hlt1: TSynFacilSyn;
    hlt2: TSynFacilSyn;
  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  //Carga resaltador
  hlt1 := TSynFacilSyn.Create(self);
  hlt1.LoadFromFile('./languages/SQLMySQL.xml');   //load the syntax file
  edMySQL.Highlighter := hlt1;

  //Carga resaltador
  hlt2 := TSynFacilSyn.Create(self);  //Usar el mismo resaltador crearía interferencia
  hlt2.LoadFromFile('./languages/SQLMySQL.xml');   //load the syntax file
  edFirebird.Highlighter := hlt2;

  //Crea modelo
  model := TDBModel.Create(hlt1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  model.Destroy;
  hlt2.Destroy;
  hlt1.Destroy;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Config.Initiate;
  edMySQL.Text := Config.sqltxt;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  nombTabla: String;
  tab: TDBTable;
begin
  if ListBox1.ItemIndex = -1 then exit;
  //Obtiene nombre ed tabla
  nombTabla := ListBox1.Items[ListBox1.ItemIndex];
  tab := model.GetTableByName(nombTabla);  //Busca en modelo
  //Selecciona el script fuente de la tabla
  edFirebird.CaretXY := Point(1, tab.scriptIni.y);
  edFirebird.BlockBegin := Point(1, tab.scriptIni.y);
  edFirebird.BlockEnd := Point(1, tab.scriptFin.y);
  edFirebird.CaretXY := edFirebird.BlockEnd;
  edFirebird.CopyToClipboard;
  //edFirebird.SetFocus;
end;
procedure TForm1.btnConvertClick(Sender: TObject);
var
  i: Integer;
  linea, nombTabla: String;
  tablaActual: TDBTable;
begin
  model.ReadScriptFromMySQL(edMySQL.lines);
  //Configura opciones
  model.FireBirdScript.dbFilePath := Config.dbFilePath;
  //Genera script
  model.WriteScriptFirebird(edFirebird.Lines);
  ListBox1.Clear;
  //Explora script para ubicar definiciones de tablas
  tablaActual := nil;
  for i:=0 to edFirebird.Lines.Count-1 do begin
    linea := edFirebird.Lines[i];
    if copy(linea,1,3) = '---' then begin
      //Encontró el inicio de una tabla
      if tablaActual<>nil then begin  //Cierra tabla actual
        tablaActual.scriptFin.y := i;
      end;
      nombTabla := copy(linea, 24, 100);
      nombTabla := copy(nombTabla, 1, pos('"', nombTabla)-1 );
      if nombTabla = '' then continue;  //No es tabla
      //Inicio de definición de tabla.
      tablaActual := model.GetTableByName(nombTabla);  //Busca en modelo
      tablaActual.scriptIni.y := i+1;  //COrrige referencia
      ListBox1.AddItem(nombTabla, tablaActual);
    end;
  end;
  //if tablaActual<>nil then begin  //Cierra tabla actual
  //  tablaActual.scriptFin.y := i-1;
  //end;
end;

procedure TForm1.btnPasteClick(Sender: TObject);
begin
  edMySQL.Text := '';
  edMySQL.PasteFromClipboard;
  btnConvertClick(nil);
end;
procedure TForm1.btnRunScriptClick(Sender: TObject);
var
  outStr: string;
  proc: TConsoleProc;
begin
  StringToFile(edFirebird.Text, 'script.sql');
  proc := TConsoleProc.Create(nil);
  proc.RunInLoop(Config.isqlpath, ' -i script.sql', -1, outStr);
  memo1.Text := outStr;
  proc.Destroy;
end;

procedure TForm1.btnSettingsClick(Sender: TObject);
begin
  Config.ShowModal;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.sqltxt := edMySQL.Text;  //Para salvar contenido
  Config.SaveToFile;  //guarda la configuración actual
end;

end.

