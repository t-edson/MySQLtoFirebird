unit FrmPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit , SynFacilBasic, SynFacilHighlighter, DBModelMySQL, DBModel;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConvert: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    edMySQL: TSynEdit;
    edFirebird: TSynEdit;
    procedure btnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

procedure TForm1.btnConvertClick(Sender: TObject);
begin
  model.ReadScriptFromMySQL(edMySQL.lines);
  model.WriteScriptFirebird(edFirebird.Lines);
end;


end.

