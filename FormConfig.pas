{Modelo de formulario de configuración que usa solo un Frame de configuración}
unit FormConfig;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, MisUtils,
  MiConfigXML;  //Change to MiConfigINI, to use INI file instead.
type
  { TConfig }
  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    Label3: TLabel;
    txtIsqlpath: TEdit;
    Label2: TLabel;
    txtDbFilePath: TEdit;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    msjError  : string;    //Error string
    //vars to manage
    isqlpath  : string;
    dbFilePath: string;
    sqltxt    : string;
    procedure Initiate;
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

{ TConfig }
procedure TConfig.FormCreate(Sender: TObject);
begin
  cfgFile.VerifyFile;
end;

procedure TConfig.Initiate;
begin
  //asociate vars to controls
  cfgFile.Asoc_Str('isql_path' , @isqlpath  , txtIsqlpath, 'C:\Program Files\Firebird\Firebird_3_0\isql.exe');
  cfgFile.Asoc_Str('dbFilePath', @dbFilePath, txtDbFilePath, 'C:\');
  cfgFile.Asoc_Str('sqltxt'    , @sqltxt    ,
  'CREATE TABLE IF NOT EXISTS `mydb`.`perfiles` ('+ LineEnding +
  '`idPerfil` VARCHAR(12) NOT NULL,'+ LineEnding +
  '`nombre` VARCHAR(16) NOT NULL,'+ LineEnding +
  '`descripcion` VARCHAR(45) NULL,'+ LineEnding +
  'PRIMARY KEY (`idPerfil`))' + LineEnding +
  'ENGINE = InnoDB;');
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  cfgFile.PropertiesToWindow;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  self.Close;
end;

procedure TConfig.SaveToFile;
begin
  if not cfgFile.PropertiesToFile then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;

end.

