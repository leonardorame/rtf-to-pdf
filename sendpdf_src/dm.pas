unit dm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pqconnection, sqldb, FileUtil,db,
  IniFiles, LazLogger, dateutils, process;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    PQConnection1: TPQConnection;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    function GetHtml(AStream: TStream): string;
    function GetHTMLFromStream(AStream: TStream): string;
    procedure ProcessDocument(ADocument: string);
  public
    procedure ProcessDocuments;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  lFile: string;
  lIni: TIniFile;
begin
  lFile := ExtractFilePath(ParamStr(0)) + 'sendpdf.ini';
  DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), 'Opening ' + lFile);
  lIni := TIniFile.Create(lFile);
  try
    DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), lFile + ' open successful.');
    PQConnection1.DatabaseName:= lIni.ReadString('default', 'db', 'defaultdb');
    PQConnection1.HostName:= lIni.ReadString('default', 'host', '127.0.0.1');
    PQConnection1.UserName:= lIni.ReadString('default', 'user', 'postgres');
    PQConnection1.Password:= lIni.ReadString('default', 'pass', 'postgres');
    PQConnection1.Params.Add('port=' + lIni.ReadString('default', 'port', '5432'));
    PQConnection1.Connected:= True;
    DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), 'Connected to database.');
  finally
    lIni.Free;
  end;
end;

function TDataModule1.GetHtml(AStream: TStream): string;
var
  lBlobStream: TMemoryStream;
begin
  lBlobStream := TMemoryStream(AStream);
  try
    DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), '[Document] ', Format('Blob stream loaded, size: %d', [lBlobStream.Size]));
    AStream.Position:= 0;
    Result := GetHTMLFromStream(AStream);
  finally
    lBlobStream.Free;
  end;
end;

function TDataModule1.GetHTMLFromStream(AStream: TStream): string;
var
  lOut: TStringStream;
  lErr: TStringStream;
  lProcess: TProcess;
  lBuf: array[0..511] of byte;
  lReadCount: Integer;
begin
  lOut := TStringStream.Create('');
  lErr := TStringStream.Create('');
  lProcess := TProcess.Create(nil);
  try
    lProcess.Executable := '/usr/bin/unrtf';
    lProcess.Options := [poUsePipes];
    lProcess.Execute;

    while lProcess.Running or (lProcess.Output.NumBytesAvailable > 0) do
    begin
      // now write data to be encoded.
      lReadCount := AStream.Read(lBuf, SizeOf(lBuf));
      if lReadCount = 0 then
        lProcess.CloseInput
      else
        lProcess.Input.Write(lBuf, lReadCount);

      // stdout
      while lProcess.Output.NumBytesAvailable > 0 do
      begin
        lReadCount := lProcess.Output.Read(lBuf, SizeOf(lBuf));
        if lReadCount > 0 then
          lOut.Write(lBuf, lReadCount);
      end;

      // stderr
      while lProcess.StdErr.NumBytesAvailable > 0 do
      begin
        lReadCount := lProcess.StdErr.Read(lBuf, SizeOf(lBuf));
        if lReadCount > 0 then
        begin
          lErr.Write(lBuf, lReadCount);
        end;
      end;
    end;

    DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), '[Document] ', lErr.DataString);
    Result := lOut.DataString;
  finally
    lProcess.Free;
    lOut.Free;
    lErr.Free;
  end;
end;

procedure TDataModule1.ProcessDocument(ADocument: string);
var
  lSql: TSQLQuery;
  lHeaderHtml: string;
  lBodyHtml: string;
  lFooterHtml: string;
begin
  DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), '[Document] ', 'Accession: ' + ADocument);
  lSql := TSQLQuery.Create(nil);
  try
    lSql.DataBase := PQConnection1;
    lSql.SQL.Text:= 'select d.nombredocumento, d.cabeceradocumento, d.cuerpodocumento, d.piedocumento ' +
      'from turnodocumento d ' +
      'join turno t on d.idturno = t.idturno ' +
      'where t.turnosistemaexterno = :accession';
    lSql.ParamByName('accession').AsString := ADocument;
    lSql.Open;
    while not lSql.EOF do
    begin
      DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), '[Document] ', 'Nombre: ' + lSql.FieldByName('nombredocumento').AsString);
      lHeaderHtml := GetHtml(lSql.CreateBlobStream(lSql.FieldByName('cabeceradocumento'), bmRead));
      lBodyHtml := GetHtml(lSql.CreateBlobStream(lSql.FieldByName('cuerpodocumento'), bmRead));
      lFooterHtml := GetHtml(lSql.CreateBlobStream(lSql.FieldByName('piedocumento'), bmRead));
      with TStringList.Create do
      begin
        Add(lHeaderHtml);
        Add(lBodyHtml);
        Add(lFooterHtml);
        SaveToFile(ADocument + '.html');
        Free;
      end;
      lSql.Next;
    end;
  finally
    lSql.Free;
  end;
end;

procedure TDataModule1.ProcessDocuments;
var
  lAccession: string;
  lSql: TSQLQuery;
begin
  DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), 'Processing documents.');
  lSql := TSQLQuery.Create(nil);
  try
    lSql.DataBase := PQConnection1;
    lSql.SQL.Text:= 'select id, accession from rtftopdf where sent=false';
    lSql.Open;
    while not lSql.EOF do
    begin
      lAccession := lSql.FieldByName('accession').AsString;
      ProcessDocument(lAccession);
      lSql.Next;
    end;
    DebugLn(FormatDateTime('YYYY-MM-DD HH:NN:SS ', now), 'Done.');
  finally
    lSql.Free;
  end;
end;

end.

