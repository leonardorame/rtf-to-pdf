program replacefont;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, regexpr
  { you can add units after this };

type

  { TRTFFontReplacer }

  TRTFFontReplacer = class(TCustomApplication)
  private
    function GetValue(AContent, AName: string): Integer;
    function ReplaceFont(ARegExpr: TRegExpr): string;
    function ReplaceLineSpacing(ARegExpr: TRegExpr): string;
    procedure Process(var AContent: string; AStart: Integer = 0);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TRTFFontReplacer }

function TRTFFontReplacer.GetValue(AContent, AName: string): Integer;
var
  lValStr: string;
  lTmpStr: string;
  lStart: Integer;
  lEnd: Integer;
begin
  Result := 0;
  lStart := Pos(AName, AContent);
  if lStart > 0 then
  begin
    lStart := lStart + Length(AName);
    lEnd := Pos(' ', Copy(AContent, lStart, Length(AContent)));
    lValStr := Copy(AContent, lStart, lEnd - 1);
    Result := StrToInt(lValStr);
  end;
end;

function TRTFFontReplacer.ReplaceFont(ARegExpr: TRegExpr): string;
begin
  Result := '\fs' + ParamStr(2) + ' ';
end;

function TRTFFontReplacer.ReplaceLineSpacing(ARegExpr: TRegExpr): string;
begin
  Result := '\sl' + ParamStr(3) + ' ';
end;

procedure TRTFFontReplacer.Process(var AContent: string; AStart: Integer = 0);
var
  lStart: Integer;
  lEnd: Integer;
  lTail: string;
  lNewStart: Integer;
  lNewFont: string;
  lBuffer: string;
begin
  lBuffer := Copy(AContent, AStart, Length(AContent));
  lStart := Pos('{\fonttbl', lBuffer);
  if lStart = 0 then
    exit
  else
  begin
    if AStart > 0 then
      AStart := AStart - 1;
    lStart := AStart + lStart + Length('{\fonttbl');
    // fonttbl contiene al menos un grupo entre { y }
    // si el próximo caracter es '{' es porque
    // comienza un grupo
    lBuffer := Copy(AContent, lStart, 100);
    while Copy(AContent, lStart, 1) = '{' do
    begin
      lStart := lStart + Pos('{', Copy(AContent, lStart, Length(AContent)));
      lEnd := Pos('}', Copy(AContent, lStart, Length(AContent)));
      lTail := Copy(AContent, lStart + lEnd, Length(AContent) - 1);

      lStart := lStart + Pos(' ', Copy(AContent, lStart, Length(AContent))) - 1;

      lNewFont := ParamStr(1) + ';';
      AContent := Copy(AContent, 0, lStart) + lNewFont + '}';

      lStart := Length(AContent) + 1;
      AContent := AContent + lTail;
    end;
    Process(AContent, lStart);
  end;
end;

procedure TRTFFontReplacer.DoRun;
var
  lFile: text;
  lData: string;
  lContent: string;
  lFontSize: Integer;
begin
  if ParamCount < 3 then
  begin
    Writeln('Falta parámetro FONT. Ej.: ./replacefont "Times New Roman" 22 1500');
    Writeln('Los parámetros son font size linespacing.');
    exit;
  end;

  // read from stdin
  AssignFile(lFile, '');
  reset(lFile);
  lContent := '';
  while not eof(lFile) do
  begin
    readln(lFile, lData);
    lContent := lContent + lData;
  end;
  close(lFile);

  {With TStringList.Create do
  begin
    LoadFromFile('test.rtf');
    lContent := Text;
    Free;
  end;}

  // replace font size
  with TRegExpr.Create do
  begin
    Expression := '\\fs[0-9]*\s';
    lContent := Replace(lContent, @ReplaceFont);
    Free;
  end;

  // replace line spacing
  with TRegExpr.Create do
  begin
    Expression := '\\sl[0-9]*\s';
    lContent := Replace(lContent, @ReplaceLineSpacing);
    Free;
  end;

  // replace plain with linespacing
  // muchas veces no hay \sl, por lo 
  // que el método ReplaceLineSpacing
  // no se ejecuta nunca.
  // En estos casos reemplazamos 
  // \plain por \plain\slN
  lContent := StringReplace(
    lContent,
    '\plain ',
    Format('\plain\sl%s ', [ParamStr(3)]),
    [rfReplaceAll]
    );

  // replace font
  Process(lContent);
  // output to stdout
  WriteLn(lContent);

  // stop program loop
  Terminate;
end;

constructor TRTFFontReplacer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRTFFontReplacer.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TRTFFontReplacer;
begin
  Application:=TRTFFontReplacer.Create(nil);
  Application.Title:='RTF Font Replacer';
  Application.Run;
  Application.Free;
end.

