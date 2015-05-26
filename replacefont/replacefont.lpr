program replacefont;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TRTFFontReplacer }

  TRTFFontReplacer = class(TCustomApplication)
  private
    function GetValue(AContent, AName: string): Integer;
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
    lEnd := Pos('\', Copy(AContent, lStart, Length(AContent)));
    if lEnd = 0 then
      lEnd := Pos(' ', Copy(AContent, lStart, Length(AContent)));
    lTmpStr := Copy(AContent, lStart, lEnd - 1);
    lValStr := Copy(lTmpStr, Length(AName) + 1, Length(lTmpStr));
    Result := StrToInt(lValStr);
  end;
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
begin
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

  if ParamCount = 0 then
  begin
    Writeln('Falta parámetro FONT. Ej.: ./replacefont "Times New Roman"');
    exit;
  end;
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

