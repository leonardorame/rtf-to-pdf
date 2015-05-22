program replacepict;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TRTFPictReplacer }

  TRTFPictReplacer = class(TCustomApplication)
  private
    procedure ReplacePicture(AContent: string);
    procedure Process(AContent: string; AStart: Integer = 0);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TRTFPictReplacer }

procedure TRTFPictReplacer.ReplacePicture(AContent: string);
var
  lHexPict: string;
  lFrom: Integer;
  lTo: Integer;
  lStream: TMemoryStream;
  lHexValue: string;
  lBinValue: array[0..SizeOf(Extended) * 2] of Char;
  lBinBufSize: Integer;
  lStoredBytes: Integer;
  I: Integer;
begin
  lStream := TMemoryStream.Create;
  try
    lFrom := Pos(' ', AContent) + 1;
    lTo := Pos('}', AContent);
    lHexPict := Copy(AContent, lFrom, lTo - lFrom);
    lStream.Size := Length(lHexPict) div 2;
    HexToBin(PAnsiChar(lHexPict), lStream.Memory, lStream.Size);
    lStream.SaveToFile('salida.bmp');
    halt;

  finally
    lStream.Free;
  end;
end;

procedure TRTFPictReplacer.Process(AContent: string; AStart: Integer = 0);
var
  lStart: Integer;
  lEnd: Integer;
  lTail: string;
begin
  lStart := Pos('{\pict', Copy(AContent, AStart, Length(AContent)));
  if lStart = 0 then
    exit
  else
  begin
    lStart := lStart + AStart;
    lEnd := Pos('}', Copy(AContent, lStart, Length(AContent)));
    ReplacePicture(Copy(AContent, lStart, lEnd));
    lStart := lStart + lEnd;
    lTail := Copy(AContent, lStart, Length(AContent));
    WriteLn(lTail);
    Process(AContent, lStart);
  end;
end;

procedure TRTFPictReplacer.DoRun;
var
  lFile: text;
  lData: string;
  lContent: string;
  ErrorMsg: String;
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

  Process(lContent);

  // stop program loop
  Terminate;
end;

constructor TRTFPictReplacer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRTFPictReplacer.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TRTFPictReplacer;
begin
  Application:=TRTFPictReplacer.Create(nil);
  Application.Title:='RTF Picture Replacer';
  Application.Run;
  Application.Free;
end.

