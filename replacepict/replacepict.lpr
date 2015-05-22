program replacepict;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Process
  { you can add units after this };

type

  { TRTFPictReplacer }

  TRTFPictReplacer = class(TCustomApplication)
  private
    function GetValue(AContent, AName: string): Integer;
    procedure Convert(AInStream, AOutStream: TMemoryStream; AScaleX, AScaleY: Integer);
    procedure ReplacePicture(AContent: string);
    procedure Process(AContent: string; AStart: Integer = 0);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TRTFPictReplacer }

function TRTFPictReplacer.GetValue(AContent, AName: string): Integer;
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

procedure TRTFPictReplacer.Convert(AInStream, AOutStream: TMemoryStream;
  AScaleX, AScaleY: Integer);
var
  lProcess: TProcess;
  lNumBytes: LongInt;
  lBytesRead: Integer;
  lResize: string;

const
  READ_BYTES = 2048;
begin
  lProcess := TProcess.Create(nil);
  try
    lBytesRead:= 0;
    lProcess.Executable:= '/usr/bin/convert';
    lProcess.Options:= [poUsePipes];
    lProcess.Parameters.Add('-resize');
    lResize := Format('%d%%x%d%%', [AScaleX, AScaleY]);
    lProcess.Parameters.Add(lResize); // size
    lProcess.Parameters.Add('-'); // input stdin
    lProcess.Parameters.Add('jpeg:-'); // output stdout
    lProcess.Execute;
    AInStream.Position:= 0;
    lProcess.Input.Write(AInStream.Memory^, AInStream.Size);
    lProcess.CloseInput;
    while lProcess.Running do
    begin
      // make sure we have room
      AOutStream.SetSize(lBytesRead + READ_BYTES);
      // try reading it
      lNumBytes := lProcess.Output.Read((AOutStream.Memory + lBytesRead)^, READ_BYTES);
      if lNumBytes > 0 then// All read() calls will block, except the final one.
        Inc(lBytesRead, lNumBytes)
      else
        Break; // Program has finished execution.
    end;
    AOutStream.SetSize(lBytesRead);
    AOutStream.Position:=0;
  finally
    lProcess.Free;
  end;
end;

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
  lPicScaleX: Integer;
  lPicScaleY: Integer;
  lOut: TMemoryStream;
begin
  // get the x and y scale
  lPicScaleX := GetValue(AContent, 'picscalex');
  lPicScaleY := GetValue(AContent, 'picscaley');

  // extract the image stream
  lStream := TMemoryStream.Create;
  lOut := TMemoryStream.Create;
  try
    lFrom := Pos(' ', AContent) + 1;
    lTo := Pos('}', AContent);
    lHexPict := Copy(AContent, lFrom, lTo - lFrom);
    lStream.Size := Length(lHexPict) div 2;
    HexToBin(PAnsiChar(lHexPict), lStream.Memory, lStream.Size);
    // convert the image stream
    Convert(lStream, lOut, lPicScaleX, lPicScaleY);
    lOut.SaveToFile('salida.jpg');
    //
  finally
    lOut.Free;
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
  {AssignFile(lFile, '');
  reset(lFile);
  lContent := '';
  while not eof(lFile) do
  begin
    readln(lFile, lData);
    lContent := lContent + lData;
  end;
  close(lFile);}
  with TStringList.Create do
  begin
    LoadFromFile('../soffice/test.rtf');
    lContent := Text;
    Free;
  end;

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

