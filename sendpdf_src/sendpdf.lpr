program sendpdf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, dm
  { you can add units after this };

type

  { TSendPDF }

  TSendPDF = class(TCustomApplication)
  private
    procedure Process;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSendPDF }

procedure TSendPDF.Process;
begin
  DataModule1.ProcessDocuments;
end;

procedure TSendPDF.DoRun;
var
  ErrorMsg: String;
begin
  { add your program here }
  Process;
  // stop program loop
  Terminate;
end;

constructor TSendPDF.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  DataModule1 := TDataModule1.Create(nil);
end;

destructor TSendPDF.Destroy;
begin
  DataModule1.Free;
  inherited Destroy;
end;

var
  Application: TSendPDF;
begin
  Application:=TSendPDF.Create(nil);
  Application.Title:='SendPDF';
  Application.Run;
  Application.Free;
end.

