unit Boss.Commands;

interface

uses Vcl.StdCtrls;

procedure Runner(ACommand, APath: string; ALoadPackages: Boolean);
procedure RunBossInstall(APath: string; ALoadPackages: Boolean);

implementation

uses Winapi.Windows, Vcl.Forms, System.SysUtils, System.Classes, System.Threading, Providers.Message,
  Boss.Modules.PackageProcessor, DosCommand;

procedure RunBossInstall(APath: string; ALoadPackages: Boolean);
begin
  Runner('boss install', APath, ALoadPackages);
end;

procedure Runner(ACommand, APath: string; ALoadPackages: Boolean);
var
  LDosCommand: TDosCommand;
begin
  LDosCommand := TDosCommand.Create(nil);
  LDosCommand.OnNewLine :=
    procedure(ASender: TObject; const ANewLine: string; AOutputType: TOutputType)
    begin
      TProviderMessage.GetInstance.WriteLn(ANewLine);
    end;

  LDosCommand.OnTerminated :=
    procedure(ASender: TObject)
    begin
      LDosCommand.Free;
      if ALoadPackages then
      begin
        TBossPackageProcessor.GetInstance.LoadBpls;
      end;
    end;

  LDosCommand.InputToOutput := False;
  LDosCommand.CurrentDir := APath;
  LDosCommand.CommandLine := ACommand;
  LDosCommand.Execute;
end;

end.
