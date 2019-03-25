unit Boss.Commands;

interface

uses
  Vcl.StdCtrls;

procedure Runner(ACommand, APath: string; ALoadPackages: Boolean);

procedure RunBossInstall(APath: string; ALoadPackages: Boolean);

implementation

uses
  Winapi.Windows, Vcl.Forms, System.SysUtils, System.Classes, System.Threading, Providers.Message, DosCommand, Boss.Modules.PackageProcessor;

procedure RunBossInstall(APath: string; ALoadPackages: Boolean);
begin
  Runner('boss install', APath, ALoadPackages);
end;


procedure Runner(ACommand, APath: string; ALoadPackages: Boolean);
var
  LDosCommand: TDosCommand;
begin
  LDosCommand := TDosCommand.Create(nil);
  try
    LDosCommand.OnNewLine :=
      procedure(ASender: TObject; const ANewLine: string; AOutputType: TOutputType)
      begin
        TProviderMessage.GetInstance.WriteLn(ANewLine);
      end;

    LDosCommand.OnTerminateProcess :=
      procedure(ASender: TObject; var ACanTerminate: Boolean)
      begin
        ACanTerminate := True;
        LDosCommand.Free;
        if ALoadPackages then
        begin          
          TBossPackageProcessor
            .GetInstance
            .LoadBpls(APath);
        end;
      end;

    LDosCommand.InputToOutput := False;
    LDosCommand.CurrentDir := APath;
    LDosCommand.CommandLine := GetEnvironmentVariable('COMSPEC');
    LDosCommand.Execute;
    LDosCommand.SendLine(ACommand, False);
    LDosCommand.SendLine(#13, False);
    LDosCommand.SendLine('', True);
  finally

  end;

end;


end.
