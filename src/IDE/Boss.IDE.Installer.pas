unit Boss.IDE.Installer;

interface

uses
  ToolsApi;

type
  TBossIDEInstaller = class
  public
    class function InstallBpl(const AFile: string): Boolean;
    class function RemoveBpl(const AFile: string): Boolean;
  end;

implementation

uses
  System.Classes, Providers.Message, System.SysUtils;

{ TBossIDEInstaller }

class function TBossIDEInstaller.InstallBpl(const AFile: string): Boolean;
var
  LResult: Boolean;
begin
  try
    TThread.Synchronize(nil,
      procedure
      begin
        LResult := (BorlandIDEServices as IOTAPAckageServices).InstallPackage(AFile)
      end);
  except
    on E: Exception do
    begin
      TProviderMessage.GetInstance.WriteLn('Failed to install ' + AFile);
      TProviderMessage.GetInstance.WriteLn(#10 + E.Message);
      LResult := False;
    end;
  end;
  Result := LResult;
end;

class function TBossIDEInstaller.RemoveBpl(const AFile: string): Boolean;
var
  LResult: Boolean;
begin
  try
    TThread.Synchronize(nil,
      procedure
      begin
        LResult := (BorlandIDEServices as IOTAPAckageServices).UninstallPackage(AFile)
      end);
  except
    on E: Exception do
    begin
      TProviderMessage.GetInstance.WriteLn('Failed to remove ' + AFile);
      TProviderMessage.GetInstance.WriteLn(#10 + E.Message);
      LResult := False;
    end;
  end;
  Result := LResult;
end;

end.
