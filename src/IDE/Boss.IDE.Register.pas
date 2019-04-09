unit Boss.IDE.Register;

interface

procedure Register;

implementation

uses
  ToolsAPI, Winapi.Windows, Boss.IDE.BossInstall, Boss.IDE.PojectListener, Boss.Modules.PackageProcessor,
  Vcl.Graphics, DesignIntf, System.Types, Providers.Logo, Providers.Message;

const
  C_INVALID_NOTIFIER = -1;

var
  FNotifierMenuIndex, FNotifierProjectIndex: Integer;

procedure Register;
var
  LProjectManager: IOTAProjectManager;
  LServices: IOTAServices;
  LMessageServices: IOTAMessageServices;
  LDMLogo: TDataModuleLogo;
  LLogo: TBitmap;
begin
  ForceDemandLoadState(dlDisable);
  LDMLogo := TDataModuleLogo.Create(nil);
  try
    LLogo := TBitmap.Create;
    try
      LDMLogo.ImageList.GetBitmap(0, LLogo);
      SplashScreenServices.AddPluginBitmap('Boss', LLogo.Handle);
    finally
      LLogo.Free;
    end;
  finally
    LDMLogo.Free;
  end;

  LProjectManager := (BorlandIDEServices as IOTAProjectManager);
  LServices := (BorlandIDEServices as IOTAServices);
  LMessageServices := (BorlandIDEServices as IOTAMessageServices);

  FNotifierMenuIndex := LProjectManager.AddMenuItemCreatorNotifier(TMenuNotifierBossInstall.Create);

  FNotifierProjectIndex := LServices.AddNotifier(TBossProjectListener.GetInstance);

  TProviderMessage.GetInstance.Initialize(LMessageServices);

  TBossProjectListener.GetInstance.AddListener(ofnActiveProjectChanged, TBossPackageProcessor.OnActiveProjectChanged);
end;

initialization

finalization

if FNotifierProjectIndex > C_INVALID_NOTIFIER then
begin
  (BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierProjectIndex);
  FNotifierProjectIndex := C_INVALID_NOTIFIER;
end;

if FNotifierMenuIndex > C_INVALID_NOTIFIER then
begin
  (BorlandIDEServices as IOTAProjectManager).RemoveMenuItemCreatorNotifier(FNotifierMenuIndex);
  FNotifierMenuIndex := C_INVALID_NOTIFIER;
end;

end.
