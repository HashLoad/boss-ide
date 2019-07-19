unit Boss.Modules.PackageProcessor;

interface

uses
  System.IniFiles, System.Classes, System.SysUtils, System.Types;

type
  TBossPackageProcessor = class
  private
    function GetBplList(ARootPath: string): TStringDynArray;
    function GetBinList(ARootPath: string): TStringDynArray;

    function GetEnv: string;

    procedure LoadTools(AProjectPath: string);
    procedure MakeLink(AProjectPath: string);
    procedure DoLoadBpls(ABpls: TArray<string>);

    constructor Create;
  public
    procedure LoadBpls;
    procedure UnloadOlds;

    class Procedure OnActiveProjectChanged(AProject: string);
    class function GetInstance: TBossPackageProcessor;
  end;


const
  BPLS = 'BPLS';
  DELIMITER = ';';

implementation

uses
  System.IOUtils, Providers.Consts, Boss.IDE.Installer, Providers.Message, Vcl.Dialogs, ToolsAPI,
  Boss.IDE.OpenToolApi.Tools, Winapi.ShellAPI, Winapi.Windows, Vcl.Menus, Boss.EventWrapper, Vcl.Forms;

{ TBossPackageProcessor }

const
  SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = $2 platform;

var
  _Instance: TBossPackageProcessor;

procedure ExecuteAndWait(const aCommando: string);
var
  tmpStartupInfo: TStartupInfo;
  tmpProcessInformation: TProcessInformation;
  tmpProgram: String;
begin
  tmpProgram := trim(aCommando);
  FillChar(tmpStartupInfo, SizeOf(tmpStartupInfo), 0);
  with tmpStartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    wShowWindow := SW_HIDE;
  end;

  if CreateProcess(nil, pchar(tmpProgram), nil, nil, true, CREATE_NO_WINDOW,
    nil, nil, tmpStartupInfo, tmpProcessInformation) then
  begin
    while WaitForSingleObject(tmpProcessInformation.hProcess, 10) > 0 do
    begin
      Application.ProcessMessages;
    end;
    CloseHandle(tmpProcessInformation.hProcess);
    CloseHandle(tmpProcessInformation.hThread);
  end
  else
  begin
    RaiseLastOSError;
  end;
end;

function TBossPackageProcessor.GetEnv: string;
begin
  Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') + TPath.DirectorySeparatorChar +
    C_BOSS_CACHE_FOLDER + TPath.DirectorySeparatorChar + C_ENV;
end;


procedure TBossPackageProcessor.MakeLink(AProjectPath: string);
var
  LCommand: PChar;
begin
  RemoveDir(Pchar(GetEnv));
  LCommand := PChar(Format('cmd /c Mklink /D /J "%s" "%s"',
    [GetEnv, AProjectPath + TPath.DirectorySeparatorChar + C_MODULES_FOLDER + C_BPL_FOLDER]));
  ExecuteAndWait(LCommand);
end;

constructor TBossPackageProcessor.Create;
begin
  UnloadOlds;
end;

function TBossPackageProcessor.GetBinList(ARootPath: string): TStringDynArray;
begin
  if not DirectoryExists(ARootPath + C_BIN_FOLDER) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath + C_BIN_FOLDER, '*.exe')
end;

function TBossPackageProcessor.GetBplList(ARootPath: string): TStringDynArray;
begin
  if not DirectoryExists(ARootPath) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath, '*.bpl')
end;

class function TBossPackageProcessor.GetInstance: TBossPackageProcessor;
begin
  if not Assigned(_Instance) then
    _Instance := TBossPackageProcessor.Create;
  Result := _Instance;
end;

procedure PackageInfoProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
begin

end;

procedure TBossPackageProcessor.LoadBpls;
var
  LBpls: TStringDynArray;
begin
  LBpls := GetBplList(GetEnv);
  DoLoadBpls(LBpls);
end;

procedure TBossPackageProcessor.DoLoadBpls(ABpls: TArray<string>);
var
  LBpl: string;
  LFlag: Integer;
  LHnd: NativeUInt;
  LBplsRedo: TStringDynArray;
  LInstalledNew: Boolean;
begin
  LInstalledNew := False;
  LBplsRedo := [];

  for LBpl in ABpls do
  begin
    try
      LHnd := LoadPackage(LBpl);
      GetPackageInfo(LHnd, nil, LFlag, PackageInfoProc);
      UnloadPackage(LHnd);
    except
      TProviderMessage.GetInstance.WriteLn('Failed to get info of ' + LBpl);
      LBplsRedo := LBplsRedo + [LBpl];
      Continue;
    end;

    if not(LFlag and pfRunOnly = pfRunOnly) then
    begin
      if TBossIDEInstaller.InstallBpl(LBpl) then
      begin
        TProviderMessage.GetInstance.WriteLn('Instaled: ' + LBpl);
        LInstalledNew := True;
      end
      else
        LBplsRedo := LBplsRedo + [LBpl];
    end;
  end;

  if LInstalledNew then
  begin
    DoLoadBpls(LBplsRedo);
  end;
end;

procedure TBossPackageProcessor.LoadTools(AProjectPath: string);
var
  LBins: TStringDynArray;
  LBin, LBinName: string;
  LMenu: TMenuItem;
  LMenuItem: TMenuItem;
begin
  LMenu := NativeServices.MainMenu.Items.Find('Tools');
  LBins := GetBinList(AProjectPath);

  NativeServices.MenuBeginUpdate;
  try
    for LBin in LBins do
    begin
      LBinName := ExtractFileName(LBin);
      LMenuItem := TMenuItem.Create(NativeServices.MainMenu);
      LMenuItem.Caption := Providers.Consts.C_BOSS_TAG + ' ' + LBinName;
      LMenuItem.OnClick := GetOpenEvent(LBin);
      LMenuItem.Name := 'boss_' + LBinName.Replace('.', '_');
      LMenuItem.Hint := LBin;
      LMenu.Add(LMenuItem);
    end;
  finally
    NativeServices.MenuEndUpdate;
  end;
end;

class procedure TBossPackageProcessor.OnActiveProjectChanged(AProject: string);
begin
  TProviderMessage.GetInstance.Clear;
  TProviderMessage.GetInstance.WriteLn('Loading packages from project ' + AProject);

  GetInstance.UnloadOlds;
  GetInstance.MakeLink(ExtractFilePath(AProject));
  GetInstance.LoadBpls;
  GetInstance.LoadTools(ExtractFilePath(AProject) + C_MODULES_FOLDER);
end;

procedure TBossPackageProcessor.UnloadOlds;
var
  LBpl: string;
  LMenu: TMenuItem;
  LMenuItem: TMenuItem;
  LIndex: Integer;
begin
  for LBpl in GetBplList(GetEnv) do
  begin
    TBossIDEInstaller.RemoveBpl(LBpl);
    TProviderMessage.GetInstance.WriteLn('Removed: ' + LBpl);
  end;

  LMenu := NativeServices.MainMenu.Items.Find('Tools');

  NativeServices.MenuBeginUpdate;
  try
    for LIndex := 0 to LMenu.Count - 1 do
    begin
      LMenuItem := LMenu.Items[LIndex];
      if LMenuItem.Caption.StartsWith(C_BOSS_TAG) then
      begin
        LMenu.Remove(LMenuItem);
        LMenuItem.Free;
      end;
    end;
  finally
    NativeServices.MenuEndUpdate;
  end;
end;

initialization

finalization

_Instance.Free;

end.
