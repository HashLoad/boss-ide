unit Boss.Modules.PackageProcessor;

interface

uses System.IniFiles, System.Classes, System.Types;

type
  TBossPackageProcessor = class
  private
    FDataFile: TStringList;
    FHomeDrive: string;
    FHomePath: string;

    function GetBplList: TStringDynArray;
    function GetBinList(ARootPath: string): TStringDynArray;

    function GetEnv(AEnv: string): string;
    function GetDataCachePath: string;

    procedure SaveData;

    procedure LoadTools(AProjectPath: string);
    procedure MakeLink(AProjectPath, AEnv: string);
    procedure DoLoadBpls(ABpls: TStringDynArray);

    constructor Create;
  public
    procedure LoadBpls;
    procedure UnloadOlds;

    class procedure OnActiveProjectChanged(AProject: string);
    class function GetInstance: TBossPackageProcessor;
  end;

const
  BPLS = 'BPLS';
  DELIMITER = ';';

implementation

uses System.IOUtils, Providers.Consts, Boss.IDE.Installer, Providers.Message, Vcl.Dialogs, ToolsAPI, Boss.IDE.OpenToolApi.Tools,
  Winapi.ShellAPI, Winapi.Windows, Vcl.Menus, Boss.EventWrapper, Vcl.Forms, System.SysUtils;

var
  _Instance: TBossPackageProcessor;

procedure ExecuteAndWait(const ACommand: string);
var
  LStartup: TStartupInfo;
  LProcess: TProcessInformation;
  LProgram: string;
begin
  LProgram := Trim(ACommand);
  FillChar(LStartup, SizeOf(LStartup), 0);
  LStartup.cb := SizeOf(TStartupInfo);
  LStartup.wShowWindow := SW_HIDE;

  if CreateProcess(nil, pchar(LProgram), nil, nil, true, CREATE_NO_WINDOW, nil, nil, LStartup, LProcess) then
  begin
    while WaitForSingleObject(LProcess.hProcess, 10) > 0 do
    begin
      Application.ProcessMessages;
    end;
    CloseHandle(LProcess.hProcess);
    CloseHandle(LProcess.hThread);
  end
  else
    RaiseLastOSError;
end;

function TBossPackageProcessor.GetEnv(AEnv: string): string;
begin
  Result := FHomeDrive + FHomePath + TPath.DirectorySeparatorChar + C_BOSS_CACHE_FOLDER + TPath.DirectorySeparatorChar + C_ENV + AEnv;
end;

procedure DeleteDirectory(const DirName: string);
var
  FileOp: TSHFileOpStruct;
begin
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := pchar(DirName + #0);
  FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
  SHFileOperation(FileOp);
end;

procedure TBossPackageProcessor.MakeLink(AProjectPath, AEnv: string);
var
  LFile: string;
begin
  try
    if DirectoryExists(GetEnv(AEnv)) then
      DeleteDirectory(GetEnv(AEnv));

    ForceDirectories(GetEnv(AEnv));

    for LFile in TDirectory.GetFiles(AProjectPath + TPath.DirectorySeparatorChar + C_MODULES_FOLDER + '.' + AEnv) do
    begin
      TFile.Copy(LFile, TPath.Combine(GetEnv(AEnv), TPath.GetFileName(LFile)), true);
    end;
  except
    on E: Exception do
      TProviderMessage.GetInstance.WriteLn('Failed on make link: ' + E.Message);
  end;
end;

constructor TBossPackageProcessor.Create;
begin
  FDataFile := TStringList.Create;
  FHomeDrive := GetEnvironmentVariable('HOMEDRIVE');
  FHomePath := GetEnvironmentVariable('HOMEPATH');

  if FileExists(GetDataCachePath) then
    FDataFile.LoadFromFile(GetDataCachePath);

  if not FDataFile.Values[BPLS].IsEmpty then
  begin
    FDataFile.DELIMITER := ';';
    FDataFile.DelimitedText := FDataFile.Values[BPLS];
  end;

  if DirectoryExists(GetEnv(EmptyStr)) then
    ForceDirectories(GetEnv(EmptyStr));

  UnloadOlds;
end;

function TBossPackageProcessor.GetBinList(ARootPath: string): TStringDynArray;
begin
  if not DirectoryExists(ARootPath + C_BIN_FOLDER) then
    Exit();

  Result := TDirectory.GetFiles(ARootPath + C_BIN_FOLDER, '*.exe')
end;

function TBossPackageProcessor.GetBplList: TStringDynArray;
var
  LOrderFileName: string;
  LOrder: TStringList;
  LIndex: Integer;
  I: Integer;
begin
  Result := nil;
  if not DirectoryExists(GetEnv(C_ENV_BPL)) then
    Exit();

  LOrderFileName := GetEnv(C_ENV_BPL) + TPath.DirectorySeparatorChar + C_BPL_ORDER;
  if FileExists(LOrderFileName) then
  begin
    LOrder := TStringList.Create;
    try
      LOrder.LoadFromFile(LOrderFileName);
      for LIndex := 0 to LOrder.Count - 1 do
        LOrder.Strings[LIndex] := GetEnv(C_ENV_BPL) + TPath.DirectorySeparatorChar + LOrder.Strings[LIndex];

      SetLength(Result, LOrder.Count);
      for I := 0 to LOrder.Count - 1 do
        Result[I] := LOrder[I];
    finally
      LOrder.Free;
    end;
  end
  else
    Result := TDirectory.GetFiles(GetEnv(C_ENV_BPL), '*.bpl')
end;

function TBossPackageProcessor.GetDataCachePath: string;
begin
  Result := FHomeDrive + FHomePath + TPath.DirectorySeparatorChar + C_BOSS_CACHE_FOLDER + TPath.DirectorySeparatorChar + C_DATA_FILE;
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
  LBpls := GetBplList;
  DoLoadBpls(LBpls);
end;

procedure TBossPackageProcessor.DoLoadBpls(ABpls: TStringDynArray);
var
  LBpl: string;
  ALength, LFlag: Integer;
  LHnd: NativeUInt;
  LBplsRedo: TStringDynArray;
  LInstalledNew: Boolean;
begin
  LInstalledNew := False;
  ALength := 0;
  SetLength(LBplsRedo, ALength);

  for LBpl in ABpls do
  begin
    try
      LHnd := LoadPackage(LBpl);
      GetPackageInfo(LHnd, nil, LFlag, PackageInfoProc);
      UnloadPackage(LHnd);
    except
      on E: Exception do
      begin
        TProviderMessage.GetInstance.WriteLn('Failed to get info of ' + LBpl);
        TProviderMessage.GetInstance.WriteLn(#10 + E.Message);
        Inc(ALength);
        SetLength(LBplsRedo, ALength);
        LBplsRedo[ALength - 1] := LBpl;
        Continue;
      end;
    end;

    if not(LFlag and pfRunOnly = pfRunOnly) then
    begin
      if TBossIDEInstaller.InstallBpl(LBpl) then
      begin
        TProviderMessage.GetInstance.WriteLn('Instaled: ' + LBpl);
        FDataFile.Add(LBpl);
        LInstalledNew := true;
      end
      else
      begin
        Inc(ALength);
        SetLength(LBplsRedo, ALength);
        LBplsRedo[ALength - 1] := LBpl;
      end;

    end;
  end;

  SaveData;

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
  GetInstance.MakeLink(ExtractFilePath(AProject), C_ENV_BPL);
  GetInstance.MakeLink(ExtractFilePath(AProject), C_ENV_DCU);
  GetInstance.MakeLink(ExtractFilePath(AProject), C_ENV_DCP);
  GetInstance.LoadBpls;
  GetInstance.LoadTools(ExtractFilePath(AProject) + C_MODULES_FOLDER);
end;

procedure TBossPackageProcessor.SaveData;
begin
  FDataFile.SaveToFile(GetDataCachePath);
end;

procedure TBossPackageProcessor.UnloadOlds;
var
  LBpl: string;
  LMenu: TMenuItem;
  LMenuItem: TMenuItem;
  LIndex: Integer;
begin
  for LBpl in FDataFile do
  begin
    TBossIDEInstaller.RemoveBpl(LBpl);
    TProviderMessage.GetInstance.WriteLn('Removed: ' + LBpl);
    Application.ProcessMessages;
  end;

  FDataFile.Clear;
  SaveData;

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
