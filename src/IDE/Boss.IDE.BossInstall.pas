unit Boss.IDE.BossInstall;

interface

uses ToolsAPI, Vcl.ActnList, System.Classes;

type
  TMenuNotifierBossInstall = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  public
    function CanHandle(const Ident: string): Boolean;
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  TButtonBoss = class(TNotifierObject, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FHelpContext: Integer;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
    FOnExecute: TNotifyEvent;
    FProject: IOTAProject;
  public
    { IOTALocalMenu }
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);
    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property HelpContext: Integer read GetHelpContext write SetHelpContext;
    property Name: string read GetName write SetName;
    property Parent: string read GetParent write SetParent;
    property Position: Integer read GetPosition write SetPosition;
    property Verb: string read GetVerb write SetVerb;
    property OnExute: TNotifyEvent read FOnExecute write FOnExecute;

    { IOTAProjectManagerMenu }
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
    property IsMultiSelectable: Boolean read GetIsMultiSelectable write SetIsMultiSelectable;

    constructor Create(aProject: IOTAProject); virtual;
  end;

  TButtonBossSeparator = class(TButtonBoss)
  public
    constructor Create(aProject: IOTAProject); override;
  end;

  TButtonBossInstall = class(TButtonBoss)
  public
    constructor Create(aProject: IOTAProject); override;
  end;

implementation

uses System.SysUtils, Boss.Modules.PackageProcessor, Boss.Commands, Boss.Ide.CnOTAUtils, Providers.Consts;

constructor TButtonBoss.Create(aProject: IOTAProject);
begin
  Self.Enabled := True;
  FProject := aProject;
end;

procedure TButtonBoss.Execute(const MenuContextList: IInterfaceList);
begin
  if CnOtaGetCurrentProject = FProject then
    TBossPackageProcessor.GetInstance.UnloadOlds;
  RunBossInstall(ExtractFilePath(FProject.FileName), CnOtaGetCurrentProject = FProject);
end;

function TButtonBoss.GetCaption: string;
begin
  Result := FCaption;
end;

function TButtonBoss.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TButtonBoss.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TButtonBoss.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TButtonBoss.GetIsMultiSelectable: Boolean;
begin
  Result := False;
end;

function TButtonBoss.GetName: string;
begin
  Result := FName;
end;

function TButtonBoss.GetParent: string;
begin
  Result := FParent;
end;

function TButtonBoss.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TButtonBoss.GetVerb: string;
begin
  Result := FVerb;
end;

function TButtonBoss.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function TButtonBoss.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

procedure TButtonBoss.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TButtonBoss.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TButtonBoss.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TButtonBoss.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TButtonBoss.SetIsMultiSelectable(Value: Boolean);
begin
  IsMultiSelectable := Value;
end;

procedure TButtonBoss.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TButtonBoss.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TButtonBoss.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TButtonBoss.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

{ TMenuNotifierBossInstall }

procedure TMenuNotifierBossInstall.AddMenu(const Project: IOTAProject; const IdentList: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
begin
  if CanHandle(Project.ApplicationType) and (not IsMultiSelect) and Assigned(Project) and
    (IdentList.IndexOf(sProjectContainer) <> -1) and Assigned(ProjectManagerMenuList) then
  begin
    ProjectManagerMenuList.Add(TButtonBossSeparator.Create(Project));
    ProjectManagerMenuList.Add(TButtonBossInstall.Create(Project));
  end;
end;

function TMenuNotifierBossInstall.CanHandle(const Ident: string): Boolean;
begin
  Result := (Ident = sApplication) or (Ident = sConsole) or (Ident = sPackage);
end;

{ TButtonBossSeparator }

constructor TButtonBossSeparator.Create(aProject: IOTAProject);
begin
  inherited Create(aProject);
  Self.Caption := '-';
  Self.Position := C_BUTTON_SEPARATOR_POSITION;
end;

{ TButtonBossInstall }

constructor TButtonBossInstall.Create(aProject: IOTAProject);
begin
  inherited Create(aProject);
  Self.Caption := 'Boss install';
  Self.Position := C_BUTTON_INSTALL_POSITION;
end;

end.
