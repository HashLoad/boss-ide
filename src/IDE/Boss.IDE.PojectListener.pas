unit Boss.IDE.PojectListener;

interface

uses
  ToolsAPI, System.Generics.Collections, System.SysUtils;

type
  TBossProjectListener = class(TNotifierObject, IOTAIDENotifier)
  private
    FListeners: TObjectDictionary<TOTAFileNotification, TList<TProc<string>>>;

    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterSave;

    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeSave;

    procedure Destroyed;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure Modified;
    constructor Create;
  public

    procedure AddListener(AType: TOTAFileNotification; ACallback: TProc<string>);
    procedure RemoveListener(AType: TOTAFileNotification; ACallback: TProc<string>);
    class function GetInstance: TBossProjectListener;
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.Windows;

var
  FInstance: TBossProjectListener;

  { TBossProjectListener }

procedure TBossProjectListener.AddListener(AType: TOTAFileNotification; ACallback: TProc<string>);
var
  LListeners: TList<TProc<string>>;
begin
  if not FListeners.TryGetValue(AType, LListeners) then
  begin
    LListeners := TList < TProc < string >>.Create;
    FListeners.Add(AType, LListeners);
  end;
  LListeners.Add(ACallback)
end;

procedure TBossProjectListener.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TBossProjectListener.AfterSave;
begin

end;

procedure TBossProjectListener.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TBossProjectListener.BeforeSave;
begin

end;

constructor TBossProjectListener.Create;
begin
  FListeners := TObjectDictionary < TOTAFileNotification, TList < TProc<string> >>.Create([doOwnsValues]);
end;

destructor TBossProjectListener.Destroy;
begin
  FListeners.Free;
  inherited;
end;

procedure TBossProjectListener.Destroyed;
begin
end;

procedure TBossProjectListener.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
var
  LListeners: TList<TProc<string>>;
  LCallback: TProc<string>;
begin
  if not FListeners.TryGetValue(NotifyCode, LListeners) then
    Exit;

  for LCallback in LListeners do
  begin
    LCallback(FileName);
  end;
end;

class function TBossProjectListener.GetInstance: TBossProjectListener;
begin
  if not Assigned(FInstance) then
    FInstance := TBossProjectListener.Create;
  Result := FInstance;
end;

procedure TBossProjectListener.Modified;
begin

end;

procedure TBossProjectListener.RemoveListener(AType: TOTAFileNotification; ACallback: TProc<string>);
var
  LListeners: TList<TProc<string>>;
begin
  if FListeners.TryGetValue(AType, LListeners) then
  begin
    LListeners.Remove(ACallback);
  end;
end;

end.
