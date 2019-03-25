unit CnCommon.lite;

interface

uses
  System.Classes;

function _CnExtractFileDir(const FileName: string): string;
function _CnExtractFilePath(const FileName: string): string;
function _CnExtractFileExt(const FileName: string): string;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
function ExpandEnvironmentVar(var Value: string): Boolean;

function LinkPath(const Head, Tail: string): string;
function MakeDir(const Path: string): string;
function MakePath(const Dir: string): string;
function AddDirSuffix(const Dir: string): string;

implementation

uses
  System.SysUtils, Winapi.Windows;

procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  Assert(Dest <> nil);
  Dest.Clear;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do
    begin
      Dest.Add(P);
      P := StrEnd(P);
      Inc(P);
    end;
  end;
end;

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

function _CnExtractFileDir(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  if (I > 1) and (FileName[I] = PathDelim) and (not IsDelimiter(PathDelim + DriveDelim, FileName, I - 1)) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}

begin
  Result := ExtractFileDir(FileName);
end;
{$ENDIF}

function _CnExtractFilePath(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}
begin
  Result := ExtractFilePath(FileName);
end;
{$ENDIF}

function _CnExtractFileExt(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;
{$ELSE}
begin
  Result := ExtractFileExt(FileName);
end;
{$ENDIF}

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.Clear;
  Raw := GetEnvironmentStrings;
  try
    MultiSzToStrings(Vars, Raw);
    Result := True;
  finally
    FreeEnvironmentStrings(Raw);
  end;
  if Expand then
  begin
    for I := 0 to Vars.Count - 1 do
    begin
      Expanded := Vars[I];
      if ExpandEnvironmentVar(Expanded) then
        Vars[I] := Expanded;
    end;
  end;
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;

function LinkPath(const Head, Tail: string): string;
var
  HeadIsUrl: Boolean;
  TailHasRoot: Boolean;
  TailIsRel: Boolean;
  AHead, ATail, S: string;
  UrlPos, i: Integer;
begin
  if Head = '' then
  begin
    Result := Tail;
    Exit;
  end;

  if Tail = '' then
  begin
    Result := Head;
    Exit;
  end;

  TailHasRoot := (AnsiPos(':\', Tail) = 2) or // C:\Test
                 (AnsiPos('\\', Tail) = 1) or // \\Name\C\Test
                 (AnsiPos('://', Tail) > 0);  // ftp://ftp.abc.com
  if TailHasRoot then
  begin
    Result := Tail;
    Exit;
  end;

  UrlPos := AnsiPos('://', Head);
  HeadIsUrl := UrlPos > 0;
  AHead := StringReplace(Head, '/', '\', [rfReplaceAll]);
  ATail := StringReplace(Tail, '/', '\', [rfReplaceAll]);

  TailIsRel := ATail[1] = '\';
  if TailIsRel then
  begin
    if AnsiPos(':\', AHead) = 2 then
      Result := AHead[1] + ':' + ATail
    else if AnsiPos('\\', AHead) = 1 then
    begin
      S := Copy(AHead, 3, MaxInt);
      i := AnsiPos('\', S);
      if i > 0 then
        Result := Copy(AHead, 1, i + 1) + ATail
      else
        Result := AHead + ATail;
    end else if HeadIsUrl then
    begin
      S := Copy(AHead, UrlPos + 3, MaxInt);
      i := AnsiPos('\', S);
      if i > 0 then
        Result := Copy(AHead, 1, i + UrlPos + 1) + ATail
      else
        Result := AHead + ATail;
    end
    else
    begin
      Result := Tail;
      Exit;
    end;
  end
  else
  begin
    if Copy(ATail, 1, 2) = '.\' then
      Delete(ATail, 1, 2);
    AHead := MakeDir(AHead);
    i := Pos('..\', ATail);
    while i > 0 do
    begin
      AHead := _CnExtractFileDir(AHead);
      Delete(ATail, 1, 3);
      i := Pos('..\', ATail);
    end;
    Result := MakePath(AHead) + ATail;
  end;

  if HeadIsUrl then
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

function MakeDir(const Path: string): string;
begin
  Result := Trim(Path);
  if Result = '' then Exit;
  if CharInSet(Result[Length(Result)], ['/', '\']) then
    Delete(Result, Length(Result), 1);
end;

function MakePath(const Dir: string): string;
begin
  Result := AddDirSuffix(Dir);
end;

function AddDirSuffix(const Dir: string): string;
begin
  Result := Trim(Dir);
  if Result = '' then Exit;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF};
end;

end.
