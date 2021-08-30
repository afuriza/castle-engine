unit OpenProjectDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CastleDialogs, BCButtonFocus;

type

  { TfrOpenProject }

  TfrOpenProject = class(TForm)
    bcOpenProject: TBCButtonFocus;
    bcSelectProject: TBCButtonFocus;
    Label1: TLabel;
    Label2: TLabel;
    EditProjectPath: TLabeledEdit;
    lbRecents: TListBox;
    OpenProject: TCastleOpenDialog;
    procedure bcSelectProjectClick(Sender: TObject);
    procedure bcOpenProjectClick(Sender: TObject);
    procedure EditProjectPathChange(Sender: TObject);
    procedure EditProjectPathEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbRecentsClick(Sender: TObject);
  private

  public

  end;

var
  frOpenProject: TfrOpenProject;

implementation

{$R *.lfm}

uses
  ProjectWizard, CastleLCLUtils, CastleURIUtils, CastleStringUtils, CastleUtils,
  EditorUtils;

{ TfrOpenProject }

procedure TfrOpenProject.EditProjectPathEnter(Sender: TObject);
begin

end;

procedure TfrOpenProject.FormActivate(Sender: TObject);
begin
end;

procedure TfrOpenProject.FormShow(Sender: TObject);
var
  I: Integer;
  Url, S, NotExistingSuffix: String;
begin
  lbRecents.Items.Clear;
  if FrProjectWizard.RecentProjects.URLs.Count > 0 then
    EditProjectPath.Text := FrProjectWizard.RecentProjects.URLs[I];
  for I := 0 to FrProjectWizard.RecentProjects.URLs.Count - 1 do
  begin
    Url := FrProjectWizard.RecentProjects.URLs[I];

    if URIExists(Url) in [ueFile, ueUnknown] then
      NotExistingSuffix := ''
    else
      NotExistingSuffix := ' (PROJECT FILES MISSING)';

    // show file URLs simpler, esp to avoid showing space as %20
    Url := SuffixRemove('/CastleEngineManifest.xml', Url, true);
    if URIProtocol(Url) = 'file' then
      S := URIToFilenameSafeUTF8(Url)
    else
      S := URIDisplay(Url);
    lbRecents.Items.Add(SQuoteLCLCaption(S + NotExistingSuffix));
  end;
end;

procedure TfrOpenProject.lbRecentsClick(Sender: TObject);
var
  Url: String;
  i, ActiveLb: integer;
begin
  if lbRecents.SelCount > 0 then
  begin
    for i := 0 to lbRecents.Items.Count -1 do
    begin
      if lbRecents.Selected[i] then
        ActiveLb := i;
    end;
    Url := FrProjectWizard.RecentProjects.URLs[ActiveLb];

    if not (URIExists(Url) in [ueFile, ueUnknown]) then
    begin
      if YesNoBox(Format('Project file "%s" does not exist. Remove the project from the recent list?', [
        URIDisplay(Url)
      ])) then
      begin
        FrProjectWizard.RecentProjects.URLs.Delete(ActiveLb);
        lbRecents.Items.Delete(ActiveLb);
      end;
      Exit;
    end;

    EditProjectPath.Text := Url;

  end;
end;

procedure TfrOpenProject.EditProjectPathChange(Sender: TObject);
begin

end;

procedure TfrOpenProject.bcSelectProjectClick(Sender: TObject);
begin
  { This is critical in a corner case:
    - You run CGE editor such that it detects as "data directory"
      current directory. E.g. you compiled it manually and run on Unix as
      "tools/castle-editor/castle-editor"
    - Now you open project in subdirectory. (E.g. some CGE example,
      to continue previous example.)
    - With UseCastleDataProtocol, OpenProject.URL will now be like
      'castle-data:/examples/xxx/CastleEngineManifest.xml'.
      Which means that it's absolute (AbsoluteURI in ProjectOpen will not change it),
      but it's also bad to be used (because later we will set ApplicationDataOverride
      to something derived from it, thus ResolveCastleDataURL will resolve
      castle-data:/ to another castle-data:/ , and it will make no sense
      since one castle-data:/ assumes ApplicationDataOverride = '' ...).
  }
  OpenProject.UseCastleDataProtocol := false;

  if OpenProject.Execute then
  begin
    try
      EditProjectPath.Text := OpenProject.URL;
      lbRecents.ClearSelection;
    except
      raise;
    end;
  end;
end;

procedure TfrOpenProject.bcOpenProjectClick(Sender: TObject);
begin
  FrProjectWizard.ProjectOpen(EditProjectPath.Text);
end;

end.

