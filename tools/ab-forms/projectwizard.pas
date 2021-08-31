{
  Copyright 2018-2021 by Michalis Kamburelis, modified by Dio Affriza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit ProjectWizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, BCButtonFocus, BCMDButtonFocus, BCMaterialDesignButton,
  BGRAThemeButton, BCPanel, NewProjectDialog, OpenProjectDialog, UIHelper,
  CastleDialogs, CastleLCLRecentFiles, CastleUriUtils, FormProject;

type

  { TFrProjectWizard }

  TFrProjectWizard = class(TForm)
    bcNew: TBCButtonFocus;
    bcOpen: TBCButtonFocus;
    bcPreferences: TBCButtonFocus;
    Label1: TLabel;
    pnTabPos: TPanel;
    pnProject: TBCPanel;
    Image1: TImage;
    procedure bcNewClick(Sender: TObject);
    procedure bcOpenClick(Sender: TObject);
    procedure bcPreferencesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    RecentProjects: TCastleRecentFiles;
    procedure ProjectOpen(ManifestUrl: string);
  end;

var
  FrProjectWizard: TFrProjectWizard;

implementation

{$R *.lfm}

uses CastleConfig, CastleLCLUtils, CastleUtils,
  CastleFilesUtils, CastleParameters, CastleLog, CastleStringUtils,
  ProjectUtils, EditorUtils, FormNewProject, FormPreferences,
  ToolCompilerInfo, ToolFpcVersion,
  FormNewUnit;

{ TFrProjectWizard }

procedure TFrProjectWizard.ProjectOpen(ManifestUrl: string);
begin
  ManifestUrl := AbsoluteURI(ManifestUrl);

  // Validate
  if not URIFileExists(ManifestUrl) then
    raise Exception.CreateFmt('Cannot find CastleEngineManifest.xml at this location: "%s". Invalid project opened.',
      [ManifestUrl]);

  ProjectForm := TProjectForm.Create(Application);
  ProjectForm.OpenProject(ManifestUrl);
  ProjectForm.Show;

  { Do this even if you just opened this project through "recent" menu.
    This way URL is moved to the top. }
  RecentProjects.Add(ManifestUrl);
end;

procedure TFrProjectWizard.FormShow(Sender: TObject);
begin
  frOpenProject.Parent := pnProject;
  frOpenProject.BorderStyle := bsNone;
  frOpenProject.Align := alClient;
  frOpenProject.Color := pnProject.Background.Color;
  frOpenProject.lbRecents.Color := bcclStateNormal;
  frOpenProject.lbRecents.Font.Color := clWhite;
  frCreateProject.Parent := pnProject;
  frCreateProject.BorderStyle := bsNone;
  frCreateProject.Align := alClient;
  frCreateProject.Color := pnProject.Background.Color;
  pnTabPos.Top := bcNew.Top;
  frOpenProject.Hide;
  frCreateProject.Show;
end;

procedure TFrProjectWizard.bcNewClick(Sender: TObject);
begin
  pnTabPos.Top := bcNew.Top;
  frOpenProject.Hide;
  frCreateProject.Show;
end;

procedure TFrProjectWizard.bcOpenClick(Sender: TObject);
begin
  pnTabPos.Top := bcOpen.Top;
  frCreateProject.Hide;
  frOpenProject.Show;
end;

procedure TFrProjectWizard.bcPreferencesClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
  //UpdateWarningFpcLazarus;
end;

procedure TFrProjectWizard.FormCreate(Sender: TObject);
  procedure ConfigLoad;
  begin
    FpcCustomPath := UserConfig.GetValue('fpc_custom_path', '');
    LazarusCustomPath := UserConfig.GetValue('lazarus_custom_path', '');
    CodeEditor := TCodeEditor(UserConfig.GetValue('code_editor/setting', Ord(DefaultCodeEditor)));
    CodeEditorCommand := UserConfig.GetValue('code_editor/command', '');
    CodeEditorCommandProject := UserConfig.GetValue('code_editor/command_project', '');
    MuteOnRun := UserConfig.GetValue('sound/mute_on_run', DefaultMuteOnRun);
    EditorVolume := UserConfig.GetFloat('sound/editor_volume', DefaultEditorVolume);
    SoundEngineSetVolume;
  end;

begin
  UserConfig.Load;
  RecentProjects := TCastleRecentFiles.Create(Self);
  RecentProjects.LoadFromConfig(UserConfig);
  //  RecentProjects.NextMenuItem := ; // unused for now
  ConfigLoad;
end;

end.

