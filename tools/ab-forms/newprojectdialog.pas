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
unit NewProjectDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, FileUtil, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, BCButtonFocus, FGL, UIHelper, ProjectUtils;

type

  TBCButtonFocusList = specialize TFPGObjectList<TBCButtonFocus>;

  { TfrCreateProject }

  TfrCreateProject = class(TForm)
    BCButtonFocus1: TBCButtonFocus;
    bcEmpty: TBCButtonFocus;
    bc3DViewer: TBCButtonFocus;
    bc3DFPS: TBCButtonFocus;
    bc2DGame: TBCButtonFocus;
    BCButtonFocus6: TBCButtonFocus;
    Label1: TLabel;
    EditLocation: TLabeledEdit;
    EditProjectName: TLabeledEdit;
    EditProjectCaption: TLabeledEdit;
    EditStateName: TLabeledEdit;
    CloseTarget: TForm;
    procedure BCButtonFocus1Click(Sender: TObject);
    procedure bcEmptyClick(Sender: TObject);
    procedure bcEmptyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    bcCollection: TBCButtonFocusList;
    procedure AdjustStateNameUi;
  public

  end;

var
  frCreateProject: TfrCreateProject;

implementation

{$R *.lfm}

uses {$ifdef MSWINDOWS} WinDirs, {$endif}
  LazFileUtils,
  CastleURIUtils, CastleConfig, CastleUtils, CastleStringUtils,
  EditorUtils, ProjectWizard;

{ TfrCreateProject }

procedure TfrCreateProject.FormCreate(Sender: TObject);
var
  i: integer;
begin
  bcCollection := TBCButtonFocusList.Create(False);
  bcCollection.Add(bcEmpty);
  bcCollection.Add(bc3DViewer);
  bcCollection.Add(bc3DFPS);
  bcCollection.Add(bc2DGame);
  for i := 0 to bcCollection.Count -1 do
  begin
    bcCollection[i].OnKeyUp := @bcEmptyKeyUp;
  end;
end;

procedure TfrCreateProject.bcEmptyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: integer;
begin
  if Key = 9 then
  begin
    for i := 0 to bcCollection.Count -1 do
    begin
      bcCollection[i].StateNormal.Background.Color := bcclStateNormal;
    end;
    TBCButtonFocus(Sender).StateNormal.Background.Color := bcclStateActive;
    TBCButtonFocus(Sender).StateHover.Background.Color := bcclStateActive;
  end;
end;

procedure TfrCreateProject.FormDestroy(Sender: TObject);
begin
  bcCollection.Free;
end;

procedure TfrCreateProject.FormShow(Sender: TObject);

  function DefaultProjectsParentDir: String;
  begin
    Result :=
      {$ifdef MSWINDOWS}
      // get "Documents" dir
      GetWindowsSpecialDir(CSIDL_PERSONAL);
      {$else}
      GetUserDir;
      {$endif}
  end;

var
  DefaultNewProjectDir, NewProjectDir: String;
begin
  { Initialize everything to default values }

  //ButtonTemplateEmpty.Down := true;

  DefaultNewProjectDir := InclPathDelim(DefaultProjectsParentDir) +
    'Castle Game Engine Projects';
  NewProjectDir := UserConfig.GetValue('new_project/default_dir', DefaultNewProjectDir);
  EditLocation.Text := NewProjectDir;

  EditProjectName.Text := 'my-new-project';

  EditStateName.Text := 'Main';

  AdjustStateNameUi;
end;

procedure TfrCreateProject.AdjustStateNameUi;
var
  AskForStateName: Boolean;
begin
  AskForStateName := bcEmpty.Down or bc3dViewer.Down;
  SetEnabledVisible(EditStateName.EditLabel, AskForStateName);
  SetEnabledVisible(EditStateName, AskForStateName);
end;

procedure TfrCreateProject.bcEmptyClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to bcCollection.Count -1 do
  begin
    bcCollection[i].StateNormal.Background.Color := bcclStateNormal;
  end;
  TBCButtonFocus(Sender).StateNormal.Background.Color := bcclStateActive;
  TBCButtonFocus(Sender).StateHover.Background.Color := bcclStateActive;
  AdjustStateNameUi;
end;

procedure TfrCreateProject.BCButtonFocus1Click(Sender: TObject);
const
  AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9'];
  ValidProjectNameChars = AlphaNum + ['_', '-'];
  InvalidProjectNameChars = AllChars - ValidProjectNameChars;
var
  ProjectDir, ProjectDirUrl, ManifestUrl, TemplateName: String;
  InvalidIndex: Integer;
  ProjectName, ProjectLocation: TCaption;
  CanClose: Boolean = False;
begin

  ProjectName := EditProjectName.Text;
  ProjectLocation := EditLocation.Text;

  if ProjectName = '' then
  begin
    ErrorBox('Project name cannot be empty.');
    CanClose := false;
    Exit;
  end;

  InvalidIndex := CharsPos(InvalidProjectNameChars, ProjectName);
  if InvalidIndex <> 0 then
  begin
    ErrorBox(Format('Project name contains invalid character "%s".' + NL +
      NL +
      'The internal project name is used with various tools, in various contexts, and thus it is limited to alphanumeric characters plus underscore ("_") and hyphen ("-").' + NL +
      NL +
      'Note that this is only an internal project name. The user-visible "Project Caption" has no such limitations.',
      [SReadableForm(ProjectName[InvalidIndex])]));
    CanClose := false;
    Exit;
  end;

  if ProjectLocation = '' then
  begin
    ErrorBox('No Project Location chosen.');
    CanClose := false;
    Exit;
  end;

  ProjectDir := InclPathDelim(ProjectLocation) + ProjectName;
  if DirectoryExists(ProjectDir) then
  begin
    ErrorBox(Format('Directory "%s" already exists, cannot create a project there. Please pick a project name that does not correspond to an already-existing directory.',
      [ProjectDir]));
    CanClose := false;
    Exit;
  end;

  if EditStateName.Visible and not IsValidIdent(EditStateName.Text) then
  begin
    ErrorBox(Format('State name "%s" is not a valid Pascal identifier',
      [EditStateName.Text]));
    CanClose := false;
    Exit;
  end;

  UserConfig.SetValue('new_project/default_dir', ProjectLocation);

  UseEditorApplicationData; // we use our castle-data:/xxx to copy template

  try
    // Create project dir
    ProjectDir := InclPathDelim(EditLocation.Text) + EditProjectName.Text;
    ProjectDirUrl := FilenameToURISafe(InclPathDelim(ProjectDir));
    if not ForceDirectories(ProjectDir) then
      raise Exception.CreateFmt('Cannot create directory "%s".', [ProjectDir]);

    // Calculate TemplateName
    if bcEmpty.StateNormal.Background.Color = bcclStateActive then
      TemplateName := 'empty'
    else
    if bc3dViewer.StateNormal.Background.Color = bcclStateActive then
      TemplateName := '3d_model_viewer'
    else
    if bc3DFPS.StateNormal.Background.Color = bcclStateActive then
      TemplateName := '3d_fps_game'
    else
    if bc2DGame.StateNormal.Background.Color = bcclStateActive then
      TemplateName := '2d_game'
    else
      raise EInternalError.Create('Unknown project template selected');

    // Fill project dir
    CopyTemplate(ProjectDirUrl, TemplateName,
      EditProjectName.Text,
      EditProjectCaption.Text,
      EditStateName.Text);
    GenerateProgramWithBuildTool(ProjectDirUrl);

    // Open new project
    ManifestUrl := CombineURI(ProjectDirUrl, 'CastleEngineManifest.xml');
    FrProjectWizard.ProjectOpen(ManifestUrl);
  except
    on E: Exception do
    begin
      Show;
      ErrorBox(ExceptMessage(E));
    end;
  end;

  if CanClose then
    CloseTarget.Hide;
end;

end.

