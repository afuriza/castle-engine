{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleScene, TCastleViewport and related functionality.
  Follow the relevant tutorial pages
  https://castle-engine.io/manual_load_3d.php
  https://castle-engine.io/manual_scene.php
}
program cars_demo;

uses SysUtils, CastleVectors, CastleTransform, CastleUIControls, CastleUtils,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene,
  CastleKeysMouse, CastleBoxes, X3DNodes, CastleViewport;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  CarScene, RoadScene: TCastleScene;
  CarTransforms: array [1..20] of TCastleTransform;

procedure WindowUpdate(Container: TCastleContainer);

  procedure UpdateCarTransform(const CarTransform: TCastleTransform);
  var
    T: TVector3;
  begin
    T := CarTransform.Translation;
    { Thanks to multiplying by SecondsPassed, it is a time-based operation,
      and will always move 40 units / per second along the +Z axis. }
    T := T + Vector3(0, 0, 40) * Container.Fps.SecondsPassed;
    { Wrap the Z position, to move in a loop }
    if T.Z > 70 then
      T.Z := -50;
    CarTransform.Translation := T;
  end;

var
  I: Integer;
begin
  for I := Low(CarTransforms) to High(CarTransforms) do
    UpdateCarTransform(CarTransforms[I]);
end;

procedure WindowPress(Container: TCastleContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey('c') then
    CarTransforms[1].Exists := not CarTransforms[1].Exists;

  { capture a screenshot }
  if Event.IsKey(keyF5) then
    Window.Container.SaveScreenToDefaultFile;
end;

function CreateBoxesScene: TCastleScene;
const
  WallHeight = 5;
var
  RoadBox: TBox3D;
  RootNode: TX3DRootNode;
  Appearance: TAppearanceNode;
  Material: TMaterialNode;
  Shape1, Shape2: TShapeNode;
  Box1, Box2: TBoxNode;
  Transform1, Transform2: TTransformNode;
begin
  { The created geometry will automatically adjust to the bounding box
    of the road 3D model. }
  RoadBox := RoadScene.BoundingBox;
  if RoadBox.IsEmpty then
    raise Exception.Create('Invalid road 3D model: empty bounding box');

  Material := TMaterialNode.Create;
  { Yellow (we could have also used YellowRGB constant from CastleColors unit) }
  Material.DiffuseColor := Vector3(1, 1, 0);
  Material.Transparency := 0.75;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  { Note: you could use TBoxNode.CreateWithTransform shortcut method
    to create Box1, Shape1, Transform1 in one instruction.
    But we show the longer version below, as it's easier to understand. }

  Box1 := TBoxNode.Create('box_1_geometry');
  Box1.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

  Shape1 := TShapeNode.Create('box_1_shape');
  Shape1.Appearance := Appearance;
  Shape1.Geometry := Box1;

  Transform1 := TTransformNode.Create('box_1_transform');
  Transform1.Translation := Vector3(RoadBox.Min.X, WallHeight / 2, RoadBox.Center.Z);
  Transform1.AddChildren(Shape1);

  Box2 := TBoxNode.Create('box_2_geometry');
  Box2.Size := Vector3(0.5, WallHeight, RoadBox.Size.Z);

  Shape2 := TShapeNode.Create('box_2_shape');
  { Reuse the same Appearance node for another shape.
    This is perfectly allowed (the X3D is actually a graph, not a tree). }
  Shape2.Appearance := Appearance;
  Shape2.Geometry := Box2;

  Transform2 := TTransformNode.Create('box_2_transform');
  Transform2.Translation := Vector3(RoadBox.Max.X, WallHeight / 2, RoadBox.Center.Z);
  Transform2.AddChildren(Shape2);

  RootNode := TX3DRootNode.Create;
  RootNode.AddChildren(Transform1);
  RootNode.AddChildren(Transform2);

  Result := TCastleScene.Create(Application);
  Result.Load(RootNode, true);
end;

var
  I: Integer;
begin
  Window := TCastleWindowBase.Create(Application);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  CarScene := TCastleScene.Create(Application);
  CarScene.Load('castle-data:/car.gltf');
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.PlayAnimation('wheels_turning', true);

  for I := Low(CarTransforms) to High(CarTransforms) do
  begin
    CarTransforms[I] := TCastleTransform.Create(Application);
    CarTransforms[I].Translation := Vector3(
       (Random(4) - 2) * 6, 0, RandomFloatRange(-70, 50));
    CarTransforms[I].Add(CarScene);
    Viewport.Items.Add(CarTransforms[I]);
  end;

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load('castle-data:/road.gltf');
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];

  Viewport.Items.Add(RoadScene);
  Viewport.Items.MainScene := RoadScene;

  Viewport.Items.Add(CreateBoxesScene);

  Viewport.Camera.SetView(
    Vector3(-11.34, 30.04, 96.07), // position
    Vector3(0.10, -0.49, -0.87), // direction
    Vector3(0.35, 0.83, -0.43), // up (current)
    Vector3(0.00, 1.00, 0.00) // gravity up
  );

  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.Open;
  Application.Run;
end.
