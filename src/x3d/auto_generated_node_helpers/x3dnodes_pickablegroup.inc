{ -*- buffer-read-only: t -*-

  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal .

  The documentation for properties should go to x3dnodes_documentation.txt . }

{$ifdef read_interface}

  strict private
    function GetPickable: Boolean;
    procedure SetPickable(const Value: Boolean);
  public
    { Set X3D field "objectType". } { }
    procedure SetObjectType(const Value: array of string);
    { Set X3D field "objectType". } { }
    procedure SetObjectType(const Value: TCastleStringList);
    { X3D field "pickable". } { }
    property Pickable: Boolean read GetPickable write SetPickable;

{$endif read_interface}

{$ifdef read_implementation}

{ TPickableGroupNode ----------------------------------------------- }

procedure TPickableGroupNode.SetObjectType(const Value: array of string);
begin
  FdObjectType.Send(Value);
end;

procedure TPickableGroupNode.SetObjectType(const Value: TCastleStringList);
begin
  FdObjectType.Send(Value);
end;

function TPickableGroupNode.GetPickable: Boolean;
begin
  Result := FdPickable.Value;
end;

procedure TPickableGroupNode.SetPickable(const Value: Boolean);
begin
  FdPickable.Send(Value);
end;

{$endif read_implementation}
