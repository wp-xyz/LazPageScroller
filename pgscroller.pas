{
 /***************************************************************************
                                 pgscroller.pas
                                 --------------
                             Component Library Code

 ***************************************************************************/
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit PgScroller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Graphics, Controls, ExtCtrls, ComCtrls, Buttons, ImgList;

type
  TScrollBtnSymbol = (sbsDefault, sbsSmallFilled, sbsSmallOpen, {sbsMedFilled, }sbsLargeFilled, sbsLargeOpen);
  TScrollMouseWheel = (smwDisabled, smwDefault, smwReverse);
  TPageScrollerOrientation = (soHorizontal, soVertical);

  TLazPageScroller = class(TCustomControl)
  //TLazPageScroller = class(TCustomPanel)
  private
    const
      DefaultBtnSize = 16;
  private
    FBtnSize: Integer;
    FBtnSymbol: TScrollBtnSymbol;
    FControl: TControl;
    FControlPanel: TCustomPanel;
    FOrientation: TPageScrollerOrientation;
    FScrollBtnUp: TSpeedButton;
    FScrollBtnDown: TSpeedButton;
    FScrollDistance: Integer;
    FScrollMouseWheel: TScrollMouseWheel;
    FOnChangeOrientation: TNotifyEvent;
    function BtnSizeIsStored: Boolean;
    function GetFlat: Boolean;
    function GetImageIndexDown: TImageIndex;
    function GetImageIndexUp: TImageIndex;
    function GetImages: TCustomImageList;
    function GetImagesWidth: Integer;
    procedure SetBtnSize(AValue: Integer);
    procedure SetBtnSymbol(AValue: TScrolLBtnSymbol);
    procedure SetControl(AValue: TControl);
    procedure SetFlat(AValue: Boolean);
    procedure SetImageIndexDown(AValue: TImageIndex);
    procedure SetImageIndexUp(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImagesWidth(AValue: Integer);
    procedure SetOrientation(AValue: TPageScrollerOrientation);

  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure DoChangeOrientation; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function GetScrollDistance: Integer;
    class function GetControlClassDefaultSize: TSize; override;
    procedure InternalScroll(Up: Boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure ScrollBtnClickHandler(Sender: TObject);
    procedure Scroll(ADelta: Integer); virtual;
    procedure UpdateScrollBtnSize;
    procedure UpdateScrollBtnSymbols;
    procedure UpdateScrollBtnVisibility;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property BtnSize: Integer read FBtnSize write SetBtnSize stored BtnSizeIsStored;
    property BtnSymbol: TScrollBtnSymbol read FBtnSymbol write SetBtnSymbol default sbsDefault;
    property Control: TControl read FControl write SetControl;
    property Flat: Boolean read GetFlat write SetFlat default false;
    property ImageIndexDown: TImageIndex read GetImageIndexDown write SetImageIndexDown default -1;
    property ImageIndexUp: TImageIndex read GetImageIndexUp write SetImageIndexUp default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImagesWidth: Integer read GetImagesWidth write SetImagesWidth;
    property Orientation: TPageScrollerOrientation read FOrientation write SetOrientation default soHorizontal;
    property ScrollDistance: Integer read FScrollDistance write FScrollDistance default 0;
    property ScrollMouseWheel: TScrollMouseWheel read FScrollMouseWheel write FScrollMouseWheel default smwDefault;
    property OnChangeOrientation: TNotifyEvent read FOnChangeOrientation write FOnChangeOrientation;

    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;          // to control size and color of the BtnSymbol characters
    property ParentColor;
    property ParentFont;

    property OnChangeBounds;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TToolbarHelper = class helper for TToolbar
    procedure SetOrientation(AValue: TPageScrollerOrientation);
  end;

implementation

constructor TLazPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents, csNoFocus, csParentBackground] - [csOpaque];

  FBtnSize := DefaultBtnSize;
  FScrollMouseWheel := smwDefault;

  FScrollBtnDown := TSpeedButton.Create(self);
  FScrollBtnDown.Parent := self;
  FScrollBtnDown.Width := FBtnSize;
  FScrollBtnDown.Align := alLeft;
  FScrollBtnDown.Caption := '<';
  FScrollBtnDown.OnClick := @ScrollBtnClickHandler;

  FScrollBtnUp := TSpeedButton.Create(self);
  FScrollBtnUp.Parent := self;
  FScrollBtnUp.Width := FBtnSize;
  FScrollBtnUp.Align := alRight;
  FScrollBtnUp.Caption := '>';
  FScrollBtnUp.OnClick := @ScrollBtnClickHandler;

  FControlPanel := TPanel.Create(Self);
  FControlPanel.Parent := Self;
  FControlPanel.Align := alClient;
  FControlPanel.BevelOuter := bvNone;
  FControlPanel.Caption := '';

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TLazPageScroller.BtnSizeIsStored: Boolean;
begin
  Result := FBtnSize <> DefaultBtnSize;
end;

{ Calculates the size used when AutoSize is active. }
procedure TLazPageScroller.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if Assigned(FControl) then
    FControl.GetPreferredSize(PreferredWidth, PreferredHeight, false, WithThemeSpace);
end;

{ Called by LCL scaling when the monitor resolution changes. }
procedure TLazPageScroller.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited;
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if BtnSizeIsStored then
      FBtnSize := round(FBtnSize * AXProportion);
  end;
end;

procedure TLazPageScroller.DoChangeOrientation;
begin
  if Assigned(FOnChangeOrientation) then
    FOnChangeOrientation(Self);
end;

function TLazPageScroller.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := Inherited;;
  if (not Result) and (FScrollMouseWheel <> smwDisabled) then
  begin
    InternalScroll(FScrollMouseWheel = smwDefault);
    Result := true;
  end;
end;

function TLazPageScroller.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := Inherited;;
  if (not Result) and (FScrollMouseWheel <> smwDisabled) then
  begin
    InternalScroll(FScrollMouseWheel = smwReverse);
    Result := true;
  end;
end;

class function TLazPageScroller.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 30;
end;

function TLazPageScroller.GetFlat: Boolean;
begin
  Result := FScrolLBtnDown.Flat;
end;

function TLazPageScroller.GetImageIndexDown: TImageIndex;
begin
  Result := FScrollBtnDown.ImageIndex;
end;

function TLazPageScroller.GetImageIndexUp: TImageIndex;
begin
  Result := FScrollBtnUp.ImageIndex;
end;

function TLazPageScroller.GetImages: TCustomImageList;
begin
  Result := FScrollBtnUp.Images;
end;

function TLazPageScroller.GetImagesWidth: Integer;
begin
  Result := FScrollBtnUp.ImageWidth;
end;

{ Returns the distance to be scrolled (in pixels) when a scroll button is clicked.
  This normally is the value of the ScrollDistance property, but when that is
  0 (or negative) scrolling goes by a full page (width/height of the scroller). }
function TLazPageScroller.GetScrollDistance: Integer;
begin
  if FScrollDistance <= 0 then
    case FOrientation of
      soHorizontal: Result := FControlPanel.Width;
      soVertical: Result := FControlPanel.Height;
    end
  else
    Result := FScrollDistance;
end;

procedure TLazPageScroller.InternalScroll(Up: Boolean);
var
  dist: Integer;
begin
  dist := GetScrollDistance;
  if Up then dist := -dist;
  Scroll(dist);
end;

procedure TLazPageScroller.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FControl then
      FControl := nil
    else
    if AComponent = GetImages then
      SetImages(nil);
  end;
end;

procedure TLazPageScroller.Resize;
begin
  inherited;
  Scroll(0);
end;

{ Performs the scrolling action. This simply is done by moving the Left (or Top)
  position of the embedded control. }
procedure TLazPageScroller.Scroll(ADelta: Integer);
var
  L, T: Integer;
begin
  if Assigned(FControl) then
  begin
    case FOrientation of
      soHorizontal:
        begin
          L := FControl.Left + ADelta;
          if L + FControl.Width < FControlPanel.Width then
            L := FControlPanel.Width - FControl.Width;
          if L > 0 then
            L := 0;
          FControl.Left := L;
        end;
      soVertical:
        begin
          T := FControl.Top + ADelta;
          if T + FControl.Height < FControlPanel.Height then
            T := FControlPanel.Height - FControl.Height;
          if T > 0 then
            T := 0;
          FControl.Top := T;
        end;
    end;
    UpdateScrollBtnVisibility;
  end;
end;

{ Handler for the OnClick event of the scroll buttons. }
procedure TLazPageScroller.ScrollBtnClickHandler(Sender:TObject);
begin
  if (Sender = FScrollBtndown) or (Sender = FScrollBtnUp) then
    InternalScroll(Sender = FScrollBtnUp);
end;

procedure TLazPageScroller.SetBtnSize(AValue: Integer);
begin
  if AValue <> FBtnSize then
  begin
    FBtnSize := AValue;
    UpdateScrollBtnSize;
  end;
end;

{ BtnSymbol defines the arrow symbol drawn on the scroll buttons. These are
  special ASCII or UTF8 characters. When an ImageList is assigned to the
  PageScroller, however, the icons at ImageIndexDown and ImageIndexUp are
  displayed instead. }
procedure TLazPageScroller.SetBtnSymbol(AValue: TScrollBtnSymbol);
begin
  if FBtnSymbol <> AValue then
  begin
    FBtnSymbol := AValue;
    UpdateScrollBtnSymbols;
  end;
end;

{ Assigns the control to the scroller. When it is wider (in
  case of horizontal orientation) or higher (in case of vertical orientation)
  than the scroller, scroll buttons are displayed which allow to scroll the
  control across the width (height) of the scroller.

  IMPORTANT: The Align property of the control must be alNone or alCustom for
  the scroller to work properly. }
procedure TLazPageScroller.SetControl(AValue: TControl);
begin
  if FControl <> AValue then
  begin
    FControl := AValue;
    FControl.Parent := FControlPanel;
    FControl.Left := 0;
    FControl.Top := 0;
    UpdateScrollBtnVisibility;
  end;
end;

procedure TLazPageScroller.SetFlat(AValue: Boolean);
begin
  if AValue <> FScrollBtnDown.Flat then
  begin
    FScrollBtnDown.Flat := AValue;
    FScrollBtnUp.Flat := AValue;
  end;
end;

procedure TLazPageScroller.SetImageIndexDown(AValue: TImageIndex);
begin
  if AValue <> FScrollBtnDown.ImageIndex then
    FScrollBtnDown.ImageIndex := AValue;
end;

procedure TLazPageScroller.SetImageIndexUp(AValue: TImageIndex);
begin
  if AValue <> FScrollBtnUp.ImageIndex then
    FScrollBtnUp.ImageIndex := AValue;
end;

procedure TLazPageScroller.SetImages(AValue: TCustomImageList);
begin
  if AValue <> GetImages then
  begin
    FScrollBtnDown.Images := AValue;
    FScrollBtnUp.Images := AValue;
    FScrollBtnDown.ShowCaption := AValue = nil;
    FScrollBtnUp.ShowCaption := AValue = nil;
  end;
end;

procedure TLazPageScroller.SetImagesWidth(AValue: Integer);
begin
  if AValue <> GetImagesWidth then
  begin
    FScrollBtnDown.ImageWidth := AValue;
    FScrollBtnUp.ImageWidth := AValue;
  end;
end;

{ Rotates the scroller from horizontal to vertical orientation, or vice versa.
  The calling routine must provide a handler for the OnChangeOrientation event
  to rotate the embedded control in the same way. }
procedure TLazPageScroller.SetOrientation(AValue: TPageScrollerOrientation);
var
  w, h: Integer;
begin
  if AValue = FOrientation then
    exit;
  w := Width;
  h := Height;
  FOrientation := AValue;
  SetBounds(Left, Top, h, w);
  case FOrientation of
    soHorizontal:
      begin
        FScrolLBtnDown.Align := alLeft;
        FScrollBtnUp.Align := alRight;
      end;
    soVertical:
      begin
        FScrollBtnDown.Align := alTop;
        FScrollBtnUp.Align := alBottom;
      end;
  end;
  UpdateScrollBtnSize;
  UpdateScrollBtnSymbols;
  DoChangeOrientation;
end;

procedure TLazPageScroller.UpdateScrollBtnSize;
begin
  case FOrientation of
    soHorizontal:
      begin
        FScrollBtnDown.Width := FBtnSize;
        FScrollBtnUp.Width := FBtnSize;
      end;
    soVertical:
      begin
        FScrollBtndown.Height := FBtnSize;
        FScrolLBtnUp.Height := FBtnSize;
      end;
  end;
end;

procedure TLazPageScroller.UpdateScrollBtnSymbols;

  procedure SetSymbols(ALeft, ARight, AUp, ADown: String);
  begin
    case FOrientation of
      soHorizontal:
        begin
          FScrollBtnDown.Caption := ALeft;
          FScrollBtnUp.Caption := ARight;
        end;
      soVertical:
        begin
          FScrollBtnDown.Caption := AUp;       // Up/down mix-up is intentional
          FScrollBtnUp.Caption := ADown;
        end;
    end;
  end;

begin
  case FBtnSymbol of
    sbsDefault: SetSymbols('<', '>', '^', 'v');      // ASCII
    sbsSmallFilled: SetSymbols(#$E2#$97#$82, #$E2#$96#$B8, #$E2#$96#$B4, #$E2#$96#$BE);    // "Geometric shapes" UTF8 range
    sbsSmallOpen: SetSymbols(#$E2#$97#$83, #$E2#$96#$B9, #$E2#$96#$B5, #$E2#$96#$BF);      // "Geometric shapes" UTF8 range
//    sbsMedFilled: SetSymbols(#$E2#$AF#$87, #$E2#$AF#$88, #$E2#$AF#$85, #$E2#$AF#$86);      // "Misc Symbols and Arrows" UTF8 range
    sbsLargeFilled: SetSymbols(#$E2#$97#$80, #$E2#$96#$B6, #$E2#$96#$B2, #$E2#$96#$BC);    // "Geometric shapes" UTF8 range
    sbsLargeOpen: SetSymbols(#$E2#$97#$81, #$E2#$96#$B7, #$E2#$96#$B3, #$E2#$96#$BD);      // "Geometric shapes" UTF8 range
  end;
end;

procedure TLazPageScroller.UpdateScrollBtnVisibility;
begin
  if FControl <> nil then
    case FOrientation of
      soHorizontal:
        begin
          FScrollBtnDown.Visible := FControl.Left < 0;
          FScrollBtnUp.Visible := FControl.Left + FControl.Width > FControlPanel.Width;
        end;
      soVertical:
        begin
          FScrollBtnDown.Visible := FControl.Top < 0;
          FScrollBtnUp.Visible := FControl.Top + FControl.Height > FControlPanel.Height;
        end;
    end;
end;



procedure TToolbarHelper.SetOrientation(AValue: TPageScrollerOrientation);
var
  w, h: Integer;
  R: TRect;
begin
  Align := alNone;
  w := Width;
  h := Height;
  if ((AValue = soHorizontal) and (h > w)) or
     ((AValue = soVertical) and (w > h)) then
  begin
    DisableAutoSizing;
    Width := h;
    Height := w;
    EnableAutoSizing;
    //SetBounds(Left, Top, h, w);
//    AlignControls(nil, R);
    case AValue of
      soHorizontal: WrapButtons(Width, w, h, false);
      soVertical: WrapButtons(Height, w, h, false);
    end;
  end;
end;

end.

