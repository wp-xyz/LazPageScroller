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
  Classes, SysUtils, Types, LazLoggerBase,
  Graphics, LMessages, Controls, ExtCtrls, ComCtrls, Buttons, ImgList;

type
  TScrollButtonSymbol = (sbsDefault, sbsSmallFilled, sbsSmallOpen, {sbsMedFilled, }sbsLargeFilled, sbsLargeOpen);
  TScrollMouseWheelMode = (mwmDisabled, mwmDefault, mwmReverse);
  TPageScrollerOrientation = (soHorizontal, soVertical);

  TLazPageScroller = class(TCustomControl)
//  TLazPageScroller = class(TCustomPanel)
  private
    const
      DefaultBtnSize = 16;
  private
    FAutoScroll: Boolean;
    FButtonSize: Integer;
    FButtonSymbol: TScrollButtonSymbol;
    FControl: TControl;
    FControlParent: TWinControl;
    FControlPanel: TCustomPanel;
    FMouseWheelMode: TScrollMouseWheelMode;
    FOrientation: TPageScrollerOrientation;
    FScrollBtnUp: TSpeedButton;
    FScrollBtnDown: TSpeedButton;
    FScrollDistance: Integer;
    FScrollTimer: TTimer;
    FOnChangeOrientation: TNotifyEvent;
    function ButtonSizeIsStored: Boolean;
    function GetFlat: Boolean;
    function GetImageIndexDown: TImageIndex;
    function GetImageIndexUp: TImageIndex;
    function GetImages: TCustomImageList;
    function GetImagesWidth: Integer;
    function GetMargin: Integer;
    function MarginIsStored: Boolean;
    procedure SetButtonSize(AValue: Integer);
    procedure SetButtonSymbol(AValue: TScrollButtonSymbol);
    procedure SetControl(AValue: TControl);
    procedure SetFlat(AValue: Boolean);
    procedure SetImageIndexDown(AValue: TImageIndex);
    procedure SetImageIndexUp(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImagesWidth(AValue: Integer);
    procedure SetMargin(AValue: Integer);
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
    procedure Scroll(ADelta: Integer); virtual;
    procedure ScrollButtonClickHandler(Sender: TObject);
    procedure ScrollButtonMouseEnterHandler(Sender: TObject);
    procedure ScrollButtonMouseLeaveHandler(Sender: TObject);
    procedure ScrollTimerHandler(Sender: TObject);
    procedure UpdateScrollButtonSize;
    procedure UpdateScrollButtonSymbols;
    procedure UpdateScrollButtonVisibility;

    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default false;
    property ButtonSize: Integer read FButtonSize write SetButtonSize stored ButtonSizeIsStored;
    property ButtonSymbol: TScrollButtonSymbol read FButtonSymbol write SetButtonSymbol default sbsDefault;
    property Control: TControl read FControl write SetControl;
    property Flat: Boolean read GetFlat write SetFlat default false;
    property ImageIndexDown: TImageIndex read GetImageIndexDown write SetImageIndexDown default -1;
    property ImageIndexUp: TImageIndex read GetImageIndexUp write SetImageIndexUp default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImagesWidth: Integer read GetImagesWidth write SetImagesWidth;
    property Margin: Integer read GetMargin write SetMargin stored MarginIsStored;
    property MouseWheelMode: TScrollMouseWheelMode read FMouseWheelMode write FMouseWheelMode default mwmDefault;
    property Orientation: TPageScrollerOrientation read FOrientation write SetOrientation default soHorizontal;
    property ScrollDistance: Integer read FScrollDistance write FScrollDistance default 0;
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

procedure Register;

implementation

{$R pagescroller_icons.res}

const
  SCROLL_DOWN_TAG = 0;
  SCROLL_UP_TAG = 1;

procedure Register;
begin
  RegisterComponents('LazControls', [TLazPageScroller]);
end;

constructor TLazPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents, csNoFocus, csParentBackground] - [csOpaque];

  FButtonSize := DefaultBtnSize;
  FMouseWheelMode := mwmDefault;  // Rotate mouse wheel to scroll the embedded control

  FScrollTimer := TTimer.Create(self);
  FScrollTimer.Enabled := false;
  FScrollTimer.Interval := 100;
  FScrollTimer.OnTimer := @ScrollTimerHandler;

  FScrollBtnDown := TSpeedButton.Create(self);
  FScrollBtnDown.Parent := self;
  FScrollBtnDown.Width := FButtonSize;
  FScrollBtnDown.Align := alLeft;
  FScrollBtnDown.Caption := '<';
  FScrollBtnDown.OnClick := @ScrollButtonClickHandler;
  FScrollBtnDown.OnMouseEnter := @ScrollButtonMouseEnterHandler;
  FScrollBtnDown.OnMouseLeave := @ScrollButtonMouseLeaveHandler;

  FScrollBtnUp := TSpeedButton.Create(self);
  FScrollBtnUp.Parent := self;
  FScrollBtnUp.Width := FButtonSize;
  FScrollBtnUp.Align := alRight;
  FScrollBtnUp.Caption := '>';
  FScrollBtnUp.OnClick := @ScrollButtonClickHandler;
  FScrollBtnUp.OnMouseEnter := @ScrollButtonMouseEnterHandler;
  FScrollBtnUp.OnMouseLeave := @ScrollButtonMouseLeaveHandler;

  FControlPanel := TPanel.Create(Self);
  FControlPanel.Parent := Self;
  FControlPanel.Align := alClient;
  FControlPanel.BevelOuter := bvNone;
  FControlPanel.Caption := '';

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TLazPageScroller.ButtonSizeIsStored: Boolean;
begin
  Result := FButtonSize <> DefaultBtnSize;
end;

{ Calculates the size used when AutoSize is active. }
procedure TLazPageScroller.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if Assigned(FControl) then
  begin
    FControl.GetPreferredSize(PreferredWidth, PreferredHeight, false, WithThemeSpace);
    inc(PreferredWidth, 2*FControlPanel.BorderSpacing.Around);
    inc(PreferredHeight, 2*FControlPanel.BorderSpacing.Around);
  end;
end;

procedure TLazPageScroller.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;
  if Orientation = soHorizontal then
  begin
    if IsRightToLeft then
    begin
      FScrollBtnDown.Align := alRight;
      FScrollBtnUp.Align := alLeft;
      if Assigned(FControl) then begin
        FControl.Anchors := [akTop, akRight];
        FControl.Left := FControlPanel.ClientWidth - FControl.Width;
      end;
    end else
    begin
      FScrollBtnDown.Align := alLeft;
      FScrollBtnUp.Align := alRight;
      if Assigned(FControl) then
      begin
        FControl.Anchors := [akLeft, akTop];
        FControl.Left := 0;
      end;
    end;
  end else
    FControl.Anchors := [akLeft, akTop];
end;

{ Called by LCL scaling when the monitor resolution changes. }
procedure TLazPageScroller.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited;
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if ButtonSizeIsStored then
      FButtonSize := round(FButtonSize * AXProportion);
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
  if (not Result) and (FMouseWheelMode <> mwmDisabled) then
  begin
    InternalScroll(FMouseWheelMode = mwmDefault);
    Result := true;
  end;
end;

function TLazPageScroller.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := Inherited;;
  if (not Result) and (FMouseWheelMode <> mwmDisabled) then
  begin
    InternalScroll(FMouseWheelMode = mwmReverse);
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

function TLazPageScroller.GetMargin: Integer;
begin
  Result := FControlPanel.BorderSpacing.Around;
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

function TLazPageScroller.MarginIsStored: Boolean;
begin
  Result := GetMargin <> 0;
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
  p: Integer;  // Control's anchor position
begin
  if Assigned(FControl) then
  begin
    case FOrientation of
      soHorizontal:
        if IsRightToLeft then
        begin
          // p is the position of the control's right side
          if FControl.Width < FControlPanel.Width then
            p := FControlPanel.Width
          else
          begin
            p := FControl.Left + FControl.Width - ADelta;
            if p < FControlPanel.Width then
              p := FControlPanel.Width;
            if p - FControl.Width > 0 then
              p := FControl.Width;
          end;
          FControl.Left := p - FControl.Width;
        end else
        begin
          // p is the position of the control's left side
          p := FControl.Left + ADelta;
          if p + FControl.Width < FControlPanel.Width then
            p := FControlPanel.Width - FControl.Width;
          if p > 0 then
            p := 0;
          FControl.Left := p;
        end;
      soVertical:
        begin
          // p is the position of the control's top side
          p := FControl.Top + ADelta;
          if p + FControl.Height < FControlPanel.Height then
            p := FControlPanel.Height - FControl.Height;
          if p > 0 then
            p := 0;
          FControl.Top := p;
        end;
    end;
    UpdateScrollButtonVisibility;
  end;
end;

{ Handler for the OnClick event of the scroll buttons. }
procedure TLazPageScroller.ScrollButtonClickHandler(Sender:TObject);
begin
  if (Sender = FScrollBtnDown) or (Sender = FScrollBtnUp) then
    InternalScroll(Sender = FScrollBtnUp);
end;

procedure TLazPageScroller.ScrollButtonMouseEnterHandler(Sender: TObject);
begin
  if FAutoScroll then
  begin
    if Sender = FScrollBtnUp then
      FScrollTimer.Tag := SCROLL_UP_TAG
    else
      FScrollTimer.Tag := SCROLL_DOWN_TAG;
    FScrollTimer.Enabled := true;
  end;
end;

procedure TLazPageScroller.ScrollButtonMouseLeaveHandler(Sender: TObject);
begin
  FScrollTimer.Enabled := false;
end;

{ Handler for the scroll timer which fires when AutoScroll is true and the
  mouse hovers over one button. }
procedure TLazPageScroller.ScrollTimerHandler(Sender: TObject);
begin
  InternalScroll(FScrollTimer.Tag = SCROLL_UP_TAG);
end;

procedure TLazPageScroller.SetButtonSize(AValue: Integer);
begin
  if AValue <> FButtonSize then
  begin
    FButtonSize := AValue;
    UpdateScrollButtonSize;
  end;
end;

{ ButtonSymbol defines the arrow symbol drawn on the scroll buttons. These are
  special ASCII or UTF8 characters. When an ImageList is assigned to the
  PageScroller, however, the icons at ImageIndexDown and ImageIndexUp are
  displayed instead. }
procedure TLazPageScroller.SetButtonSymbol(AValue: TScrollButtonSymbol);
begin
  if FButtonSymbol <> AValue then
  begin
    FButtonSymbol := AValue;
    UpdateScrollButtonSymbols;
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
    if Assigned(FControl) then
      FControl.Parent := FControlParent;

    FControl := AValue;

    if Assigned(FControl) then
    begin
      FControlParent := FControl.Parent;
      FControl.Parent := FControlPanel;
      if (FOrientation = soHorizontal) and IsRightToLeft then
        FControl.Left := FControlPanel.Width - FControl.Width
      else
        FControl.Left := 0;
      FControl.Top := 0;
    end else
      FControlParent := nil;

    UpdateScrollButtonVisibility;
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

procedure TLazPageScroller.SetMargin(AValue: Integer);
begin
  FControlPanel.BorderSpacing.Around := AValue;
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
        FScrollBtnDown.Align := alLeft;
        FScrollBtnUp.Align := alRight;
      end;
    soVertical:
      begin
        FScrollBtnDown.Align := alTop;
        FScrollBtnUp.Align := alBottom;
      end;
  end;
  UpdateScrollButtonSize;
  UpdateScrollButtonSymbols;
  DoChangeOrientation;
end;

procedure TLazPageScroller.UpdateScrollButtonSize;
begin
  case FOrientation of
    soHorizontal:
      begin
        FScrollBtnDown.Width := FButtonSize;
        FScrollBtnUp.Width := FButtonSize;
      end;
    soVertical:
      begin
        FScrollBtndown.Height := FButtonSize;
        FScrolLBtnUp.Height := FButtonSize;
      end;
  end;
end;

procedure TLazPageScroller.UpdateScrollButtonSymbols;

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
  case FButtonSymbol of
    sbsDefault: SetSymbols('<', '>', '^', 'v');      // ASCII
    sbsSmallFilled: SetSymbols(#$E2#$97#$82, #$E2#$96#$B8, #$E2#$96#$B4, #$E2#$96#$BE);    // "Geometric shapes" UTF8 range
    sbsSmallOpen: SetSymbols(#$E2#$97#$83, #$E2#$96#$B9, #$E2#$96#$B5, #$E2#$96#$BF);      // "Geometric shapes" UTF8 range
//    sbsMedFilled: SetSymbols(#$E2#$AF#$87, #$E2#$AF#$88, #$E2#$AF#$85, #$E2#$AF#$86);      // "Misc Symbols and Arrows" UTF8 range
    sbsLargeFilled: SetSymbols(#$E2#$97#$80, #$E2#$96#$B6, #$E2#$96#$B2, #$E2#$96#$BC);    // "Geometric shapes" UTF8 range
    sbsLargeOpen: SetSymbols(#$E2#$97#$81, #$E2#$96#$B7, #$E2#$96#$B3, #$E2#$96#$BD);      // "Geometric shapes" UTF8 range
  end;
end;

procedure TLazPageScroller.UpdateScrollButtonVisibility;
begin
  if FControl <> nil then
    case FOrientation of
      soHorizontal:
        if IsRightToLeft then
        begin
          FScrollBtnDown.Visible := FControl.Left + FControl.Width > FControlPanel.Width;
          FScrollBtnUp.Visible := FControl.Left < 0;
        end else
        begin
          FScrollBtnDown.Visible := FControl.Left < 0;
          FScrollBtnUp.Visible := FControl.Left + FControl.Width > FControlPanel.Width;
        end;
      soVertical:
        begin
          FScrollBtnDown.Visible := FControl.Top < 0;
          FScrollBtnUp.Visible := FControl.Top + FControl.Height > FControlPanel.Height;
        end;
    end
  else
  begin
    FScrollBtnUp.Visible := false;
    FScrollBtnDown.Visible := false;
  end;
end;

procedure TToolbarHelper.SetOrientation(AValue: TPageScrollerOrientation);
var
  w, h: Integer;
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

