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
  TScrollButtonSymbol = (sbsDefault, sbsSmallFilled, sbsSmallOpen, {sbsMedFilled, }sbsLargeFilled, sbsLargeOpen, sbsImage);
  TScrollMouseWheelMode = (mwmDisabled, mwmDefault, mwmReverse);
  TPageScrollerOrientation = (soHorizontal, soVertical);

  TLazPageScroller = class(TCustomControl)
//  TLazPageScroller = class(TCustomPanel)
  private
    const
      DefaultBtnSize = 16;
      DefaultScrollInterval = 100;
      DefaultFirstScrollInterval = 300;
      SCROLL_LEFT_OR_UP = 1;
      SCROLL_RIGHT_OR_DOWN = 2;
      IMG_LEFT = SCROLL_LEFT_OR_UP;
      IMG_RIGHT = SCROLL_RIGHT_OR_DOWN;
      IMG_UP = IMG_LEFT + 2;
      IMG_DOWN = IMG_RIGHT + 2;
    type
      TScrollButton = class(TCustomControl)
      private
        FSpeedButton: TSpeedButton;
      public
        constructor Create(AOwner: TComponent); override;
        function MouseOver: Boolean;
        property SpeedButton: TSpeedButton read FSpeedButton;
      end;
  private
    FAutoScroll: Boolean;
    FButtonSize: Integer;
    FButtonSymbol: TScrollButtonSymbol;
    FControl: TControl;
    FControlParent: TWinControl;
    FImageIndex: array[IMG_LEFT..IMG_DOWN] of TImageIndex;
    FImages: TCustomImageList;
//    FMargin: Integer;
    FMouseWheelMode: TScrollMouseWheelMode;
    FOrientation: TPageScrollerOrientation;
    FScrollBtn: array[SCROLL_LEFT_OR_UP..SCROLL_RIGHT_OR_DOWN] of TScrollButton;
    FScrollDistance: Integer;
    FScrollTimer: TTimer;
    FFirstScrollTimer: TTimer;
    FScrolling: Integer;
    FWrappingPanel: TPanel;
    FOnChangeOrientation: TNotifyEvent;
    function ButtonSizeIsStored: Boolean;
    function GetFlat: Boolean;
    function GetImageIndex(AIndex: Integer): TImageIndex;
    function GetImagesWidth: Integer;
    function GetMargin: Integer;
    function GetScrollInterval(AIndex: Integer): Integer;
//    function MarginIsStored: Boolean;
    procedure SetButtonSize(AValue: Integer);
    procedure SetButtonSymbol(AValue: TScrollButtonSymbol);
    procedure SetControl(AValue: TControl);
    procedure SetFlat(AValue: Boolean);
    procedure SetImageIndex(AIndex: Integer; AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImagesWidth(AValue: Integer);
    procedure SetMargin(AValue: Integer);
    procedure SetScrollInterval(AIndex, AValue: Integer);
    procedure SetOrientation(AValue: TPageScrollerOrientation);

  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure DoChangeOrientation; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure FirstScrollTimerHandler(Sender: TObject);
    function GetScrollDistance: Integer;
    class function GetControlClassDefaultSize: TSize; override;
    procedure InternalScroll(RightOrDown: Boolean);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure Scroll(ADelta: Integer); virtual;
    procedure ScrollButtonMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer);
    procedure ScrollButtonMouseEnterHandler(Sender: TObject);
    procedure ScrollButtonMouseLeaveHandler(Sender: TObject);
    procedure ScrollButtonMouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer);
    procedure ScrollTimerHandler(Sender: TObject);
    procedure UpdateControlZPosition;
    procedure UpdateScrollButtonSize;
    procedure UpdateScrollButtonSymbols;
    procedure UpdateScrollButtonVisibility;

    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Wrap(Enable: Boolean);

  published
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default false;
    property ButtonSize: Integer read FButtonSize write SetButtonSize stored ButtonSizeIsStored;
    property ButtonSymbol: TScrollButtonSymbol read FButtonSymbol write SetButtonSymbol default sbsDefault;
    property Control: TControl read FControl write SetControl;
    property FirstScrollInterval: Integer index 0 read GetScrollInterval write SetScrollInterval default DefaultFirstScrollInterval;
    property Flat: Boolean read GetFlat write SetFlat default false;
    property ImageIndex_Left: TImageIndex index IMG_LEFT read GetImageIndex write SetImageIndex default -1;
    property ImageIndex_Right: TImageIndex index IMG_RIGHT read GetImageIndex write SetImageIndex default -1;
    property ImageIndex_Up: TImageIndex index IMG_UP read GetImageIndex write SetImageIndex default -1;
    property ImageIndex_Down: TImageIndex index IMG_DOWN read GetImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property ImagesWidth: Integer read GetImagesWidth write SetImagesWidth;
    property Margin: Integer read GetMargin write SetMargin stored false;
    property MouseWheelMode: TScrollMouseWheelMode read FMouseWheelMode write FMouseWheelMode default mwmDefault;
    property Orientation: TPageScrollerOrientation read FOrientation write SetOrientation default soHorizontal;
    property ScrollDistance: Integer read FScrollDistance write FScrollDistance default 0;
    property ScrollInterval: Integer index 1 read GetScrollInterval write SetScrollInterval default DefaultScrollInterval;
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

{ TLazPageScroller.TScrollButton }

constructor TLazPageScroller.TScrollButton.Create(AOwner: TComponent);
begin
  inherited;
  FSpeedButton := TSpeedButton.Create(Self);
  FSpeedButton.Parent := self;
  FSpeedButton.Align := alClient;
end;

function TLazPageScroller.TScrollButton.MouseOver: Boolean;
begin
  Result := PtInRect(Rect(0, 0, Width, Height), ScreenToClient(Mouse.CursorPos));
end;


{ TLazPageScroller }

constructor TLazPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque]
    + [csAcceptsControls, csClickEvents, csNoFocus, csParentBackground];
//       csAutoSizeKeepChildLeft, csAutoSizeKeepChildTop];

  FButtonSize := DefaultBtnSize;
  FMouseWheelMode := mwmDefault;  // Rotate mouse wheel to scroll the embedded control
  FillChar(FImageIndex, SizeOf(FImageIndex), -1);

  FScrollTimer := TTimer.Create(self);
  FScrollTimer.Enabled := false;
  FScrollTimer.Interval := DefaultScrollInterval;
  FScrollTimer.OnTimer := @ScrollTimerHandler;

  FFirstScrollTimer := TTimer.Create(Self);
  FFirstScrollTimer.Enabled := false;
  FFirstScrollTimer.Interval := DefaultFirstScrollInterval;
  FFirstScrollTimer.OnTimer := @FirstScrollTimerHandler;

  FWrappingPanel := TPanel.Create(self);
  FWrappingPanel.Parent := self;
  FWrappingPanel.Caption := '';
  FWrappingPanel.BevelOuter := bvNone;
  FWrappingPanel.Align := alClient;
  FWrappingPanel.Visible := false;

  FScrollBtn[SCROLL_LEFT_OR_UP] := TScrollButton.Create(self);
  with FScrollBtn[SCROLL_LEFT_OR_UP] do
  begin
    Parent := self;
    Width := FButtonSize * 2;
    Align := alLeft;
    SpeedButton.Caption := '<';
    SpeedButton.Spacing := 0;
    SpeedButton.Tag := SCROLL_LEFT_OR_UP;
    SpeedButton.OnMouseEnter := @ScrollButtonMouseEnterHandler;
    SpeedButton.OnMouseLeave := @ScrollButtonMouseLeaveHandler;
    SpeedButton.OnMouseDown := @ScrollButtonMouseDownHandler;
    SpeedButton.OnMouseUp := @ScrollButtonMouseUpHandler;
  end;

  FScrollBtn[SCROLL_RIGHT_OR_DOWN] := TScrollButton.Create(self);
  with FScrollBtn[SCROLL_RIGHT_OR_DOWN] do
  begin
    Parent := self;
    Width := FButtonSize;
    Align := alRight;
    SpeedButton.Caption := '>';
    SpeedButton.Spacing := 0;
    SpeedButton.Tag := SCROLL_RIGHT_OR_DOWN;
    SpeedButton.OnMouseEnter := @ScrollButtonMouseEnterHandler;
    SpeedButton.OnMouseLeave := @ScrollButtonMouseLeaveHandler;
    SpeedButton.OnMouseDown := @ScrollButtonMouseDownHandler;
    SpeedButton.OnMouseUp := @ScrollButtonMouseUpHandler;
  end;

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
  inherited;
  if Assigned(FControl) then
  begin
    FControl.GetPreferredSize(PreferredWidth, PreferredHeight, false, WithThemeSpace);
    //inc(PreferredWidth, 2*Margin);
    //PreferredWidth := 0;
    //inc(PreferredHeight, 2*Margin);
  end;
end;

procedure TLazPageScroller.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;

  if not Assigned(FControl) then
    exit;

  if Orientation = soHorizontal then
  begin
    if IsRightToLeft then
    begin
      FControl.Anchors := [akTop, akRight];
      FControl.Left := ClientWidth - FControl.Width - Margin;
    end else
    begin
      FControl.Anchors := [akLeft, akTop];
      FControl.Left := Margin;
    end;
  end else
  begin
    FControl.Anchors := [akLeft, akTop];
    FControl.Left := Margin;
  end;

  UpdateScrollButtonSymbols;
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

procedure TLazPageScroller.FirstScrollTimerHandler(Sender: TObject);
begin
  FFirstScrollTimer.Enabled := false;
  FScrollTimer.Enabled := true;
end;

function TLazPageScroller.GetScrollInterval(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FFirstScrollTimer.Interval;
    1: Result := FScrollTimer.Interval;
  end;
end;

class function TLazPageScroller.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 30;
end;

function TLazPageScroller.GetFlat: Boolean;
begin
  Result := FScrollBtn[SCROLL_LEFT_OR_UP].FSpeedButton.Flat;
end;

function TLazPageScroller.GetImageIndex(AIndex: Integer): TImageIndex;
begin
  Assert(AIndex in [IMG_LEFT..IMG_DOWN]);
  Result := FImageIndex[AIndex];
end;

function TLazPageScroller.GetImagesWidth: Integer;
begin
  if Assigned(FScrollBtn[SCROLL_LEFT_OR_UP]) then
    Result := FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageWidth
  else
    Result := 0;
end;

function TLazPageScroller.GetMargin: Integer;
begin
  Result := 0; //BorderSpacing.InnerBorder;
end;

{ Returns the distance to be scrolled (in pixels) when a scroll button is clicked.
  This normally is the value of the ScrollDistance property, but when that is
  0 (or negative) scrolling goes by a full page (width/height of the scroller). }
function TLazPageScroller.GetScrollDistance: Integer;
begin
  if FScrollDistance <= 0 then
    case FOrientation of
      soHorizontal: Result := ClientWidth;
      soVertical: Result := ClientHeight;
    end
  else
    Result := FScrollDistance;
end;

procedure TLazPageScroller.InternalScroll(RightOrDown: Boolean);
var
  dist: Integer;
begin
  dist := GetScrollDistance;
  if RightOrDown then dist := -dist;
  Scroll(dist);
end;

procedure TLazPageScroller.Loaded;
begin
  inherited;
  if (FControl = nil) and (ControlCount > 2) then
    SetControl(Controls[2]);
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
    if (AComponent = FImages) then
      SetImages(nil)
    else
    if (AComponent = FScrollBtn[SCROLL_LEFT_OR_UP]) then
      FScrollBtn[SCROLL_LEFT_OR_UP] := nil
    else
    if (AComponent = FScrollBtn[SCROLL_RIGHT_OR_DOWN]) then
      FScrollBtn[SCROLL_RIGHT_OR_DOWN] := nil;
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
        begin
          if IsRightToLeft then
            // In RTL, p is the position of the control's right side, measured from the right, increasing to the left
            p := ClientWidth - (FControl.Left + FControl.Width + ADelta)
          else
            // In LTR, p is the position of the control's left side, measured from the left, increasing to the right
            p := FControl.Left + Margin + ADelta;
          if p + FControl.Width < ClientWidth - Margin then
          begin
            p := ClientWidth - Margin - FControl.Width;
            FScrollTimer.Enabled := false;
          end;
          if p > Margin then
          begin
            p := Margin;
            FScrollTimer.Enabled := false;
          end;
          if IsRightToLeft then
            FControl.Left := ClientWidth - (p + FControl.Width)
          else
            FControl.Left := p;
        end;
      soVertical:
        begin
          // p is the position of the control's top side
          p := FControl.Top + Margin + ADelta;
          if p + FControl.Height < ClientHeight - Margin then
          begin
            p := ClientHeight - Margin - FControl.Height;
            FScrollTimer.Enabled := false;
          end;
          if p > Margin then
          begin
            p := Margin;
            FScrollTimer.Enabled := false;
          end;
          FControl.Top := p;
          FControl.Left := Margin;
        end;
    end;
    UpdateScrollButtonVisibility;
  end;
end;

{ When the left mouse button is pressed the embedded control scrolls by 1 unit
  and the "FirstScrollTimer" is triggered. After this interval is expired the
  normal ScrollTimer is triggered which scrolls the control after each interval. }
procedure TLazPageScroller.ScrollButtonMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  if Button <> mbLeft then
    exit;
  if Sender = FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton then
    FScrolling := SCROLL_LEFT_OR_UP
  else if Sender = FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton then
    FScrolling := SCROLL_RIGHT_OR_DOWN
  else
    exit;
  InternalScroll(FScrolling = SCROLL_RIGHT_OR_DOWN);
  FFirstScrollTimer.Enabled := true;
end;

{ When the mouse enters the scrollbutton and AutoScroll is active the
  scroll timer is trigger. After each interval the control then scrolls by
  one unit distance. }
procedure TLazPageScroller.ScrollButtonMouseEnterHandler(Sender: TObject);
begin
  if FAutoScroll then
  begin
    FScrolling := TSpeedButton(Sender).Tag;
    FScrollTimer.Enabled := true;
  end;
end;

{ When the mouse leaves the scrollbuton and AutoScroll is active the
  scroll timer is stopped. }
procedure TLazPageScroller.ScrollButtonMouseLeaveHandler(Sender: TObject);
begin
  if FAutoScroll then
    FScrollTimer.Enabled := false;
  UpdateScrollButtonVisibility;
end;

{ When the mouse button is released all timer-based scrolls are stopped. }
procedure TLazPageScroller.ScrollButtonMouseUpHandler(Sender: TObject;
  Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  FScrollTimer.Enabled := false;
  FFirstScrollTimer.Enabled := false;
  FScrolling := 0;
end;

{ Handler for the scroll timer which fires when AutoScroll is true and the
  mouse hovers over one button. The scroll direction depends on the value
  of FScrolling which was set at MouseDown or MouseEnter. }
procedure TLazPageScroller.ScrollTimerHandler(Sender: TObject);
begin
  InternalScroll(FScrolling = SCROLL_RIGHT_OR_DOWN);
end;

{ Defines the interval length of the ScrollTimer or the FirstScrollTimer }
procedure TLazPageScroller.SetScrollInterval(AIndex, AValue: Integer);
begin
  if AValue >= 0 then
    case AIndex of
      0: if AValue <> FFirstScrollTimer.Interval then
           FFirstScrollTimer.Interval := AValue;
      1: if AValue <> FScrollTimer.Interval then
           FScrollTimer.Interval := AValue;
    end;
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

  IMPORTANT: The Align property of the control must be alCustom for
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
      FControl.Parent := Self;
      FControl.Align := alCustom;   // Important: No scrolling if Align=alNone
      if (FOrientation = soHorizontal) and IsRightToLeft then
        FControl.Left := ClientWidth - Margin - FControl.Width
      else
        FControl.Left := Margin;
      FControl.Top := Margin;
      UpdateControlZPosition;
    end else
      FControlParent := nil;

    UpdateScrollButtonVisibility;
  end;
end;

procedure TLazPageScroller.SetFlat(AValue: boolean);
begin
  FScrollBtn[SCROLL_LEFT_OR_UP].FSpeedButton.Flat := AValue;
  FScrollBtn[SCROLL_RIGHT_OR_DOWN].FSpeedButton.Flat := AValue;
end;

procedure TLazPageScroller.SetImageIndex(AIndex: Integer;
  AValue: TImageIndex);
var
  btnIndex: Integer;
begin
  Assert(AIndex in [IMG_LEFT..IMG_DOWN]);
  FImageIndex[AIndex] := AValue;

  if AIndex in [IMG_LEFT, IMG_RIGHT] then
    btnIndex := AIndex
  else
  if AIndex in [IMG_UP, IMG_DOWN] then
    btnIndex := AIndex - 2;

  if Assigned(FScrollBtn[btnIndex]) and (AValue <> FScrollBtn[btnIndex].SpeedButton.ImageIndex) then
  begin
    FScrollBtn[btnIndex].SpeedButton.ImageIndex := AValue;
    UpdateScrollButtonSymbols;
  end;
end;

procedure TLazPageScroller.SetImages(AValue: TCustomImageList);
begin
  if AValue <> FImages then
  begin
    FImages := AValue;
    UpdateScrollButtonSymbols;
  end;
end;

procedure TLazPageScroller.SetImagesWidth(AValue: Integer);
begin
  if AValue <> GetImagesWidth then
  begin
    FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageWidth := AValue;
    FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.ImageWidth := AValue;
  end;
end;

procedure TLazPageScroller.SetMargin(AValue: Integer);
begin
  DisableAutoSizing;
  BorderSpacing.InnerBorder := AValue;
  {
  FMargin := AValue;
  if Assigned(FControl) then
    case FOrientation of
      soHorizontal: FControl.Top := FMargin;
      soVertical: FControl.Left := FMargin;
    end;
    }
  EnableAutoSizing;
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
        FScrollBtn[SCROLL_LEFT_OR_UP].Align := alLeft;
        FScrollBtn[SCROLL_RIGHT_OR_DOWN].Align := alRight;
        FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageIndex := FImageIndex[IMG_LEFT];
        FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.ImageIndex := FImageIndex[IMG_RIGHT];
      end;
    soVertical:
      begin
        FScrollBtn[SCROLL_LEFT_OR_UP].Align := alTop;
        FScrollBtn[SCROLL_RIGHT_OR_DOWN].Align := alBottom;
        FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageIndex := FImageIndex[IMG_UP];
        FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.ImageIndex := FImageIndex[IMG_DOWN];
      end;
  end;
  UpdateScrollButtonSize;
  UpdateScrollButtonSymbols;
  DoChangeOrientation;
  if Assigned(FControl) then
  begin
    FControl.Top := Margin;
    if (FOrientation = soHorizontal) and IsRightToLeft then
      FControl.Left := ClientWidth - Margin - FControl.Width
    else
      FControl.Left := Margin;
  end;
end;

{ Moves the scroll buttons above the control so that they are not covered by it. }
procedure TLazPageScroller.UpdateControlZPosition;
begin
  SetControlIndex(FScrollBtn[SCROLL_LEFT_OR_UP], 999);
  SetControlIndex(FScrollBtn[SCROLL_RIGHT_OR_DOWN], 999);
end;

procedure TLazPageScroller.UpdateScrollButtonSize;
begin
  case FOrientation of
    soHorizontal:
      begin
        FScrollBtn[SCROLL_LEFT_OR_UP].Width := FButtonSize;
        FScrollBtn[SCROLL_RIGHT_OR_DOWN].Width := FButtonSize;
      end;
    soVertical:
      begin
        FScrollBtn[SCROLL_LEFT_OR_UP].Height := FButtonSize;
        FScrolLBtn[SCROLL_RIGHT_OR_DOWN].Height := FButtonSize;
      end;
  end;
end;

procedure TLazPageScroller.UpdateScrollButtonSymbols;

  procedure SetSymbols(ALeft, ARight, AUp, ADown: String);
  var
    FixUTF8: String = '';
  begin
    // UTF8 code points are not drawn correctly as TBitBtn caption. We
    // fix this by appending a space character.
    if not (FButtonSymbol in [sbsDefault, sbsImage]) then
      FixUTF8 := ' ';
    case FOrientation of
      soHorizontal:
        begin
          if IsRightToLeft then
          begin
            FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.Caption := ARight + FixUTF8;
            FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.Caption := ALeft + FixUTF8;
          end else
          begin
            FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.Caption := ALeft + FixUTF8;
            FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.Caption := ARight + FixUTF8;
          end;
          FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageIndex := FImageIndex[IMG_LEFT];
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.ImageIndex := FImageIndex[IMG_RIGHT];
        end;
      soVertical:
        begin
          FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.Caption := AUp + FixUTF8;
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.Caption := ADown + FixUTF8;
          FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.ImageIndex := FImageIndex[IMG_UP];
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.ImageIndex := FImageIndex[IMG_DOWN];
        end;
    end;
    if FButtonSymbol = sbsImage then
    begin
      FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.Images := FImages;
      FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.Images := FImages;
    end else
    begin
      FScrollBtn[SCROLL_LEFT_OR_UP].SpeedButton.Images := nil;
      FScrollBtn[SCROLL_RIGHT_OR_DOWN].SpeedButton.Images := nil;
    end;
  end;

begin
  case FButtonSymbol of
    sbsDefault:
      SetSymbols('<', '>', '^', 'v');      // ASCII
    sbsSmallFilled:
      SetSymbols(#$E2#$97#$82, #$E2#$96#$B8, #$E2#$96#$B4, #$E2#$96#$BE);  // "Geometric shapes" UTF8 range
    sbsSmallOpen:
      SetSymbols(#$E2#$97#$83, #$E2#$96#$B9, #$E2#$96#$B5, #$E2#$96#$BF);  // "Geometric shapes" UTF8 range
//    sbsMedFilled: SetSymbols(#$E2#$AF#$87, #$E2#$AF#$88, #$E2#$AF#$85, #$E2#$AF#$86);      // "Misc Symbols and Arrows" UTF8 range
    sbsLargeFilled:
      SetSymbols(#$E2#$97#$80, #$E2#$96#$B6, #$E2#$96#$B2, #$E2#$96#$BC);  // "Geometric shapes" UTF8 range
    sbsLargeOpen:
      SetSymbols(#$E2#$97#$81, #$E2#$96#$B7, #$E2#$96#$B3, #$E2#$96#$BD);  // "Geometric shapes" UTF8 range
    sbsImage:
      SetSymbols('', '', '', '');
  end;
end;

procedure TLazPageScroller.UpdateScrollButtonVisibility;
var
  scrolledToStart, scrolledToEnd: Boolean;
begin
  if Assigned(FControl) then
  begin
    case FOrientation of
      soHorizontal:
        if IsRightToLeft then
        begin
          scrolledToStart := (FControl.Left + FControl.Width <= ClientWidth - Margin);
          scrolledToEnd := (FControl.Left >= Margin);
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].Visible := (not scrolledToStart) or FScrollBtn[SCROLL_RIGHT_OR_DOWN].MouseOver;
          FScrollBtn[SCROLL_LEFT_OR_UP].Visible := (not scrolledToEnd) or FScrollBtn[SCROLL_LEFT_OR_UP].MouseOver;
        end else
        begin
          scrolledToStart := FControl.Left >= Margin;
          scrolledToEnd := FControl.Left + FControl.Width <= ClientWidth - Margin;
          FScrollBtn[SCROLL_LEFT_OR_UP].Visible := (not scrolledToStart) or FScrollBtn[SCROLL_LEFT_OR_UP].MouseOver;
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].Visible := (not scrolledToEnd) or FScrollBtn[SCROLL_RIGHT_OR_DOWN].MouseOver;
        end;
      soVertical:
        begin
          scrolledToStart := FControl.Top >= Margin;
          scrolledToEnd := FControl.Top + FControl.Height <= ClientHeight - Margin;
          FScrollBtn[SCROLL_LEFT_OR_UP].Visible := (not scrolledToStart) or FScrollBtn[SCROLL_LEFT_OR_UP].MouseOver;
          FScrollBtn[SCROLL_RIGHT_OR_DOWN].Visible := (not scrolledToEnd) or FScrollBtn[SCROLL_RIGHT_OR_DOWN].MouseOver;
        end;
    end
  end else
  begin
    FScrollBtn[SCROLL_LEFT_OR_UP].Visible := (csDesigning in ComponentState);
    FScrollBtn[SCROLL_RIGHT_OR_DOWN].Visible := (csDesigning in ComponentState);
  end;
end;

procedure TLazPageScroller.Wrap(Enable: Boolean);
begin
  if (FControl = nil) then
    exit;

  // The component does not like changing the Parent in design mode
  if (csDesigning in ComponentState) then
    exit;

  FWrappingPanel.Visible := Enable;
  if Enable then
  begin
    FControl.Parent := FWrappingPanel;
    FControl.Align := alClient;
  end else
  begin
    FControl.Parent := Self;
    FControl.Align := alCustom;
    UpdateControlZPosition;
  end;
end;


{ TToolbarHelper}

procedure TToolbarHelper.SetOrientation(AValue: TPageScrollerOrientation);
var
  w, h: Integer;
begin
  Align := alCustom;
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

