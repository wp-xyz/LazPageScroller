unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms,
  Controls, Graphics, Dialogs, PgScroller;

type
  TForm1 = class(TForm)
    btnFont: TButton;
    cbRTL: TCheckBox;
    cbAutoScroll: TCheckBox;
    FontDialog1: TFontDialog;
    ImageList1:TImageList;
    ArrowImages: TImageList;
    lblButtonSize: TLabel;
    Panel1: TPanel;
    rgMouseWheelMode: TRadioGroup;
    rgAlign: TRadioGroup;
    rgScrollBtnSymbols: TRadioGroup;
    seButtonSize: TSpinEdit;
    procedure btnFontClick(Sender: TObject);
    procedure cbAutoScrollChange(Sender: TObject);
    procedure cbRTLChange(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure rgMouseWheelModeClick(Sender: TObject);
    procedure rgAlignClick(Sender: TObject);
    procedure rgScrollBtnSymbolsClick(Sender: TObject);
    procedure seButtonSizeChange(Sender: TObject);
  private
    FPageScroller: TLazPageScroller;
    procedure ChangeOrientationHandler(Sender: TObject);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender:TObject);
var
  i, delta: Integer;
begin
  Width := 380;

  Panel1.Caption := '';
  Panel1.ParentColor := true;
  Panel1.ChildSizing.ControlsPerLine := 9999;
  Panel1.ChildSizing.Layout := cclLeftToRightThenTopToBottom;

  for i := 0 to 19 do
    with TSpeedButton.Create(self) do
    begin
      Width := 30;
      GroupIndex := 1;
      Images := ImageList1;
      ImageIndex := i;
      Parent := Panel1;
    end;
  Panel1.AutoSize := true;
  if Panel1.ControlCount > 0 then
    delta := Panel1.Controls[0].Width + Panel1.ChildSizing.HorizontalSpacing + 4  // Is this "4" valid for all widgetsets?
  else
    delta := 0;

  FPageScroller := TLazPageScroller.Create(self);
  FPageScroller.Parent := self;
  FPageScroller.Color := clSilver;
  FPageScroller.Top := 0;
  FPageScroller.Align := alTop;
  FPageScroller.Control := Panel1;
  FPageScroller.AutoSize := true;
//  FPageScroller.BorderSpacing.InnerBorder := 2;
  FPageScroller.ScrollDistance := delta;
  FPageScroller.Images := ArrowImages;
  FPageScroller.ImageIndexDown := 0;
  FPageScroller.ImageIndexUp := 1;
  FPageScroller.OnChangeOrientation := @ChangeOrientationHandler;

  seButtonSize.Value := FPageScroller.ButtonSize;
end;

procedure TForm1.rgMouseWheelModeClick(Sender: TObject);
begin
  FPageScroller.MouseWheelMode := TScrollMouseWheelMode(rgMouseWheelMode.ItemIndex);
end;

{ This event handler must change the button layout in the panel from left-to-right
  in case of horizontal, and from top-to-bottom in case of vertical orientation.
  It also must assign the correct scroll button icons if they are used from
  an imagelist. }
procedure TForm1.ChangeOrientationHandler(Sender: TObject);
var
  scroller: TLazPageScroller;
begin
  scroller := Sender as TLazPageScroller;
  case scroller.Orientation of
    soHorizontal:
      begin
        Panel1.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
        FPageScroller.ImageIndexDown := 0;
        FPageScroller.ImageIndexUp := 1;
      end;
    soVertical:
      begin
        Panel1.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
        FPageScroller.ImageIndexDown := 2;
        FPageScroller.ImageIndexUp := 3;
      end;
  end;
end;

{ The Align property of the PageScroller is to be changed. Besides setting
  this property, the handler must also rotate the orientation of the scroller. }
procedure TForm1.rgAlignClick(Sender: TObject);
begin
  case rgAlign.ItemIndex of
    0, 1:
      begin
        FPageScroller.Orientation := soHorizontal;
        case rgAlign.ItemIndex of
          0: FPageScroller.Align := alTop;
          1: FPageScroller.Align := alBottom;
        end;
        Panel1.Left := 0;
        Panel1.Top := 0;
        FPageScroller.ImageIndexDown := 0;
        FPageScroller.ImageIndexUp := 1;
      end;
    2, 3:
      begin
        FPageScroller.Orientation := soVertical;
        case rgAlign.ItemIndex of
          2: FPageScroller.Align := alLeft;
          3: FPageScroller.Align := alRight;
        end;
        Panel1.Left := 0;
        Panel1.Top := 0;
        FPageScroller.ImageIndexDown := 2;
        FPageScroller.ImageIndexUp := 3;
      end;
  end;
end;

procedure TForm1.rgScrollBtnSymbolsClick(Sender: TObject);
begin
  FPageScroller.ButtonSymbol := TScrollButtonSymbol(rgScrollBtnSymbols.ItemIndex);
end;

procedure TForm1.seButtonSizeChange(Sender: TObject);
begin
  FPageScroller.ButtonSize := seButtonSize.Value;
end;

procedure TForm1.cbRTLChange(Sender: TObject);
begin
  if cbRTL.Checked then
    FPageScroller.BiDiMode := bdRightToLeft
  else
    FPageScroller.BiDiMode := bdLeftToRight;
end;

procedure TForm1.btnFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(FPageScroller.Font);
  if FontDialog1.Execute then
    FPageScroller.Font.Assign(FontDialog1.Font);
end;

procedure TForm1.cbAutoScrollChange(Sender: TObject);
begin
  FPageScroller.AutoScroll := cbAutoScroll.Checked;
end;

end.

