unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms,
  Controls, Graphics, Dialogs, PgScroller;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    FontDialog1: TFontDialog;
    ImageList1:TImageList;
    ArrowImages: TImageList;
    Label1: TLabel;
    Panel1: TPanel;
    rgScrollMouseWheel: TRadioGroup;
    rgAlign: TRadioGroup;
    rgScrollBtnSymbols: TRadioGroup;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure rgScrollMouseWheelClick(Sender: TObject);
    procedure rgAlignClick(Sender: TObject);
    procedure rgScrollBtnSymbolsClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
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

  for i := 0 to ImageList1.Count-1 do
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
//  FPageScroller.Color := clRed;
  FPageScroller.Top := 0;
  FPageScroller.Height := 40;  // Just for testing AutoSize...
  FPageScroller.Align := alTop;
  FPageScroller.Control := Panel1;
  FPageScroller.AutoSize := true;
  FPageScroller.BorderSpacing.Around := 2;
  FPageScroller.ScrollDistance := delta;
  FPageScroller.OnChangeOrientation := @ChangeOrientationHandler;

  SpinEdit1.Value := FPageScroller.BtnSize;
end;

procedure TForm1.rgScrollMouseWheelClick(Sender: TObject);
begin
  FPageScroller.ScrollMouseWheel := TScrollMouseWheel(rgScrollMouseWheel.ItemIndex);
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
      end;
  end;
end;

procedure TForm1.rgScrollBtnSymbolsClick(Sender: TObject);
begin
  if rgScrollBtnSymbols.ItemIndex = rgScrollBtnSymbols.Items.Count-1 then
  begin
    FPageScroller.Images := ArrowImages;
    ChangeOrientationHandler(FPageScroller);
  end else
  begin
    FPageScroller.Images := nil;
    FPageScroller.BtnSymbol := TScrollBtnSymbol(rgScrollBtnSymbols.ItemIndex);
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  FPageScroller.BtnSize := SpinEdit1.Value;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  FPageScroller.Flat := Checkbox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(FPageScroller.Font);
  if FontDialog1.Execute then
    FPageScroller.Font.Assign(FontDialog1.Font);
end;

end.

