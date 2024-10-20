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
    ImageList1:TImageList;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SpinEdit1: TSpinEdit;
    ToolBar1:TToolBar;
    ToolButton1:TToolButton;
    ToolButton10:TToolButton;
    ToolButton11:TToolButton;
    ToolButton12:TToolButton;
    ToolButton13:TToolButton;
    ToolButton14:TToolButton;
    ToolButton15:TToolButton;
    ToolButton16:TToolButton;
    ToolButton17:TToolButton;
    ToolButton18:TToolButton;
    ToolButton19:TToolButton;
    ToolButton2:TToolButton;
    ToolButton20:TToolButton;
    ToolButton3:TToolButton;
    ToolButton4:TToolButton;
    ToolButton5:TToolButton;
    ToolButton6:TToolButton;
    ToolButton7:TToolButton;
    ToolButton8:TToolButton;
    ToolButton9:TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
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
begin
  Width := 360;

  FPageScroller := TLazPageScroller.Create(self);
  FPageScroller.Parent := self;
  FPageScroller.Top := 0;
  FPageScroller.Height := 40;  // Just for testing AutoSize...
  FPageScroller.Align := alTop;
  FPageScroller.Control := Toolbar1;
  FPageScroller.BorderWidth := 2;
  FPageScroller.ScrollDistance := Toolbar1.ButtonWidth;
  FPageScroller.OnChangeOrientation := @ChangeOrientationHandler;

  SpinEdit1.Value := FPageScroller.BtnSize;
end;

procedure TForm1.ChangeOrientationHandler(Sender: TObject);
var
  scroller: TLazPageScroller;
begin
  scroller := Sender as TLazPageScroller;
  Toolbar1.SetOrientation(scroller.Orientation);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case Radiogroup1.ItemIndex of
    0: FPageScroller.ScrollDistance := 0;
    1: FPageScroller.ScrollDistance := Toolbar1.ButtonWidth;
    2: FPageScroller.ScrollDistance := 1;
  end;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  case RadioGroup2.ItemIndex of
    0: FPageScroller.Align := alTop;
    1: FPageScroller.Align := alBottom;
  end;
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);
begin
  if RadioGroup3.ItemIndex = Radiogroup3.Items.Count-1 then
  begin
    FPageScroller.Images := ImageList1;
    FPageScroller.ImageIndexDown := 21;
    FPageScroller.ImageIndexUp := 22;
  end else
  begin
    FPageScroller.Images := nil;
    FPageScroller.BtnSymbol := TScrollBtnSymbol(RadioGroup3.ItemIndex);
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
  FPageScroller.AutoSize := true;
end;

end.

