unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms,
  Controls, Graphics, Dialogs, PgScroller;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    ImageList1:TImageList;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SpinEdit1: TSpinEdit;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    FPageScroller: TLazPageScroller;

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
  FPageScroller.Align := alTop;
  FPageScroller.Control := Edit1;
  FPageScroller.BorderWidth := 2;
  FPageScroller.AutoSize := true;
  RadioGroup1Click(nil);

  SpinEdit1.Value := FPageScroller.BtnSize;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case Radiogroup1.ItemIndex of
    0: FPageScroller.ScrollDistance := 0;
    1: FPageScroller.ScrollDistance := FPageScroller.Width div 2;
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

end.

