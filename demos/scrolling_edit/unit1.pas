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
    lblBtnSize: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    rgScrollBtnSymbols: TRadioGroup;
    seBtnSize: TSpinEdit;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure rgScrollBtnSymbolsClick(Sender: TObject);
    procedure seBtnSizeChange(Sender: TObject);
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

  seBtnSize.Value := FPageScroller.ButtonSize;
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

procedure TForm1.rgScrollBtnSymbolsClick(Sender: TObject);
begin
  if rgScrollBtnSymbols.ItemIndex = rgScrollBtnSymbols.Items.Count-1 then
  begin
    FPageScroller.Images := ImageList1;
    FPageScroller.ImageIndex_LeftOrUp := 21;
    FPageScroller.ImageIndex_RightOrDown := 22;
  end else
  begin
    FPageScroller.Images := nil;
    FPageScroller.ButtonSymbol := TScrollButtonSymbol(rgScrollBtnSymbols.ItemIndex);
  end;
end;

procedure TForm1.seBtnSizeChange(Sender: TObject);
begin
  FPageScroller.ButtonSize := seBtnSize.Value;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
//  FPageScroller.Flat := Checkbox1.Checked;
end;

end.

