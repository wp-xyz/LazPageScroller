unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, Forms,
  Controls, Graphics, Dialogs, PgScroller;

type
  TForm1 = class(TForm)
    cbBackground: TCheckBox;
    cbWrapped: TCheckBox;
    cbAutoScroll: TCheckBox;
    ImageList1:TImageList;
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
    procedure cbAutoScrollChange(Sender: TObject);
    procedure cbBackgroundChange(Sender: TObject);
    procedure cbWrappedChange(Sender: TObject);
    procedure FormCreate(Sender:TObject);
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
  FPageScroller.Height := 2*Toolbar1.Height;
  FPageScroller.AutoSize := true;
  FPageScroller.Align := alTop;
  FPageScroller.Control := Toolbar1;
  FPageScroller.BorderWidth := 2;
  FPageScroller.ScrollDistance := Toolbar1.ButtonWidth;
end;

procedure TForm1.cbWrappedChange(Sender: TObject);
begin
  if cbWrapped.Checked then
  begin
    Toolbar1.Align := alClient;
  end else
  begin
    Toolbar1.Align := alNone;
  end;
end;

procedure TForm1.cbBackgroundChange(Sender: TObject);
begin
  if cbBackground.Checked then
    FPageScroller.Color := clSilver
  else
    FPageScroller.Color := clNone;
end;

procedure TForm1.cbAutoScrollChange(Sender: TObject);
begin
  FPageScroller.AutoScroll := cbAutoScroll.Checked;
end;

end.

