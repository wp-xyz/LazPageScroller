{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PgScrollerReg;

{$warn 5023 off : no warning about unused units}
interface

uses
  Classes, PgScroller;

procedure Register;

implementation

{$R pagescroller_icons.res}

procedure Register;
begin
  RegisterComponents('LazControls', [TLazPageScroller]);
end;

end.
