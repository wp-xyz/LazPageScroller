{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPageScroller_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  pgScrollerReg, PgScroller, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pgScrollerReg', @pgScrollerReg.Register);
end;

initialization
  RegisterPackage('LazPageScroller_pkg', @Register);
end.
