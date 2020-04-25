{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazSerialWindow;

{$warn 5023 off : no warning about unused units}
interface

uses
  uLSWMain, uLSWMonitor, uLSWRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uLSWMonitor', @uLSWMonitor.Register);
  RegisterUnit('uLSWRegister', @uLSWRegister.Register);
end;

initialization
  RegisterPackage('lazSerialWindow', @Register);
end.
