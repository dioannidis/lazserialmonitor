{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazSerialWindow;

{$warn 5023 off : no warning about unused units}
interface

uses
  uLSWMain, uLSWRegister, uLSWMonitor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uLSWRegister', @uLSWRegister.Register);
  RegisterUnit('uLSWMonitor', @uLSWMonitor.Register);
end;

initialization
  RegisterPackage('lazSerialWindow', @Register);
end.
