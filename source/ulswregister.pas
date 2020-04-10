unit uLSWRegister;

{

  Serial Monitor Lazarus package.

  Copyright (C) 2020 Dimitrios Chr. Ioannidis.
    Nephelae - https://www.nephelae.eu

  Licensed under the MIT License (MIT).
  See licence file in root directory.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
  ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
  TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
  SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  LazIDEIntf, uLSWMain;

var
  LSW: TLSW;

procedure Register;
begin
  // Run ( without or with debugger ) hooks
  LazarusIDE.AddHandlerOnRunDebug(@LSW.RunHandler);
  LazarusIDE.AddHandlerOnRunWithoutDebugInit(@LSW.RunNoDebugHandler);
  LazarusIDE.AddHandlerOnRunFinished(@LSW.StopHandler, True);
end;

initialization
  LSW := TLSW.Create;

finalization
  FreeAndNil(LSW);

end.

