unit uLSWMain;

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
  Classes, SysUtils, UITypes;

type

  { TLSW }

  TLSW = class(TObject)
  private
    procedure DoOpen;
    procedure DoClose;
  public
    function RunHandler(Sender: TObject; var Handled: boolean): TModalResult;
    function RunNoDebugHandler(Sender: TObject; var Handled: boolean): TModalResult;
    procedure StopHandler(Sender: TObject);
  end;

implementation

uses
  uLSWMonitor;

{ TLSW }

function TLSW.RunHandler(Sender: TObject; var Handled: boolean): TModalResult;
begin
  DoClose;
end;

function TLSW.RunNoDebugHandler(Sender: TObject; var Handled: boolean): TModalResult;
begin
  DoClose;
end;

procedure TLSW.StopHandler(Sender: TObject);
begin
  DoOpen;
end;

procedure TLSW.DoOpen;
begin
  if Assigned(SerialMonitor) then
    SerialMonitor.RequestActivateMonitor(True);
end;

procedure TLSW.DoClose;
begin
  if Assigned(SerialMonitor) then
    SerialMonitor.RequestActivateMonitor(False);
end;

end.
