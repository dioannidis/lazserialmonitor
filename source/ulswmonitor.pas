unit uLSWMonitor;

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
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics,
  Dialogs, LCLType, StdCtrls, ComCtrls, LazSerial, IDECommands, IDEWindowIntf,
  LazIDEIntf, MenuIntf, LazSynaSer;

type

  { TSerialMonitor }

  TSerialMonitor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LazSerial1: TLazSerial;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LazSerial1RxData(Sender: TObject);
    procedure LazSerial1Status(Sender: TObject; Reason: THookSerialReason;
      const Value: string);
  private
    { private declarations }
  public
    { public declarations }
    procedure SerialActive(const AActive: boolean);
  end;

var
  SerialMonitor: TSerialMonitor;
  SerialMonitorCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowSerialMonitor(Sender: TObject);
procedure Register;
// Check the "Register Unit" of this unit in the package editor.implementation

implementation

{$R *.lfm}

procedure ShowSerialMonitor(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(SerialMonitorCreator.FormName, True);
end;

procedure CreateSerialMonitor(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'SerialMonitor') <> 0 then
  begin
    DebugLn(['ERROR: CreateSerialMonitor: there is already a form with this '
      + 'name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TSerialMonitor, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name := aFormName;
  SerialMonitor := AForm as TSerialMonitor;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewSerialMonitorCommand: TIDECommand;
  MenuItemCaption: string;
begin
  // register shortcut and menu item
  MenuItemCaption := 'Serial Monitor';
  // <- this caption should be replaced by a resourcestring
  // search shortcut category
  CmdCatViewMenu := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewSerialMonitorCommand := RegisterIDECommand(CmdCatViewMenu,
    'ViewSerialMonitor', MenuItemCaption, IDEShortCut(VK_UNKNOWN, []),
    // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowSerialMonitor);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewSerialMonitor',
    MenuItemCaption, nil, nil, ViewSerialMonitorCommand);

  // register dockable Window
  SerialMonitorCreator := IDEWindowCreators.Add('SerialMonitor',
    @CreateSerialMonitor, nil, '100', '100', '300',
    '300'  // default place at left=100, top=100, right=300, bottom=300
    // you can also define percentage values of screen or relative positions, see wiki
    );
end;

{ TSerialMonitor }

procedure TSerialMonitor.LazSerial1RxData(Sender: TObject);
begin
  memo1.Append(LazSerial1.ReadData);
end;

procedure TSerialMonitor.LazSerial1Status(Sender: TObject;
  Reason: THookSerialReason; const Value: string);
begin
  StatusBar1.SimpleText := Value;
end;

procedure TSerialMonitor.Button1Click(Sender: TObject);
begin
  LazSerial1.ShowSetupDialog;
end;

procedure TSerialMonitor.Button2Click(Sender: TObject);
begin
  SerialActive(True);
end;

procedure TSerialMonitor.Button3Click(Sender: TObject);
begin
  SerialActive(False);
end;

procedure TSerialMonitor.FormHide(Sender: TObject);
begin
  SerialActive(False);
  //LazSerial1.Active := False;
end;

procedure TSerialMonitor.FormShow(Sender: TObject);
begin
  //LazSerial1.Active := True;
end;

procedure TSerialMonitor.SerialActive(const AActive: boolean);
begin
  if Self.Showing then
    if AActive then
    begin
      LazSerial1.Open;
      Button2.Enabled := False;
      Button3.Enabled := True;
    end
    else
    begin
      LazSerial1.Close;
      Button2.Enabled := True;
      Button3.Enabled := False;
    end;
end;

end.
