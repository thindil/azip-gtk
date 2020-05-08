-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Dialogs; use Dialogs;

package body OptionsDialog is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   procedure ShowOptions is
      OptionsDialog: constant Tk_Toplevel :=
        Create(".optionsdialog", "-class Dialog");
      ButtonBox: constant Ttk_Frame := Create(".optionsdialog.buttonbox");
      Button: Ttk_Button;
      DirectoryFrame: constant Ttk_LabelFrame :=
        Create
          (".optionsdialog.directoryframe",
           "-text {Directory suggested for archive extraction (if empty: archive's location)}");
      DirectoryEntry: constant Ttk_Entry :=
        Create(".optionsdialog.directoryframe.entry");
   begin
      Tcl.Tk.Ada.Pack.Pack(DirectoryEntry, "-expand true -fill x -side left");
      Button := Create(".optionsdialog.directoryframe.button", "-text Choose");
      Tcl.Tk.Ada.Pack.Pack(Button);
      Tcl.Tk.Ada.Grid.Grid(DirectoryFrame, "-sticky we");
      Button := Create(".optionsdialog.buttonbox.ok", "-text Ok");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".optionsdialog.buttonbox.close",
           "-text Close -command {CloseDialog .optionsdialog}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid(ButtonBox, "-column 0 -row 1");
      SetDialog(OptionsDialog, "AZip - General Options", 550, 100);
   end ShowOptions;

   function Show_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowOptions;
      return TCL_OK;
   end Show_Options_Command;

   procedure AddCommands is
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise Program_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
   begin
      AddCommand("ShowOptions", Show_Options_Command'Access);
   end AddCommands;

   procedure CreateOptions is
   begin
      AddCommands;
   end CreateOptions;

end OptionsDialog;
