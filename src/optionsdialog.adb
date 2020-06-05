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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews.Commands; use ArchivesViews.Commands;
with Utils; use Utils;

package body OptionsDialog is

   procedure ShowOptions is
      OptionsDialog: constant Tk_Toplevel :=
        Create(".optionsdialog", "-class Dialog");
      ButtonBox: constant Ttk_Frame := Create(".optionsdialog.buttonbox");
      Button: Ttk_Button;
      Label: constant Ttk_Label :=
        Create
          (".optionsdialog.label",
           "-text {Directory suggested for archive extraction (if empty: archive's location)}");
      DirectoryFrame: constant Ttk_LabelFrame :=
        Create
          (".optionsdialog.directoryframe",
           "-labelwidget " & Widget_Image(Label));
      DirectoryEntry: constant Ttk_Entry :=
        Create(".optionsdialog.directoryframe.entry");
      Width: constant Positive :=
        Positive'Value(Winfo_Get(Label, "reqwidth")) + 20;
      Height: Positive :=
        Positive'Value(Winfo_Get(Label, "reqheight")) +
        Positive'Value(Winfo_Get(DirectoryEntry, "reqheight"));
   begin
      Tcl.Tk.Ada.Pack.Pack(DirectoryEntry, "-expand true -fill x -side left");
      Button :=
        Create
          (".optionsdialog.directoryframe.button",
           "-text Choose -command SelectDirectory");
      Tcl.Tk.Ada.Pack.Pack(Button);
      Tcl.Tk.Ada.Grid.Grid(DirectoryFrame, "-sticky we");
      Button :=
        Create(".optionsdialog.buttonbox.ok", "-text Ok -command SetOptions");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".optionsdialog.buttonbox.close",
           "-text Close -command {CloseDialog .optionsdialog}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid(ButtonBox, "-column 0 -row 1");
      Height := Height + Positive'Value(Winfo_Get(Button, "reqheight")) + 5;
      SetDialog(OptionsDialog, "AZip - General Options", Width, Height);
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

   function Select_Directory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Select_Directory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SelectedDirectory: constant String :=
        Choose_Directory
          ("-parent . -title {Choose extract directory} -mustexist true");
      DirectoryEntry: Ttk_Entry;
   begin
      if SelectedDirectory = "" then
         return TCL_OK;
      end if;
      DirectoryEntry.Interp := Interp;
      DirectoryEntry.Name := New_String(".optionsdialog.directoryframe.entry");
      Delete(DirectoryEntry, "0", "end");
      Insert(DirectoryEntry, "0", SelectedDirectory);
      return TCL_OK;
   end Select_Directory_Command;

   function Set_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Set_Options_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DirectoryEntry: Ttk_Entry;
      Button: Ttk_Button;
      Azip_Options_Dialog_Error: exception;
   begin
      DirectoryEntry.Interp := Interp;
      DirectoryEntry.Name := New_String(".optionsdialog.directoryframe.entry");
      ExtractingDirectory := To_Unbounded_String(Get(DirectoryEntry));
      Button.Interp := Interp;
      Button.Name := New_String(".optionsdialog.buttonbox.close");
      if Invoke(Button) /= "0" then
         raise Azip_Options_Dialog_Error with "Can't close options dialog";
      end if;
      return TCL_OK;
   end Set_Options_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowOptions", Show_Options_Command'Access);
      AddCommand("SelectDirectory", Select_Directory_Command'Access);
      AddCommand("SetOptions", Set_Options_Command'Access);
   end AddCommands;

   procedure CreateOptions is
   begin
      AddCommands;
   end CreateOptions;

end OptionsDialog;
