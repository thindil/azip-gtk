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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews; use ArchivesViews;
with Utils; use Utils;

package body ColumnsDialog is

   -- ****if* ColumnsDialog/Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog and unblock the main window
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog: Tk_Toplevel;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
   begin
      Dialog.Interp := Interp;
      Dialog.Name := New_String(CArgv.Arg(Argv, 1));
      Destroy(Dialog);
      if Winfo_Get(MainWindow, "exists") = "1"
        and then
        (Status(MainWindow) = "1" and
         CArgv.Arg(Argv, 1) /= ".creditsdialog") then
         Forget(MainWindow);
      end if;
      return TCL_OK;
   end Close_Dialog_Command;

   -- ****if* ColumnsDialog/Set_Visible_Columns_Command
   -- FUNCTION
   -- Set and show the dialog window to set visible columns in files list in
   -- archives views
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Visible_Columns_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Visible_Columns_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ColumnsDialog: constant Tk_Toplevel :=
        Create(".columnsdialog", "-class Dialog");
      CheckButton: Ttk_CheckButton;
      CloseButton: constant Ttk_Button :=
        Create
          (".columnsdialog.closebutton",
           "-text Done -command {CloseDialog .columnsdialog}");
      Tokens: Slice_Set;
      FilesView: Ttk_Tree_View;
      Width, Height: Positive := 1;
   begin
      FilesView.Interp := Interp;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create
        (Tokens, Tcl.Tk.Ada.Widgets.cget(FilesView, "-displaycolumns"), " ");
      -- Create buttons for each column in files list view and set it state
      -- based on column visibility
      for I in ColumnsNames'Range loop
         CheckButton :=
           Create
             (".columnsdialog.checkbutton" & Trim(Positive'Image(I), Left),
              "-text {" & To_String(ColumnsNames(I)) &
              "} -command SetColumns");
         for J in 1 .. Slice_Count(Tokens) loop
            if Slice(Tokens, J) = Trim(Positive'Image(I), Left) then
               Tcl_SetVar
                 (Interp,
                  ".columnsdialog.checkbutton" & Trim(Positive'Image(I), Left),
                  "1");
               exit;
            end if;
         end loop;
         -- Columns Name, Path and Result cannot be hidden manually
         if I in 1 | 10 | 12 then
            Tcl.Tk.Ada.Widgets.configure(CheckButton, "-state disabled");
         end if;
         Tcl.Tk.Ada.Pack.Pack(CheckButton, "-anchor w -padx 20");
         if Width < Positive'Value(Winfo_Get(CheckButton, "reqwidth")) then
            Width := Positive'Value(Winfo_Get(CheckButton, "reqwidth"));
         end if;
         Height :=
           Height + Positive'Value(Winfo_Get(CheckButton, "reqheight"));
      end loop;
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      SetDialog
        (ColumnsDialog, "Azip - Select displayed columns", Width + 40, Height);
      return TCL_OK;
   end Set_Visible_Columns_Command;

   -- ****if* ColumnsDialog/Set_Columns_Command
   -- FUNCTION
   -- Set visibility of the columns in files list in archives views
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Columns_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Columns_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DisplayColumns: Unbounded_String;
      FilesView: Ttk_Tree_View;
   begin
      -- Get the names of the visible commands
      for I in ColumnsNames'Range loop
         if Tcl_GetVar
             (Interp,
              ".columnsdialog.checkbutton" & Trim(Positive'Image(I), Left)) =
           "1" then
            Append(DisplayColumns, Positive'Image(I));
         end if;
      end loop;
      Trim(DisplayColumns, Left);
      Tcl_SetVar(Interp, "visiblecolumns", To_String(DisplayColumns));
      -- Set the displayed columns in all opened archives views
      FilesView.Interp := Interp;
      for I in 1 .. ArchiveNumber loop
         FilesView.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(I), Left) &
              ".filesframe.fileslist");
         if Winfo_Get(FilesView, "exists") = "1" then
            Tcl.Tk.Ada.Widgets.configure
              (FilesView, "-displaycolumns [split $visiblecolumns]");
         end if;
      end loop;
      return TCL_OK;
   end Set_Columns_Command;

   procedure AddCommands is
   begin
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("SetVisibleColumns", Set_Visible_Columns_Command'Access);
      AddCommand("SetColumns", Set_Columns_Command'Access);
   end AddCommands;

end ColumnsDialog;
