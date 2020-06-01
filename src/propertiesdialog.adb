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
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Utils; use Utils;

package body PropertiesDialog is

   function Show_Properties_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_Properties_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      PropertiesDialog: constant Tk_Toplevel :=
        Create(".propertiesdialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      ButtonBox: constant Ttk_Frame := Create(".propertiesdialog.buttonbox");
      Button: Ttk_Button;
      PropertiesTree: constant Ttk_Tree_View :=
        Create
          (".propertiesdialog.tree",
           "-show headings -selectmode none -columns [list format entries percent ratio]");
      Label: Ttk_Label;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Label :=
        Create
          (".propertiesdialog.uncompressedtext", "-text {Uncompressed size}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label := Create(".propertiesdialog.uncompressed", "-text {0 bytes}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      Label :=
        Create(".propertiesdialog.compressedtext", "-text {Compressed size}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 1");
      Label := Create(".propertiesdialog.compressed", "-text {0 bytes}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 1");
      Label := Create(".propertiesdialog.ratio");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 2 -row 1");
      Label := Create(".propertiesdialog.entriestext", "-text {Entries}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 2");
      Label := Create(".propertiesdialog.entries", "-text {0 bytes}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2");
      Heading(PropertiesTree, "format", "-text {Format (""method"")}");
      Column(PropertiesTree, "format", "-width 140");
      Heading(PropertiesTree, "entries", "-text {Entries}");
      Column(PropertiesTree, "entries", "-width 100");
      Heading(PropertiesTree, "percent", "-text {% of data}");
      Column(PropertiesTree, "percent", "-width 110");
      Heading(PropertiesTree, "ratio", "-text {Ratio}");
      Column(PropertiesTree, "ratio", "-width 100");
      Tcl.Tk.Ada.Grid.Grid
        (PropertiesTree, "-column 0 -row 3 -sticky we -columnspan 3");
      -- Some test data
      Insert(PropertiesTree, "{} end -values [list deflate 0 0% 0%]");
      Button :=
        Create
          (".propertiesdialog.buttonbox.ok",
           "-text Ok -command {CloseDialog .propertiesdialog}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".propertiesdialog.buttonbox.about",
           "-text {About AZip} -command {CloseDialog .propertiesdialog; ShowAbout}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid
        (ButtonBox, "-column 1 -row 4 -sticky we -columnspan 3");
      SetDialog(PropertiesDialog, "AZip - Archive properties", 500, 400);
      return TCL_OK;
   end Show_Properties_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowProperties", Show_Properties_Command'Access);
   end AddCommands;

end PropertiesDialog;
