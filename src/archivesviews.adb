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
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;

package body ArchivesViews is

   ArchiveNumber: Positive;

   procedure CreateView is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ArchiveNumber), Both);
      ArchiveView: constant Ttk_Frame := Create(ViewName);
      Header: constant Ttk_Frame := Create(ViewName & ".header");
      CloseButton: constant Ttk_Button :=
        Create(ViewName & ".header.close", "-text x -style Toolbutton");
      NameLabel: constant Ttk_Label :=
        Create
          (ViewName & ".header.label",
           "-text ""New Archive" & Positive'Image(ArchiveNumber) & """");
      Paned: constant Ttk_PanedWindow :=
        Create(ViewName & ".paned", "-orient horizontal");
      DirectoryTree: constant Ttk_Tree_View :=
        Create(ViewName & ".directorytree", "-show tree");
      FilesFrame: constant Ttk_Frame := Create(ViewName & ".filesframe");
      FilesXScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(FilesFrame) & ".scrollx",
           "-orient horizontal -command [list " & Widget_Image(FilesFrame) &
           ".fileslist xview]");
      FilesYScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(FilesFrame) & ".scrolly",
           "-orient vertical -command [list " & Widget_Image(FilesFrame) &
           ".fileslist yview]");
      FilesList: constant Ttk_Tree_View :=
        Create
          (Widget_Image(FilesFrame) & ".fileslist",
           "-columns [list 1 2 3 4 5 6 7 8 9 10] -xscrollcommand """ &
           Widget_Image(FilesXScroll) & " set"" -yscrollcommand """ &
           Widget_Image(FilesYScroll) & " set""");
      ColumnsNames: constant array(0 .. 10) of Unbounded_String :=
        (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
         To_Unbounded_String("Modified"), To_Unbounded_String("Attributes"),
         To_Unbounded_String("Size"), To_Unbounded_String("Packed"),
         To_Unbounded_String("Ratio"), To_Unbounded_String("Format"),
         To_Unbounded_String("CRC 32"), To_Unbounded_String("Name encoding"),
         To_Unbounded_String("Result"));
   begin
      for I in ColumnsNames'Range loop
         Heading
           (FilesList, "#" & Trim(Natural'Image(I), Both),
            "-text """ & To_String(ColumnsNames(I)) & """");
      end loop;
      Tcl.Tk.Ada.Pack.Pack(NameLabel, "-side left");
      Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
      Tcl.Tk.Ada.Pack.Pack(Header, "-fill x");
      Add(Paned, DirectoryTree, "-weight 1");
      Add(Paned, FilesFrame, "-weight 20");
      Tcl.Tk.Ada.Grid.Grid(FilesList, "-column 0 -row 0 -sticky nwes");
      Tcl.Tk.Ada.Grid.Grid(FilesYScroll, "-column 1 -row 0 -sticky nwes");
      Tcl.Tk.Ada.Grid.Grid(FilesXScroll, "-column 0 -row 1 -sticky nwes");
      Tcl.Tk.Ada.Grid.Row_Configure(FilesFrame, FilesList, "-weight 1");
      Tcl.Tk.Ada.Grid.Column_Configure(FilesFrame, FilesList, "-weight 1");
      Tcl.Tk.Ada.Pack.Pack(Paned, "-fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack(ArchiveView, "-fill both -expand true");
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
      MDI: constant Ttk_Frame := Create(".mdi");
   begin
      ArchiveNumber := 1;
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      CreateView;
   end CreateMDI;

end ArchivesViews;
