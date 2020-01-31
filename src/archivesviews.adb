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
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
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
   MDI: Ttk_PanedWindow;

   procedure CreateView is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ArchiveNumber), Both);
      ArchiveView: constant Ttk_Frame := Create(ViewName);
      Header: constant Ttk_Frame := Create(ViewName & ".header");
      CloseButton: constant Ttk_Button :=
        Create
          (ViewName & ".header.close",
           "-text x -style Toolbutton -command ""Close" &
           Positive'Image(ArchiveNumber) & """");
      NameLabel: constant Ttk_Label :=
        Create
          (ViewName & ".header.label",
           "-text ""New Archive" & Positive'Image(ArchiveNumber) & """");
      Paned: constant Ttk_PanedWindow :=
        Create(ViewName & ".paned", "-orient horizontal");
      DirectoryFrame: constant Ttk_Frame :=
        Create(ViewName & ".directoryframe");
      DirectoryXScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(DirectoryFrame) & ".scrollx",
           "-orient horizontal -command [list " &
           Widget_Image(DirectoryFrame) & ".directorytree xview]");
      DirectoryYScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(DirectoryFrame) & ".scrolly",
           "-orient vertical -command [list " & Widget_Image(DirectoryFrame) &
           ".directorytree yview]");
      DirectoryTree: constant Ttk_Tree_View :=
        Create
          (Widget_Image(DirectoryFrame) & ".directorytree",
           "-show tree -xscrollcommand """ & Widget_Image(DirectoryXScroll) &
           " set"" -yscrollcommand """ & Widget_Image(DirectoryYScroll) &
           " set""");
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
      Add(Paned, DirectoryFrame, "-weight 1");
      Tcl.Tk.Ada.Pack.Pack(DirectoryXScroll, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(DirectoryYScroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(DirectoryTree, "-side top -fill both -expand true");
      Add(Paned, FilesFrame, "-weight 20");
      Tcl.Tk.Ada.Pack.Pack(FilesXScroll, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(FilesYScroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(FilesList, "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack(Paned, "-fill both -expand true");
      Add(MDI, ArchiveView);
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
      package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
      function Close_Command
        (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
         Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
         return Interfaces.C.int;
      pragma Convention(C, Close_Command);
      function Close_Command
        (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
         Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
         return Interfaces.C.int is
         pragma Unreferenced(ClientData, Argc);
         use type Interfaces.C.int;
         ArchiveName: constant String := ".mdi.archive" & CArgv.Arg(Argv, 1);
      begin
         if Tcl.Ada.Tcl_Eval(Interp, "destroy " & ArchiveName) = TCL_ERROR then
            raise Program_Error with "Can't destroy archive view.";
         end if;
         if Panes(MDI) = "" then
            CreateView;
         end if;
         return 0;
      end Close_Command;
      function Create_Command
        (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
         Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
         return Interfaces.C.int;
      pragma Convention(C, Create_Command);
      function Create_Command
        (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
         Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
         return Interfaces.C.int is
         pragma Unreferenced(ClientData, Interp, Argc, Argv);
      begin
         CreateView;
         return 0;
      end Create_Command;
      Command: Tcl.Tcl_Command;
      pragma Unreferenced(Command);
   begin
      ArchiveNumber := 1;
      MDI := Create(".mdi", "-orient vertical");
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "Close", Close_Command'Access, 0, null);
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "Create", Create_Command'Access, 0, null);
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      CreateView;
   end CreateMDI;

end ArchivesViews;
