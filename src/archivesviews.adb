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

with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews.Commands; use ArchivesViews.Commands;

package body ArchivesViews is

   ArchiveNumber: Positive;
   ActiveArchive: Natural := 0;

   procedure SetActive(NewActive: Positive) is
      Header: Ttk_Frame;
   begin
      Header.Interp := Get_Context;
      if ActiveArchive > 0 then
         Header.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
              ".header");
         if Winfo_Get(Header, "exists") = "1" then
            configure(Header, "-style TFrame");
            Header.Name :=
              New_String
                (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
                 ".header.label");
            configure(Header, "-style TLabel");
         end if;
      end if;
      Header.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(NewActive), Both) & ".header");
      configure(Header, "-style aziptk.TFrame");
      Header.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(NewActive), Both) &
           ".header.label");
      configure(Header, "-style aziptk.TLabel");
      ActiveArchive := NewActive;
   end SetActive;

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
      SetActive(ArchiveNumber);
      Bind
        (Header, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      Bind
        (DirectoryTree, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      Bind
        (FilesList, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
   begin
      ArchiveNumber := 1;
      MDI := Create(".mdi", "-orient vertical");
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      AddCommands;
      CreateView;
   end CreateMDI;

   procedure LoadArchive(FileName: String) is
      Label: Ttk_Label;
      LabelText: Unbounded_String;
      DirectoryTree, FilesList: Ttk_Tree_View;
      ViewName: Unbounded_String :=
        To_Unbounded_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both));
   begin
      if FileName = "" then
         return;
      end if;
      Label.Interp := Get_Context;
      Label.Name := New_String(To_String(ViewName) & ".header.label");
      LabelText := To_Unbounded_String(cget(Label, "-text"));
      if Length(LabelText) > 10
        and then Slice(LabelText, 1, 10) /= "New Archiv" then
         CreateView;
         ViewName :=
           To_Unbounded_String
             (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both));
         Label.Name := New_String(To_String(ViewName) & ".header.label");
      end if;
      configure(Label, "-text """ & FileName & """");
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(To_String(ViewName) & ".directoryframe.directorytree");
      FilesList.Interp := Get_Context;
      FilesList.Name :=
        New_String(To_String(ViewName) & ".filesframe.fileslist");
      Insert(DirectoryTree, "{} end -text """ & Simple_Name(FileName) & """");
      -- Some test data for files list
      Insert
        (FilesList,
         "{} end -text """ & Simple_Name(FileName) &
         """ -values [list 0 0 0 0 0 0 0 0 0 0 0]");
      Insert
        (FilesList,
         "{} end -text """ & Simple_Name(FileName) &
         """ -values [list 0 0 0 0 0 0 0 0 0 0 0]");
      Insert
        (FilesList,
         "{} end -text """ & Simple_Name(FileName) &
         """ -values [list 0 0 0 0 0 0 0 0 0 0 0]");
   end LoadArchive;

   procedure ExtractArchive(Directory: String) is
   begin
      if Directory = "" then
         return;
      end if;
   end ExtractArchive;

end ArchivesViews;
