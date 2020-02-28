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

with Ada.Containers.Generic_Array_Sort;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Image.Bitmap; use Tcl.Tk.Ada.Image.Bitmap;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with ArchivesViews.Commands; use ArchivesViews.Commands;
with MenuBar; use MenuBar;

package body ArchivesViews is

   ArchiveNumber: Positive;
   ActiveArchive: Natural := 0;
   ColumnsNames: constant array(1 .. 11) of Unbounded_String :=
     (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
      To_Unbounded_String("Modified"), To_Unbounded_String("Attributes"),
      To_Unbounded_String("Size"), To_Unbounded_String("Packed"),
      To_Unbounded_String("Ratio"), To_Unbounded_String("Format"),
      To_Unbounded_String("CRC 32"), To_Unbounded_String("Name encoding"),
      To_Unbounded_String("Result"));

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
      SetCloseCommand(ActiveArchive);
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
           "-show headings -columns [list 1 2 3 4 5 6 7 8 9 10 11] -xscrollcommand """ &
           Widget_Image(FilesXScroll) & " set"" -yscrollcommand """ &
           Widget_Image(FilesYScroll) & " set""");
   begin
      for I in ColumnsNames'Range loop
         Heading
           (FilesList, "#" & Trim(Natural'Image(I), Both),
            "-text {" & To_String(ColumnsNames(I)) & "} -command {Sort {" &
            To_String(ColumnsNames(I)) & "}}");
      end loop;
      Heading(FilesList, "#0", "-image ""arrow-down""");
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
      Arrow: Tk_Bitmap;
      pragma Unreferenced(Arrow);
   begin
      Arrow :=
        Create
          ("arrow-up",
           "-data {#define arrowUp_width 7  #define arrowUp_height 4 static char arrowUp_bits[] = { 0x08, 0x1c, 0x3e, 0x7f };}");
      Arrow :=
        Create
          ("arrow-down",
           "-data {#define arrowDown_width 7  #define arrowDown_height 4 static char arrowDown_bits[] = { 0x7f, 0x3e, 0x1c, 0x08 };}");
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
      -- Some example data
      Insert
        (FilesList,
         "{} end -values [list {" & Simple_Name(FileName) &
         "} 0 0 0 0 0 0 0 0 0 0 0]");
   end LoadArchive;

   function GetArchiveName return String is
      HeaderLabel: Ttk_Label;
   begin
      HeaderLabel.Interp := Get_Context;
      HeaderLabel.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".header.label");
      return cget(HeaderLabel, "-text");
   end GetArchiveName;

   procedure ExtractArchive(Directory: String) is
      FileName: constant String := GetArchiveName;
   begin
      if Directory = "" then
         return;
      end if;
      Ada.Text_IO.Put_Line("Extracting: " & FileName & " into: " & Directory);
   end ExtractArchive;

   procedure AddFiles(FileName: String; Encrypted: Boolean) is
      Tokens: Slice_Set;
      FilesList: Ttk_Tree_View;
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
   begin
      if FileName = "" then
         return;
      end if;
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         SaveArchiveAs;
      end if;
      ArchiveName := To_Unbounded_String(GetArchiveName);
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         return;
      end if;
      FilesList.Interp := Get_Context;
      FilesList.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create(Tokens, FileName, " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if not Encrypted then
            Ada.Text_IO.Put_Line
              ("Adding file " & Slice(Tokens, I) & " to archive " &
               To_String(ArchiveName) & " without encryption");
         else
            Ada.Text_IO.Put_Line
              ("Adding file " & Slice(Tokens, I) & " to archive " &
               To_String(ArchiveName) & " with encryption");
         end if;
         Insert
           (FilesList,
            "{} end -values [list {" & Simple_Name(Slice(Tokens, I)) &
            "} 0 0 0 0 0 0 0 0 0 0 0]");
      end loop;
   end AddFiles;

   procedure SaveArchiveAs is
      NewFileName: Unbounded_String;
      HeaderLabel: Ttk_Label;
      ArchiveName: Unbounded_String;
   begin
      HeaderLabel.Interp := Get_Context;
      HeaderLabel.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".header.label");
      ArchiveName := To_Unbounded_String(cget(HeaderLabel, "-text"));
      NewFileName :=
        To_Unbounded_String
          (Get_Save_File
             ("-parent . -title ""Select a new name for the archive"" -filetypes {{{Zip archives} {.zip}} {{JAR (Java archives)} {.jar}} {{All files} *}} -initialfile """ &
              Simple_Name(To_String(ArchiveName)) & """ -initialdir """ &
              Containing_Directory(To_String(ArchiveName)) & """"));
      if NewFileName = Null_Unbounded_String then
         return;
      end if;
      Ada.Text_IO.Put_Line
        ("Saving " & To_String(ArchiveName) & " as " & To_String(NewFileName));
      configure(HeaderLabel, "-text """ & To_String(NewFileName) & """");
   end SaveArchiveAs;

   procedure DeleteItems is
      FilesList: Ttk_Tree_View;
      Tokens: Slice_Set;
      Selected: Unbounded_String;
   begin
      FilesList.Interp := Get_Context;
      FilesList.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Selected := To_Unbounded_String(Selection(FilesList));
      if Selected = Null_Unbounded_String then
         return;
      end if;
      Create(Tokens, To_String(Selected), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Ada.Text_IO.Put_Line
           ("Deleting file " & Item(FilesList, Slice(Tokens, I), "-text"));
         Delete(FilesList, Slice(Tokens, I));
      end loop;
   end DeleteItems;

   procedure SortArchive(Column: String) is
      FilesView: Ttk_Tree_View;
      ColumnIndex, OldSortColumn: Natural;
      ArrowName, OldArrowName, Values: Unbounded_String;
      Tokens: Slice_Set;
      Ascending: Boolean := True;
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      for I in ColumnsNames'Range loop
         ArrowName :=
           To_Unbounded_String
             (Heading
                (FilesView, "#" & Trim(Natural'Image(I), Both), "-image"));
         if ArrowName /= Null_Unbounded_String then
            OldSortColumn := I;
            OldArrowName := ArrowName;
            Heading
              (FilesView, "#" & Trim(Natural'Image(I), Both), "-image {}");
         end if;
         if ColumnsNames(I) = To_Unbounded_String(Column) then
            ColumnIndex := I;
         end if;
      end loop;
      ArrowName := To_Unbounded_String("arrow-down");
      if OldSortColumn = ColumnIndex
        and then OldArrowName = To_Unbounded_String("arrow-down") then
         ArrowName := To_Unbounded_String("arrow-up");
         Ascending := False;
      end if;
      Heading
        (FilesView, "#" & Trim(Natural'Image(ColumnIndex), Both),
         "-image " & To_String(ArrowName));
      Create(Tokens, Children(FilesView, "{}"), " ");
      if Slice(Tokens, 1) = "" then
         return;
      end if;
      declare
         type File_Record is record
            Index: Unbounded_String;
            Value: Unbounded_String;
         end record;
         function "<"(Left, Right: File_Record) return Boolean is
         begin
            if Ascending then
               return Left.Value < Right.Value;
            else
               return Left.Value > Right.Value;
            end if;
         end "<";
         type Files_Array is array(Natural range <>) of File_Record;
         procedure Sort is new Ada.Containers.Generic_Array_Sort(Natural,
            File_Record, Files_Array);
         FilesList: Files_Array(1 .. Positive(Slice_Count(Tokens)));
         FileEntry: File_Record;
      begin
         for I in 1 .. Slice_Count(Tokens) loop
            Values :=
              To_Unbounded_String
                (Item(FilesView, Slice(Tokens, I), "-values"));
            Tcl_Eval
              (FilesView.Interp,
               "lindex {" & To_String(Values) & "}" &
               Natural'Image(ColumnIndex - 1));
            FileEntry.Index := To_Unbounded_String(Slice(Tokens, I));
            FileEntry.Value :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            FilesList(Positive(I)) := FileEntry;
         end loop;
         Sort(FilesList);
         for I in FilesList'Range loop
            Move
              (FilesView, To_String(FilesList(I).Index), "{}",
               Natural'Image(I));
         end loop;
      end;
   end SortArchive;

   procedure TestArchive is
      ProgressDialog: Tk_Toplevel :=
        Create(".progressdialog", "-class Dialog");
      ProgressBar: constant Ttk_ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      X, Y: Integer;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      Tokens: Slice_Set;
      FilesView: Ttk_Tree_View;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set(ProgressDialog, "title", "{AZip - Test archive progress}");
      Wm_Set(ProgressDialog, "transient", ".");
      Wm_Set(ProgressDialog, "attributes", "-type dialog");
      X := (Positive'Value(Winfo_Get(ProgressDialog, "vrootwidth")) - 275) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(ProgressDialog, "vrootheight")) - 50) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (ProgressDialog, "geometry",
         "275x50+" & Trim(Positive'Image(X), Both) & "+" &
         Trim(Positive'Image(Y), Both));
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create(Tokens, Children(FilesView, "{}"), " ");
      if Slice(Tokens, 1) = "" then
         Tcl.Tk.Ada.Busy.Forget(MainWindow);
         return;
      end if;
      for I in 1 .. Slice_Count(Tokens) loop
         Ada.Text_IO.Put_Line(Slice(Tokens, I));
      end loop;
      Tcl.Tk.Ada.Busy.Forget(MainWindow);
      Destroy(ProgressDialog);
   end TestArchive;

end ArchivesViews;
