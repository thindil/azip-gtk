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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Image.Bitmap; use Tcl.Tk.Ada.Image.Bitmap;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews.Commands;
with ColumnsDialog; use ColumnsDialog;
with MenuBar; use MenuBar;
with Toolbar; use Toolbar;

package body ArchivesViews is

   procedure SetActive(NewActive: Positive; Created: Boolean := False) is
      Header: Ttk_Frame;
      OldName: constant String :=
        ".mdi.archive" & Trim(Natural'Image(ActiveArchive), Both);
      NewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(NewActive), Both);
      NameLabel: Ttk_Label;
   begin
      Header.Interp := Get_Context;
      if ActiveArchive > 0 then
         Header.Name := New_String(OldName & ".header");
         if Winfo_Get(Header, "exists") = "1" then
            configure(Header, "-style TFrame");
            Header.Name := New_String(OldName & ".header.label");
            configure(Header, "-style TLabel");
         end if;
      end if;
      Header.Name := New_String(NewName & ".header");
      configure(Header, "-style aziptk.TFrame");
      NameLabel.Interp := Get_Context;
      NameLabel.Name := New_String(NewName & ".header.label");
      configure(NameLabel, "-style aziptk.TLabel");
      ActiveArchive := NewActive;
      SetCloseCommand(ActiveArchive);
      Header.Name := New_String(NewName & ".directoryframe");
      if not Created then
         if Winfo_Get(Header, "ismapped") = "1" then
            Tcl_SetVar(Header.Interp, "viewtype", "tree");
         else
            Tcl_SetVar(Header.Interp, "viewtype", "flat");
         end if;
      end if;
      if cget(NameLabel, "-text")(1 .. 3) /= "New" then
         ToggleButtons;
      else
         ToggleButtons(False);
      end if;
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
           "-show headings -columns [list 1 2 3 4 5 6 7 8 9 10 11 12] -xscrollcommand {" &
           Widget_Image(FilesXScroll) & " set} -yscrollcommand {" &
           Widget_Image(FilesYScroll) & " set}");
      ViewType: constant String := Tcl_GetVar(FilesList.Interp, "viewtype");
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
           "-show tree -selectmode browse -xscrollcommand """ &
           Widget_Image(DirectoryXScroll) & " set"" -yscrollcommand """ &
           Widget_Image(DirectoryYScroll) & " set""");
   begin
      for I in ColumnsNames'Range loop
         Heading
           (FilesList, "#" & Trim(Natural'Image(I), Both),
            "-text {" & To_String(ColumnsNames(I)) & "} -command {Sort {" &
            To_String(ColumnsNames(I)) & "}}");
      end loop;
      configure(FilesList, "-displaycolumns [split $visiblecolumns]");
      Bind(DirectoryTree, "<<TreeviewSelect>>", "DirectorySelected");
      Bind(DirectoryTree, "<3>", "{tk_popup .directorymenu %X %Y}");
      Tcl.Tk.Ada.Pack.Pack(NameLabel, "-side left");
      Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
      Tcl.Tk.Ada.Pack.Pack(Header, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(DirectoryXScroll, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(DirectoryYScroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(DirectoryTree, "-side top -fill both -expand true");
      if ViewType = "tree" then
         Add(Paned, DirectoryFrame, "-weight 1");
      end if;
      Add(Paned, FilesFrame, "-weight 20");
      Tcl.Tk.Ada.Pack.Pack(FilesXScroll, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(FilesYScroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(FilesList, "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack(Paned, "-fill both -expand true");
      Add(MDI, ArchiveView);
      SetActive(ArchiveNumber, True);
      Bind
        (Header, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      Bind
        (DirectoryTree, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      Bind
        (FilesList, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Both) & "}");
      Bind(FilesList, "<3>", "{tk_popup .filesmenu %X %Y}");
      Tcl_SetVar
        (Paned.Interp, "lastindex" & Trim(Positive'Image(ActiveArchive), Both),
         "1");
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
      Arrow: Tk_Bitmap;
      pragma Unreferenced(Arrow);
      DirectoryMenu: constant Tk_Menu :=
        Create(".directorymenu", "-tearoff false");
      FilesMenu: constant Tk_Menu := Create(".filesmenu", "-tearoff false");
   begin
      Arrow :=
        Create
          ("arrow-up",
           "-data {#define arrowUp_width 7  #define arrowUp_height 4 static char arrowUp_bits[] = { 0x08, 0x1c, 0x3e, 0x7f };}");
      Arrow :=
        Create
          ("arrow-down",
           "-data {#define arrowDown_width 7  #define arrowDown_height 4 static char arrowDown_bits[] = { 0x7f, 0x3e, 0x1c, 0x08 };}");
      Add
        (DirectoryMenu, "command",
         "-label {Extract folder} -underline 0 -command Extract");
      Add
        (DirectoryMenu, "command",
         "-label {Delete folder} -underline 0 -command DeleteDirectory");
      Add
        (FilesMenu, "command",
         "-label {Extract files(s)} -underline 0 -command ExtractFile");
      Add
        (FilesMenu, "command",
         "-label {Delete files(s)} -underline 0 -command DeleteItems");
      ArchiveNumber := 1;
      MDI := Create(".mdi", "-orient vertical");
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      ArchivesViews.Commands.AddCommands;
      ColumnsDialog.AddCommands;
      CreateView;
   end CreateMDI;

   procedure AddFile(FileName, Path: String; Hide: Boolean := False) is
      FilesList: Ttk_Tree_View;
      FileIndex: Unbounded_String;
   begin
      FilesList.Interp := Get_Context;
      FilesList.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      FileIndex :=
        To_Unbounded_String
          (Tcl_GetVar
             (FilesList.Interp,
              "lastindex" & Trim(Positive'Image(ActiveArchive), Both)));
      -- Some example data. All file data are in values list in order:
      -- Name of the file, type, modified, attributes, size, packed, ratio,
      -- format, crc32, path, name encoding, result
      Insert
        (FilesList,
         "{} end -id " & To_String(FileIndex) & " -values [list {" &
         Simple_Name(FileName) & "} 2 3 4 5 6 7 8 9 {" & Path & "} 11 12]");
      if Hide then
         Detach(FilesList, To_String(FileIndex));
      end if;
      Tcl_SetVar
        (FilesList.Interp,
         "lastindex" & Trim(Positive'Image(ActiveArchive), Both),
         Positive'Image(Positive'Value(To_String(FileIndex)) + 1));
   end AddFile;

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

   procedure AddFiles
     (FileName: String; Encrypted: Boolean; Path: String := "";
      Hide: Boolean := False) is
      Tokens, Tokens2: Slice_Set;
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
      FilesView: Ttk_Tree_View;
      Values, ExistingFileName, ExistingPath: Unbounded_String;
   begin
      if FileName = "" then
         return;
      end if;
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         SaveArchiveAs;
         ArchiveName := To_Unbounded_String(GetArchiveName);
         if Length(ArchiveName) > 10
           and then Slice(ArchiveName, 1, 10) = "New Archiv" then
            return;
         end if;
      end if;
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create(Tokens, FileName, " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Create(Tokens2, Children(FilesView, "{}"), " ");
         for J in 1 .. Slice_Count(Tokens2) loop
            if Slice(Tokens2, J) /= "" then
               Values :=
                 To_Unbounded_String
                   (Item(FilesView, Slice(Tokens2, J), "-values"));
               Tcl_Eval
                 (FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
               ExistingFileName :=
                 To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
               Tcl_Eval
                 (FilesView.Interp, "lindex {" & To_String(Values) & "} 9");
               ExistingPath :=
                 To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
               if
                 (To_String(ExistingFileName) =
                  Simple_Name(Slice(Tokens, I)) and
                  To_String(ExistingPath) = Path)
                 and then
                   MessageBox
                     ("-message {File " & Simple_Name(Slice(Tokens, I)) &
                      " exists in the selected archive} -icon error -type ok") /=
                   "" then
                  return;
               end if;
            end if;
         end loop;
         if not Encrypted then
            Ada.Text_IO.Put_Line
              ("Adding file " & Slice(Tokens, I) & " to archive " &
               To_String(ArchiveName) & " without encryption");
         else
            Ada.Text_IO.Put_Line
              ("Adding file " & Slice(Tokens, I) & " to archive " &
               To_String(ArchiveName) & " with encryption");
         end if;
         AddFile(Slice(Tokens, I), Path, Hide);
      end loop;
      -- Sort archive if enabled
      if Tcl_GetVar(Get_Context, "nosorting") = "0" then
         Heading(FilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
      ToggleButtons;
   end AddFiles;

   procedure SaveArchiveAs is
      NewFileName, ArchiveName, Directories: Unbounded_String;
      HeaderLabel: Ttk_Label;
      DirectoryTree: Ttk_Tree_View;
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      Tokens: Slice_Set;
   begin
      HeaderLabel.Interp := Get_Context;
      HeaderLabel.Name := New_String(ViewName & ".header.label");
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
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(ViewName & ".directoryframe.directorytree");
      Directories := To_Unbounded_String(Children(DirectoryTree, "{}"));
      if Directories /= Null_Unbounded_String then
         Create(Tokens, To_String(Directories), " ");
         Item
           (DirectoryTree, Slice(Tokens, 1),
            "-text {" & Simple_Name(To_String(NewFileName)) & "}");
      else
         Insert
           (DirectoryTree,
            "{} end -text {" & Simple_Name(To_String(NewFileName)) & "}");
         Selection_Set
           (DirectoryTree,
            "[lindex {" & Children(DirectoryTree, "{}") & "} 0]");
      end if;
   end SaveArchiveAs;

   procedure DeleteItems is
      FilesList: Ttk_Tree_View;
      Tokens: Slice_Set;
      Selected, Values, FileName, Path: Unbounded_String;
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
      if MessageBox
          ("-message {Do you want to remove the" &
           Slice_Number'Image(Slice_Count(Tokens)) &
           " selected item(s)? } -icon question -type yesno") =
        "no" then
         return;
      end if;
      for I in 1 .. Slice_Count(Tokens) loop
         Values :=
           To_Unbounded_String(Item(FilesList, Slice(Tokens, I), "-values"));
         Tcl_Eval(FilesList.Interp, "lindex {" & To_String(Values) & "} 0");
         FileName :=
           To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesList.Interp));
         Tcl_Eval(FilesList.Interp, "lindex {" & To_String(Values) & "} 9");
         Path := To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesList.Interp));
         Ada.Text_IO.Put_Line
           ("Deleting file " &
            To_String(Path & Directory_Separator & FileName));
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
      Create(Tokens, cget(FilesView, "-displaycolumns"), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         ArrowName :=
           To_Unbounded_String(Heading(FilesView, Slice(Tokens, I), "-image"));
         if ArrowName /= Null_Unbounded_String then
            OldSortColumn := Positive'Value(Slice(Tokens, I));
            OldArrowName := ArrowName;
            Heading(FilesView, Slice(Tokens, I), "-image {}");
         end if;
         if ColumnsNames(Positive'Value(Slice(Tokens, I))) =
           To_Unbounded_String(Column) then
            ColumnIndex := Positive'Value(Slice(Tokens, I));
         end if;
      end loop;
      ArrowName := To_Unbounded_String("arrow-down");
      if OldSortColumn = ColumnIndex
        and then OldArrowName = To_Unbounded_String("arrow-down") then
         ArrowName := To_Unbounded_String("arrow-up");
         Ascending := False;
      end if;
      Heading
        (FilesView, Trim(Natural'Image(ColumnIndex), Both),
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
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      FilesView: Ttk_Tree_View;
      Values, FileName: Unbounded_String;
      LastIndex: constant Positive :=
        Positive'Value
          (Tcl_GetVar
             (MainWindow.Interp,
              "lastindex" & Trim(Positive'Image(ActiveArchive), Both)));
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(ProgressDialog, "Azip - Test archive progress", 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      if LastIndex = 1 then
         Destroy(ProgressDialog);
         return;
      end if;
      for I in 1 .. LastIndex loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
            FileName :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Ada.Text_IO.Put_Line("Testing file: " & To_String(FileName));
            Tcl_Eval
              (FilesView.Interp, "lrange {" & To_String(Values) & "} 0 10");
            Values :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Item
              (FilesView, Positive'Image(I),
               "-values [list " & To_String(Values) & " OK ]");
            Step(ProgressBar);
         end if;
      end loop;
      Destroy(ProgressDialog);
   end TestArchive;

   procedure ShowFindDialog is
      FindDialog: Tk_Toplevel := Create(".finddialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      Label: Ttk_Label;
      TEntry: Ttk_Entry;
      ButtonBox: constant Ttk_Frame := Create(".finddialog.buttonbox");
      Button: Ttk_Button;
      Tokens: Slice_Set;
      FilesView: Ttk_Tree_View;
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create(Tokens, Children(FilesView, "{}"), " ");
      if Slice(Tokens, 1) = "" then
         Destroy(FindDialog);
         return;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(FindDialog, "AZip - find in archive", 300, 200);
      Label := Create(".finddialog.findimage", "-image .toolbar.findicon");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 1");
      Label :=
        Create
          (".finddialog.labelname",
           "-text {Entry name ( if empty: all names )}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      TEntry := Create(".finddialog.entryname");
      Tcl.Tk.Ada.Grid.Grid(TEntry, "-column 1 -row 1 -sticky we");
      Label := Create(".finddialog.findimage2", "-image .toolbar.findicon");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 3");
      Label :=
        Create
          (".finddialog.labelcontent",
           "-text {Content ( if empty: all content )}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2");
      TEntry := Create(".finddialog.entrycontent");
      Tcl.Tk.Ada.Grid.Grid(TEntry, "-column 1 -row 3 -sticky we");
      Button :=
        Create(".finddialog.buttonbox.ok", "-text Ok -command FindInArchive");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".finddialog.buttonbox.cancel",
           "-text Cancel -command {CloseDialog .finddialog}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid(ButtonBox, "-column 1 -row 4 -sticky e");
   end ShowFindDialog;

   procedure FindInArchive is
      FindDialog: Tk_Toplevel;
      TextEntry: Ttk_Entry;
      Name, Content, Values, FileName: Unbounded_String;
      FilesView: Ttk_Tree_View;
      EntriesFound, Occurences, OverallResult, Result: Natural := 0;
   begin
      FindDialog.Interp := Get_Context;
      FindDialog.Name := New_String(".finddialog");
      TextEntry.Interp := FindDialog.Interp;
      TextEntry.Name := New_String(".finddialog.entryname");
      Name := To_Unbounded_String(Get(TextEntry));
      TextEntry.Name := New_String(".finddialog.entrycontent");
      Content := To_Unbounded_String(Get(TextEntry));
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      for I in
        1 ..
          Positive'Value
            (Tcl_GetVar
               (FilesView.Interp,
                "lastindex" & Trim(Positive'Image(ActiveArchive), Both))) loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
            FileName :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Ada.Text_IO.Put_Line
              ("Looking for name: " & To_String(Name) & " in " &
               To_String(FileName));
            if Name = Null_Unbounded_String then
               Result := 1;
            else
               if Index(FileName, To_String(Name)) > 0 then
                  Result := 1;
               else
                  Result := 0;
               end if;
            end if;
            EntriesFound := EntriesFound + Result;
            OverallResult := Result;
            Ada.Text_IO.Put_Line
              ("Looking for content: " & To_String(Content) & " in " &
               To_String(FileName));
            Result := 0;
            Occurences := Occurences + Result;
            OverallResult := OverallResult + Result;
            Tcl_Eval
              (FilesView.Interp, "lrange {" & To_String(Values) & "} 0 10");
            Values :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Item
              (FilesView, Positive'Image(I),
               "-values [list " & To_String(Values) & "" &
               Natural'Image(OverallResult) & " ]");
         end if;
      end loop;
      Destroy(FindDialog);
      if MessageBox
          ("-message {Search completed. " & LF & LF & "Occurences found:" &
           Natural'Image(Occurences) & " " & LF & "Total entries:" &
           Natural'Image(EntriesFound) &
           "} -icon question -type yesno -detail {Do you want to see full results (flat view & result sort)?}") =
        "yes" then
         Tcl_SetVar(FilesView.Interp, "viewtype", "flat");
         ToggleView;
         Heading(FilesView, "12", "-image arrow-down");
         SortArchive("Result");
      end if;
   end FindInArchive;

   procedure ToggleView is
      DirectoryFrame: Ttk_Frame;
      Paned: Ttk_PanedWindow;
      FilesList: Ttk_Tree_View;
      VisibleColumns: Unbounded_String;
      Tokens: Slice_Set;
   begin
      DirectoryFrame.Interp := Get_Context;
      Paned.Interp := DirectoryFrame.Interp;
      FilesList.Interp := DirectoryFrame.Interp;
      if Tcl_GetVar(Get_Context, "viewtype") = "tree" then
         Create(Tokens, Tcl_GetVar(Get_Context, "visiblecolumns"), " ");
         for I in 1 .. Slice_Count(Tokens) loop
            if Slice(Tokens, I) /= "10" then
               Append(VisibleColumns, Slice(Tokens, I) & " ");
            end if;
         end loop;
         Trim(VisibleColumns, Right);
      else
         VisibleColumns :=
           To_Unbounded_String(Tcl_GetVar(Get_Context, "visiblecolumns"));
         if Index(VisibleColumns, "10") = 0 then
            Append(VisibleColumns, " 10");
         end if;
      end if;
      Tcl_SetVar(Get_Context, "visiblecolumns", To_String(VisibleColumns));
      for I in 1 .. ArchiveNumber loop
         FilesList.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(I), Left) &
              ".filesframe.fileslist");
         if Winfo_Get(FilesList, "exists") = "0" then
            goto End_Of_Loop;
         end if;
         DirectoryFrame.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(I), Left) &
              ".directoryframe");
         Paned.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(I), Left) & ".paned");
         if Tcl_GetVar(Get_Context, "viewtype") = "tree"
           and then Winfo_Get(DirectoryFrame, "ismapped") = "0" then
            Insert(Paned, "0", DirectoryFrame, "-weight 1");
         elsif Winfo_Get(DirectoryFrame, "ismapped") = "1" then
            Forget(Paned, DirectoryFrame);
         end if;
         configure(FilesList, "-displaycolumns [split $visiblecolumns]");
         ShowFiles;
         for I in 1 .. 12 loop
            Heading(FilesList, Positive'Image(I), "-image {}");
         end loop;
         <<End_Of_Loop>>
      end loop;
   end ToggleView;

   procedure AddDirectory(DirectoryName: String; Encrypted: Boolean) is
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
      DirectoryTree: Ttk_Tree_View;
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      MainNode: Unbounded_String;
      Tokens: Slice_Set;
      function GetInsertIndex(Parent, DirName: String) return String is
         Tokens2: Slice_Set;
      begin
         Create(Tokens2, Children(DirectoryTree, Parent), " ");
         for I in 1 .. Slice_Count(Tokens2) loop
            if Slice(Tokens2, I) /= ""
              and then Item(DirectoryTree, Slice(Tokens2, I), "-text") >
                DirName then
               return Index(DirectoryTree, Slice(Tokens2, I));
            end if;
         end loop;
         return "end";
      end GetInsertIndex;
      procedure AddDir(DirName, Parent: String) is
         Directory: Dir_Type;
         Last: Natural;
         FileName: String(1 .. 1024);
         NewParentIndex: Unbounded_String;
      begin
         Open(Directory, DirName);
         loop
            Read(Directory, FileName, Last);
            exit when Last = 0;
            if FileName(1 .. Last) in "." | ".." then
               goto End_Of_Loop;
            end if;
            if Is_Directory
                (DirName & Directory_Separator & FileName(1 .. Last)) then
               if not Encrypted then
                  Ada.Text_IO.Put_Line
                    ("Adding directory " & DirName & " to archive " &
                     To_String(ArchiveName) & " without encryption");
               else
                  Ada.Text_IO.Put_Line
                    ("Adding directory " & DirName & " to archive " &
                     To_String(ArchiveName) & " with encryption");
               end if;
               NewParentIndex :=
                 To_Unbounded_String
                   (Insert
                      (DirectoryTree,
                       Parent & " " &
                       GetInsertIndex
                         (Parent, Simple_Name(FileName(1 .. Last))) &
                       " -text {" & Simple_Name(FileName(1 .. Last)) & "}"));
               AddDir
                 (DirName & Directory_Separator & FileName(1 .. Last),
                  To_String(NewParentIndex));
            else
               AddFiles
                 (DirName & Directory_Separator & FileName(1 .. Last),
                  Encrypted,
                  DirName
                    (Index(DirName, Simple_Name(DirectoryName)) ..
                         DirName'Last),
                  True);
            end if;
            <<End_Of_Loop>>
         end loop;
         Close(Directory);
      end AddDir;
   begin
      if DirectoryName = "" then
         return;
      end if;
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         SaveArchiveAs;
         ArchiveName := To_Unbounded_String(GetArchiveName);
         if Length(ArchiveName) > 10
           and then Slice(ArchiveName, 1, 10) = "New Archiv" then
            return;
         end if;
      end if;
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(ViewName & ".directoryframe.directorytree");
      MainNode := To_Unbounded_String(Children(DirectoryTree, "{}"));
      Create(Tokens, Children(DirectoryTree, To_String(MainNode)), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if Slice(Tokens, I) /= ""
           and then Item(DirectoryTree, Slice(Tokens, I), "-text") =
             Simple_Name(DirectoryName) then
            if MessageBox
                ("-message {Directory " & Simple_Name(DirectoryName) &
                 " exists in the selected archive} -icon error -type ok") /=
              "" then
               return;
            end if;
         end if;
      end loop;
      AddDir
        (DirectoryName,
         Insert
           (DirectoryTree,
            To_String(MainNode) & " " &
            GetInsertIndex(To_String(MainNode), Simple_Name(DirectoryName)) &
            " -text {" & Simple_Name(DirectoryName) & "}"));
   end AddDirectory;

   procedure ShowFiles is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      DirectoryTree: Ttk_Tree_View;
      FilesView: Ttk_Tree_View;
      Path, ParentId, Selected, Values, FilePath: Unbounded_String :=
        Null_Unbounded_String;
      FlatView: Boolean := False;
   begin
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(ViewName & ".directoryframe.directorytree");
      Selected := To_Unbounded_String(Selection(DirectoryTree));
      if Selected = Null_Unbounded_String then
         return;
      end if;
      loop
         ParentId :=
           To_Unbounded_String(Parent(DirectoryTree, To_String(Selected)));
         exit when ParentId = To_Unbounded_String("");
         if Path /= Null_Unbounded_String then
            Path := Directory_Separator & Path;
         end if;
         Path :=
           To_Unbounded_String
             (Item(DirectoryTree, To_String(Selected), "-text")) &
           Path;
         Selected := ParentId;
      end loop;
      FilesView.Interp := DirectoryTree.Interp;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
      if Tcl_GetVar(FilesView.Interp, "viewtype") = "flat" then
         FlatView := True;
      else
         for I in
           1 ..
             Positive'Value
               (Tcl_GetVar
                  (FilesView.Interp,
                   "lastindex" &
                   Trim(Positive'Image(ActiveArchive), Both))) loop
            if Exists(FilesView, Positive'Image(I)) = "1" then
               Detach(FilesView, Positive'Image(I));
            end if;
         end loop;
      end if;
      for I in
        1 ..
          Positive'Value
            (Tcl_GetVar
               (FilesView.Interp,
                "lastindex" & Trim(Positive'Image(ActiveArchive), Both))) loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 9");
            FilePath :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            if (FilePath = Path) or FlatView then
               Move(FilesView, Positive'Image(I), "{}", Positive'Image(I));
            end if;
         end if;
      end loop;
      -- Sort archive if enabled
      if Tcl_GetVar(Get_Context, "nosorting") = "0" then
         Heading(FilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
   end ShowFiles;

   procedure UpdateArchive is
      ProgressDialog: Tk_Toplevel :=
        Create(".progressdialog", "-class Dialog");
      ProgressBar: constant Ttk_ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      FilesView: Ttk_Tree_View;
      Values, FileName: Unbounded_String;
      LastIndex: constant Positive :=
        Positive'Value
          (Tcl_GetVar
             (MainWindow.Interp,
              "lastindex" & Trim(Positive'Image(ActiveArchive), Both)));
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(ProgressDialog, "Azip - Update archive progress", 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      if LastIndex = 1 then
         Destroy(ProgressDialog);
         return;
      end if;
      for I in 1 .. LastIndex loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
            FileName :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Ada.Text_IO.Put_Line("Updating file: " & To_String(FileName));
            Tcl_Eval
              (FilesView.Interp, "lrange {" & To_String(Values) & "} 0 10");
            Values :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Item
              (FilesView, Positive'Image(I),
               "-values [list " & To_String(Values) & " OK ]");
            Step(ProgressBar);
         end if;
      end loop;
      Destroy(ProgressDialog);
      if MessageBox
          ("-message {Update completed.} -icon info -type ok -detail {No entry needed to be updated.}") =
        "" then
         return;
      end if;
   end UpdateArchive;

   procedure RecompressArchive is
      ProgressDialog: Tk_Toplevel :=
        Create(".progressdialog", "-class Dialog");
      ProgressBar: constant Ttk_ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      FilesView: Ttk_Tree_View;
      Values, FileName: Unbounded_String;
      LastIndex: constant Positive :=
        Positive'Value
          (Tcl_GetVar
             (MainWindow.Interp,
              "lastindex" & Trim(Positive'Image(ActiveArchive), Both)));
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(ProgressDialog, "Azip - Update archive progress", 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      if LastIndex = 1 then
         Destroy(ProgressDialog);
         return;
      end if;
      for I in 1 .. LastIndex loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
            FileName :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Ada.Text_IO.Put_Line("Recompresing file: " & To_String(FileName));
            Tcl_Eval
              (FilesView.Interp, "lrange {" & To_String(Values) & "} 0 10");
            Values :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Item
              (FilesView, Positive'Image(I),
               "-values [list " & To_String(Values) & " OK ]");
            Step(ProgressBar);
         end if;
      end loop;
      Destroy(ProgressDialog);
      if MessageBox
          ("-message {Recompression completed.} -icon info -type ok -detail {No entry could be recompressed to a smaller size.}") =
        "" then
         return;
      end if;
   end RecompressArchive;

   procedure ShowProperties is
      PropertiesDialog: constant Tk_Toplevel :=
        Create(".propertiesdialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
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
   end ShowProperties;

   procedure ToggleSelect(SelectAll: Boolean) is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      FilesView: Ttk_Tree_View;
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
      if SelectAll then
         Selection_Set(FilesView, "[list " & Children(FilesView, "{}") & " ]");
      else
         Selection_Set(FilesView, "{}");
      end if;
   end ToggleSelect;

   procedure DeleteDirectory is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      FilesView: Ttk_Tree_View;
      DirectoryTree: Ttk_Tree_View;
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
      Selection_Set(FilesView, "[list " & Children(FilesView, "{}") & " ]");
      DeleteItems;
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(ViewName & ".directoryframe.directorytree");
      Delete(DirectoryTree, Selection(DirectoryTree));
   end DeleteDirectory;

   procedure ExtractFile(Directory: String) is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      ArchiveName: constant String := GetArchiveName;
      FilesView: Ttk_Tree_View;
      Path, Selected, FileName, Values, NewDirectory, Answer: Unbounded_String;
      Tokens: Slice_Set;
      ProgressDialog: Tk_Toplevel :=
        Create(".progressdialog", "-class Dialog");
      ProgressBar: constant Ttk_ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
      Selected := To_Unbounded_String(Selection(FilesView));
      if Selected = Null_Unbounded_String then
         Destroy(ProgressDialog);
         return;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(ProgressDialog, "Azip - Extract progress", 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      Answer :=
        To_Unbounded_String
          (MessageBox
             ("-message {Use archive's folder name for output? } -icon question -type yesnocancel"));
      if Answer = To_Unbounded_String("cancel") then
         Destroy(ProgressDialog);
         return;
      end if;
      if Answer = To_Unbounded_String("yes") then
         NewDirectory :=
           To_Unbounded_String
             (Directory & Directory_Separator &
              Ada.Directories.Base_Name(ArchiveName) & Directory_Separator);
      else
         NewDirectory := To_Unbounded_String(Directory & Directory_Separator);
      end if;
      Create(Tokens, To_String(Selected), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Values :=
           To_Unbounded_String(Item(FilesView, Slice(Tokens, I), "-values"));
         Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
         FileName :=
           To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
         Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 9");
         Path := To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
         if Length(Path) > 0 then
            Append(Path, Directory_Separator);
         end if;
         Create_Path(To_String(NewDirectory & Path));
         Ada.Text_IO.Put_Line
           ("Extracting: " & ArchiveName & " file: " & To_String(Path) &
            To_String(FileName) & " into: " & To_String(NewDirectory & Path));
         Step(ProgressBar);
      end loop;
      Destroy(ProgressDialog);
   end ExtractFile;

end ArchivesViews;
