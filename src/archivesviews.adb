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
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Image.Bitmap; use Tcl.Tk.Ada.Image.Bitmap;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews.Commands;
with ColumnsDialog;
with MenuBar; use MenuBar;
with Toolbar; use Toolbar;

package body ArchivesViews is

   procedure SetActive(NewActive: Positive; Created: Boolean := False) is
      Header: Ttk_Frame;
      OldName: constant String :=
        ".mdi.archive" & Trim(Natural'Image(ActiveArchive), Left);
      NewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(NewActive), Left);
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
      CurrentFilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left) &
           ".filesframe.fileslist");
   end SetActive;

   procedure CreateView is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ArchiveNumber), Left);
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
           (FilesList, "#" & Trim(Natural'Image(I), Left),
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
         "{setactive " & Trim(Positive'Image(ActiveArchive), Left) & "}");
      Bind
        (DirectoryTree, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Left) & "}");
      Bind
        (FilesList, "<1>",
         "{setactive " & Trim(Positive'Image(ActiveArchive), Left) & "}");
      Bind(FilesList, "<3>", "{tk_popup .filesmenu %X %Y}");
      Tcl_SetVar
        (Paned.Interp, "lastindex" & Trim(Positive'Image(ActiveArchive), Left),
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
      CurrentFilesView.Interp := Get_Context;
      MDI := Create(".mdi", "-orient vertical");
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      ArchivesViews.Commands.AddCommands;
      ColumnsDialog.AddCommands;
      CreateView;
   end CreateMDI;

   procedure AddFile(FileName, Path: String; Hide: Boolean := False) is
      FileIndex: Unbounded_String;
   begin
      FileIndex :=
        To_Unbounded_String
          (Tcl_GetVar
             (Get_Context,
              "lastindex" & Trim(Positive'Image(ActiveArchive), Left)));
      -- Some example data. All file data are in values list in order:
      -- Name of the file, type, modified, attributes, size, packed, ratio,
      -- format, crc32, path, name encoding, result
      Insert
        (CurrentFilesView,
         "{} end -id " & To_String(FileIndex) & " -values [list {" &
         Simple_Name(FileName) & "} 2 3 4 5 6 7 8 9 {" & Path & "} 11 12]");
      if Hide then
         Detach(CurrentFilesView, To_String(FileIndex));
      end if;
      Tcl_SetVar
        (Get_Context,
         "lastindex" & Trim(Positive'Image(ActiveArchive), Left),
         Positive'Image(Positive'Value(To_String(FileIndex)) + 1));
   end AddFile;

   function GetArchiveName return String is
      HeaderLabel: Ttk_Label;
   begin
      HeaderLabel.Interp := Get_Context;
      HeaderLabel.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left) &
           ".header.label");
      return cget(HeaderLabel, "-text");
   end GetArchiveName;

   procedure AddFiles
     (FileName: String; Encrypted: Boolean; Path: String := "";
      Hide: Boolean := False) is
      Tokens, Tokens2: Slice_Set;
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
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
      Create(Tokens, FileName, " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Create(Tokens2, Children(CurrentFilesView, "{}"), " ");
         for J in 1 .. Slice_Count(Tokens2) loop
            if Slice(Tokens2, J) /= "" then
               Values :=
                 To_Unbounded_String
                   (Item(CurrentFilesView, Slice(Tokens2, J), "-values"));
               Tcl_Eval
                 (CurrentFilesView.Interp, "lindex {" & To_String(Values) & "} 0");
               ExistingFileName :=
                 To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
               Tcl_Eval
                 (CurrentFilesView.Interp, "lindex {" & To_String(Values) & "} 9");
               ExistingPath :=
                 To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
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
         Heading(CurrentFilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
      ToggleButtons;
   end AddFiles;

   function GetDirectoryView return Ttk_Tree_View is
   begin
      return FilesView: Ttk_Tree_View do
         FilesView.Interp := Get_Context;
         FilesView.Name :=
           New_String
             (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left) &
              ".directoryframe.directorytree");
      end return;
   end GetDirectoryView;

   procedure SaveArchiveAs is
      NewFileName, ArchiveName, Directories: Unbounded_String;
      HeaderLabel: Ttk_Label;
      DirectoryTree: constant Ttk_Tree_View := GetDirectoryView;
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left);
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
      Tokens: Slice_Set;
      Selected, Values, FileName, Path: Unbounded_String;
   begin
      Selected := To_Unbounded_String(Selection(CurrentFilesView));
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
           To_Unbounded_String(Item(CurrentFilesView, Slice(Tokens, I), "-values"));
         Tcl_Eval(CurrentFilesView.Interp, "lindex {" & To_String(Values) & "} 0");
         FileName :=
           To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
         Tcl_Eval(CurrentFilesView.Interp, "lindex {" & To_String(Values) & "} 9");
         Path := To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
         Ada.Text_IO.Put_Line
           ("Deleting file " &
            To_String(Path & Directory_Separator & FileName));
         Delete(CurrentFilesView, Slice(Tokens, I));
      end loop;
   end DeleteItems;

   procedure SortArchive(Column: String) is
      ColumnIndex, OldSortColumn: Natural;
      ArrowName, OldArrowName, Values: Unbounded_String;
      Tokens: Slice_Set;
      Ascending: Boolean := True;
   begin
      Create(Tokens, cget(CurrentFilesView, "-displaycolumns"), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         ArrowName :=
           To_Unbounded_String(Heading(CurrentFilesView, Slice(Tokens, I), "-image"));
         if ArrowName /= Null_Unbounded_String then
            OldSortColumn := Positive'Value(Slice(Tokens, I));
            OldArrowName := ArrowName;
            Heading(CurrentFilesView, Slice(Tokens, I), "-image {}");
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
        (CurrentFilesView, Trim(Natural'Image(ColumnIndex), Left),
         "-image " & To_String(ArrowName));
      Create(Tokens, Children(CurrentFilesView, "{}"), " ");
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
                (Item(CurrentFilesView, Slice(Tokens, I), "-values"));
            Tcl_Eval
              (CurrentFilesView.Interp,
               "lindex {" & To_String(Values) & "}" &
               Natural'Image(ColumnIndex - 1));
            FileEntry.Index := To_Unbounded_String(Slice(Tokens, I));
            FileEntry.Value :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
            FilesList(Positive(I)) := FileEntry;
         end loop;
         Sort(FilesList);
         for I in FilesList'Range loop
            Move
              (CurrentFilesView, To_String(FilesList(I).Index), "{}",
               Natural'Image(I));
         end loop;
      end;
   end SortArchive;

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

   procedure ShowFiles is
      DirectoryTree: constant Ttk_Tree_View := GetDirectoryView;
      Path, ParentId, Selected, Values, FilePath: Unbounded_String :=
        Null_Unbounded_String;
      FlatView: Boolean := False;
   begin
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
      if Tcl_GetVar(CurrentFilesView.Interp, "viewtype") = "flat" then
         FlatView := True;
      else
         for I in
           1 ..
             Positive'Value
               (Tcl_GetVar
                  (CurrentFilesView.Interp,
                   "lastindex" &
                   Trim(Positive'Image(ActiveArchive), Left))) loop
            if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
               Detach(CurrentFilesView, Positive'Image(I));
            end if;
         end loop;
      end if;
      for I in
        1 ..
          Positive'Value
            (Tcl_GetVar
               (CurrentFilesView.Interp,
                "lastindex" & Trim(Positive'Image(ActiveArchive), Left))) loop
         if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(CurrentFilesView, Positive'Image(I), "-values"));
            Tcl_Eval(CurrentFilesView.Interp, "lindex {" & To_String(Values) & "} 9");
            FilePath :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(CurrentFilesView.Interp));
            if (FilePath = Path) or FlatView then
               Move(CurrentFilesView, Positive'Image(I), "{}", Positive'Image(I));
            end if;
         end if;
      end loop;
      -- Sort archive if enabled
      if Tcl_GetVar(Get_Context, "nosorting") = "0" then
         Heading(CurrentFilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
   end ShowFiles;

end ArchivesViews;
