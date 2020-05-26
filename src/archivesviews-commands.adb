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
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Toolbar; use Toolbar;
with Utils; use Utils;

package body ArchivesViews.Commands is

   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use type Interfaces.C.int;
      ArchiveName: constant String := ".mdi.archive" & CArgv.Arg(Argv, 1);
      Azip_Close_Archive_Error: exception;
   begin
      if Tcl.Ada.Tcl_Eval(Interp, "destroy " & ArchiveName) = TCL_ERROR then
         raise Azip_Close_Archive_Error with "Can't destroy archive view.";
      end if;
      Tcl.Ada.Tcl_UnsetVar(Interp, "lastindex" & CArgv.Arg(Argv, 1));
      if Panes(MDI) = "" then
         CreateView;
      end if;
      return TCL_OK;
   end Close_Command;

   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      CreateView;
      return TCL_OK;
   end Create_Command;

   function SetActive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function SetActive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      SetActive(Integer'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end SetActive_Command;

   function Close_All_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Close_All_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      use type Interfaces.C.int;
      Tokens: Slice_Set;
      Azip_Close_All_Archives_Error: exception;
   begin
      Create(Tokens, Panes(MDI), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if Tcl.Ada.Tcl_Eval(Interp, "destroy " & Slice(Tokens, I)) =
           TCL_ERROR then
            raise Azip_Close_All_Archives_Error
              with "Can't destroy archive view.";
         end if;
      end loop;
      CreateView;
      return TCL_OK;
   end Close_All_Command;

   function Load_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Load_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      Label: Ttk_Label;
      LabelText: Unbounded_String;
      DirectoryTree, FilesView: Ttk_Tree_View;
      ViewName: Unbounded_String :=
        To_Unbounded_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both));
      FileName: constant String :=
        Get_Open_File
          ("-filetypes {{{Zip archives} {.zip}} {{JAR (Java archives)} {.jar}} {{All files} *}} -title ""Select the archive to open"" -parent . -multiple false");
   begin
      if FileName = "" then
         return TCL_OK;
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
      Insert(DirectoryTree, "{} end -text """ & Simple_Name(FileName) & """");
      Selection_Set
        (DirectoryTree, "[lindex {" & Children(DirectoryTree, "{}") & "} 0]");
      -- Some testing data
      AddFile(FileName, "");
      -- Sort archive if enabled
      FilesView.Interp := Get_Context;
      FilesView.Name :=
        New_String(To_String(ViewName) & ".filesframe.fileslist");
      if Tcl_GetVar(Get_Context, "nosorting") = "0" then
         Heading(FilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
      ToggleButtons;
      return TCL_OK;
   end Load_Command;

   function Extract_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Extract_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      Directory: constant String :=
        (if ExtractingDirectory /= Null_Unbounded_String then
           Choose_Directory
             ("-parent . -title {Extract current folder's content to...} -initialdir {" &
              To_String(ExtractingDirectory) & "}")
         else Choose_Directory
             ("-parent . -title {Extract current folder's content to...}"));
      ArchiveName: constant String := GetArchiveName;
      FilesView, DirectoryTree: Ttk_Tree_View;
      Values, FilePath, Selected, ParentId, Path, FileName, Answer,
      NewDirectory: Unbounded_String;
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      ProgressDialog: Tk_Toplevel :=
        Create(".progressdialog", "-class Dialog");
      ProgressBar: constant Ttk_ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      if Directory = "" then
         Destroy(ProgressDialog);
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      SetDialog(ProgressDialog, "Azip - Extract progress", 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(ViewName & ".directoryframe.directorytree");
      Selected := To_Unbounded_String(Selection(DirectoryTree));
      if Selected = Null_Unbounded_String then
         Destroy(ProgressDialog);
         return TCL_OK;
      end if;
      Answer :=
        To_Unbounded_String
          (MessageBox
             ("-message {Use archive's folder name for output? } -icon question -type yesnocancel"));
      if Answer = To_Unbounded_String("cancel") then
         Destroy(ProgressDialog);
         return TCL_OK;
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
      if Answer = To_Unbounded_String("yes") then
         NewDirectory :=
           To_Unbounded_String
             (Directory & Directory_Separator &
              Ada.Directories.Base_Name(ArchiveName) & Directory_Separator);
      else
         NewDirectory := To_Unbounded_String(Directory & Directory_Separator);
      end if;
      FilesView.Interp := Get_Context;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
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
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 9");
            FilePath :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            if FilePath = Path or
              (Length(FilePath) > Length(Path)
               and then Head(FilePath, Length(Path)) = Path) or
              Path = Null_Unbounded_String then
               if Length(FilePath) > 0 then
                  Append(FilePath, Directory_Separator);
               end if;
               Create_Path(To_String(NewDirectory & FilePath));
               Ada.Text_IO.Put_Line
                 ("Extracting: " & ArchiveName & " file: " &
                  To_String(FilePath) & To_String(FileName) & " into: " &
                  To_String(NewDirectory & FilePath));
               Step(ProgressBar);
            end if;
         end if;
      end loop;
      Destroy(ProgressDialog);
      return TCL_OK;
   end Extract_Command;

   function Add_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Add_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Encrypted: Boolean;
   begin
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         Encrypted := True;
      else
         Encrypted := False;
      end if;
      AddFiles
        (Get_Open_File
           ("-title ""Select the files to add to the archive"" -parent . -multiple true"),
         Encrypted);
      return TCL_OK;
   end Add_Files_Command;

   function Save_As_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Save_As_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      SaveArchiveAs;
      return TCL_OK;
   end Save_As_Command;

   function Delete_Items_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Delete_Items_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      DeleteItems;
      return TCL_OK;
   end Delete_Items_Command;

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      SortArchive(CArgv.Arg(Argv, 1));
      return TCL_OK;
   end Sort_Command;

   function Test_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Test_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
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
         return TCL_OK;
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
      return TCL_OK;
   end Test_Archive_Command;

   function Toggle_View_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Toggle_View_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
   begin
      if CArgv.Arg(Argv, 1) /= "menu" then
         if Tcl.Ada.Tcl_GetVar(Interp, "viewtype") = "flat" then
            Tcl.Ada.Tcl_SetVar(Interp, "viewtype", "tree");
         else
            Tcl.Ada.Tcl_SetVar(Interp, "viewtype", "flat");
         end if;
      end if;
      ToggleView;
      return TCL_OK;
   end Toggle_View_Command;

   function Add_Folder_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Add_Folder_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Encrypted: Boolean;
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
      DirectoryTree: Ttk_Tree_View;
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      MainNode, DirectoryName: Unbounded_String;
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
         FileName: String(1 .. 1_024);
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
                    (Index(DirName, Simple_Name(To_String(DirectoryName))) ..
                         DirName'Last),
                  True);
            end if;
            <<End_Of_Loop>>
         end loop;
         Close(Directory);
      end AddDir;
   begin
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         Encrypted := True;
      else
         Encrypted := False;
      end if;
      DirectoryName :=
        To_Unbounded_String
          (Choose_Directory
             ("-parent . -title ""Select the directory to add to the archive"""));
      if DirectoryName = Null_Unbounded_String then
         return TCL_OK;
      end if;
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         SaveArchiveAs;
         ArchiveName := To_Unbounded_String(GetArchiveName);
         if Length(ArchiveName) > 10
           and then Slice(ArchiveName, 1, 10) = "New Archiv" then
            return TCL_OK;
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
             Simple_Name(To_String(DirectoryName)) then
            if MessageBox
                ("-message {Directory " &
                 Simple_Name(To_String(DirectoryName)) &
                 " exists in the selected archive} -icon error -type ok") /=
              "" then
               return TCL_OK;
            end if;
         end if;
      end loop;
      AddDir
        (To_String(DirectoryName),
         Insert
           (DirectoryTree,
            To_String(MainNode) & " " &
            GetInsertIndex
              (To_String(MainNode), Simple_Name(To_String(DirectoryName))) &
            " -text {" & Simple_Name(To_String(DirectoryName)) & "}"));
      return TCL_OK;
   end Add_Folder_Command;

   function Directory_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Directory_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowFiles;
      return TCL_OK;
   end Directory_Selected_Command;

   function Update_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Update_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      if MessageBox
          ("-message {You are about to start an archive update." & LF &
           "Files than are newer and diffrent (according to their CRC32 code) will replace those in the archive} -icon question -type yesno -detail {Proceed?}") =
        "yes" then
         declare
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
            SetDialog
              (ProgressDialog, "Azip - Update archive progress", 275, 50);
            Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
            FilesView.Interp := Get_Context;
            FilesView.Name :=
              New_String
                (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
                 ".filesframe.fileslist");
            if LastIndex = 1 then
               Destroy(ProgressDialog);
               return TCL_OK;
            end if;
            for I in 1 .. LastIndex loop
               if Exists(FilesView, Positive'Image(I)) = "1" then
                  Values :=
                    To_Unbounded_String
                      (Item(FilesView, Positive'Image(I), "-values"));
                  Tcl_Eval
                    (FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
                  FileName :=
                    To_Unbounded_String
                      (Tcl.Ada.Tcl_GetResult(FilesView.Interp));
                  Ada.Text_IO.Put_Line
                    ("Updating file: " & To_String(FileName));
                  Tcl_Eval
                    (FilesView.Interp,
                     "lrange {" & To_String(Values) & "} 0 10");
                  Values :=
                    To_Unbounded_String
                      (Tcl.Ada.Tcl_GetResult(FilesView.Interp));
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
               return TCL_OK;
            end if;
         end;
      end if;
      return TCL_OK;
   end Update_Archive_Command;

   function Recompress_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Recompress_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      if MessageBox
          ("-message {You are about to recompress this archive." & LF &
           "Contents will remain identical, but data compression may be better. " &
           LF &
           "This operation  can take a long time depending on data size and content.} -icon question -type yesno -detail {Proceed?}") =
        "yes" then
         declare
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
            SetDialog
              (ProgressDialog, "Azip - Update archive progress", 275, 50);
            Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
            FilesView.Interp := Get_Context;
            FilesView.Name :=
              New_String
                (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
                 ".filesframe.fileslist");
            if LastIndex = 1 then
               Destroy(ProgressDialog);
               return TCL_OK;
            end if;
            for I in 1 .. LastIndex loop
               if Exists(FilesView, Positive'Image(I)) = "1" then
                  Values :=
                    To_Unbounded_String
                      (Item(FilesView, Positive'Image(I), "-values"));
                  Tcl_Eval
                    (FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
                  FileName :=
                    To_Unbounded_String
                      (Tcl.Ada.Tcl_GetResult(FilesView.Interp));
                  Ada.Text_IO.Put_Line
                    ("Recompresing file: " & To_String(FileName));
                  Tcl_Eval
                    (FilesView.Interp,
                     "lrange {" & To_String(Values) & "} 0 10");
                  Values :=
                    To_Unbounded_String
                      (Tcl.Ada.Tcl_GetResult(FilesView.Interp));
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
               return TCL_OK;
            end if;
         end;
      end if;
      return TCL_OK;
   end Recompress_Archive_Command;

   function Toggle_Select_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Toggle_Select_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both);
      FilesView: Ttk_Tree_View;
   begin
      FilesView.Interp := Get_Context;
      FilesView.Name := New_String(ViewName & ".filesframe.fileslist");
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         Selection_Set(FilesView, "[list " & Children(FilesView, "{}") & " ]");
      else
         Selection_Set(FilesView, "{}");
      end if;
      return TCL_OK;
   end Toggle_Select_Command;

   function Delete_Directory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Delete_Directory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      DeleteDirectory;
      return TCL_OK;
   end Delete_Directory_Command;

   function Extract_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Extract_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ExtractFile
        (Choose_Directory
           ("-parent . -title {Extract the selected items to...}"));
      return TCL_OK;
   end Extract_File_Command;

   procedure AddCommands is
   begin
      AddCommand("Close", Close_Command'Access);
      AddCommand("Create", Create_Command'Access);
      AddCommand("setactive", SetActive_Command'Access);
      AddCommand("CloseAll", Close_All_Command'Access);
      AddCommand("Load", Load_Command'Access);
      AddCommand("Extract", Extract_Command'Access);
      AddCommand("AddFiles", Add_Files_Command'Access);
      AddCommand("SaveAs", Save_As_Command'Access);
      AddCommand("DeleteItems", Delete_Items_Command'Access);
      AddCommand("Sort", Sort_Command'Access);
      AddCommand("TestArchive", Test_Archive_Command'Access);
      AddCommand("ToggleView", Toggle_View_Command'Access);
      AddCommand("AddFolder", Add_Folder_Command'Access);
      AddCommand("DirectorySelected", Directory_Selected_Command'Access);
      AddCommand("UpdateArchive", Update_Archive_Command'Access);
      AddCommand("RecompressArchive", Recompress_Archive_Command'Access);
      AddCommand("ToggleSelect", Toggle_Select_Command'Access);
      AddCommand("DeleteDirectory", Delete_Directory_Command'Access);
      AddCommand("ExtractFile", Extract_File_Command'Access);
   end AddCommands;

end ArchivesViews.Commands;
