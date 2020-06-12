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
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with ProgressDialog; use ProgressDialog;
with Toolbar; use Toolbar;
with Utils; use Utils;

package body ArchivesViews.Commands is

   -- ****if* ACommands/Close_Command
   -- FUNCTION
   -- Close the selected archive view and remove all variables related to it.
   -- If it was the last available an archive view, create a new, empty one
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

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

   -- ****if* ACommands/Create_Command
   -- FUNCTION
   -- Create a new, empty archive view
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      CreateView;
      return TCL_OK;
   end Create_Command;

   -- ****if* ACommands/SetActive_Command
   -- FUNCTION
   -- Set the selected archive view as currently active
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function SetActive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function SetActive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      SetActive(Integer'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end SetActive_Command;

   -- ****if* ACommands/Close_All_Command
   -- FUNCTION
   -- Close all archives views and create a new, empty view
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Close_All_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

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

   -- ****if* ACommands/Load_Command
   -- FUNCTION
   -- Load the selected archive file into the empty archive view. If currently
   -- selected archive view is not empty, create a new one
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Load_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Load_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Label: Ttk_Label;
      LabelText: Unbounded_String;
      ViewName: Unbounded_String :=
        To_Unbounded_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left));
      FileName: constant String :=
        Get_Open_File
          ("-filetypes {{{Zip archives} {.zip}} {{JAR (Java archives)} {.jar}} {{All files} *}} -title ""Select the archive to open"" -parent . -multiple false");
   begin
      -- If user don't selected any file, quit
      if FileName = "" then
         return TCL_OK;
      end if;
      Label.Interp := Interp;
      Label.Name := New_String(To_String(ViewName) & ".header.label");
      LabelText := To_Unbounded_String(cget(Label, "-text"));
      -- If currently selected archive is not empty, create a new one
      if Length(LabelText) > 10
        and then Slice(LabelText, 1, 10) /= "New Archiv" then
         CreateView;
         ViewName :=
           To_Unbounded_String
             (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Left));
         Label.Name := New_String(To_String(ViewName) & ".header.label");
      end if;
      -- Set the label for the archive view
      configure(Label, "-text {" & FileName & "}");
      -- Here should go all directories from the selected archive file. This
      -- one is parent for them all (file name of the archive)
      Insert
        (CurrentDirectoryView, "{} end -text {" & Simple_Name(FileName) & "}");
      Selection_Set
        (CurrentDirectoryView,
         "[lindex {" & Children(CurrentDirectoryView, "{}") & "} 0]");
      -- Some testing data, normally here should go all files from the selected
      -- archive file.
      AddFile(FileName, "");
      -- Sort archive by name if sorting is enabled
      if Tcl_GetVar(Get_Context, "nosorting") = "0" then
         Heading(CurrentFilesView, "1", "-image {}");
         SortArchive("Name");
      end if;
      -- Enable toolbars buttons and menus entries if needed
      ToggleButtons;
      return TCL_OK;
   end Load_Command;

   -- ****if* ACommands/Extract_Command
   -- FUNCTION
   -- Extract the selected directory in the current archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Extract_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

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
      FilePath, Selected, ParentId, Path, FileName, Answer,
      NewDirectory: Unbounded_String;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      -- If no directory selected for extraction, quit
      if Directory = "" then
         return TCL_OK;
      end if;
      Selected := To_Unbounded_String(Selection(CurrentDirectoryView));
      -- If no directory selected in the current archive, quit
      if Selected = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Answer :=
        To_Unbounded_String
          (MessageBox
             ("-message {Use archive's folder name for output? } -icon question -type yesnocancel"));
      -- Quit if the user selected cancel option
      if Answer = To_Unbounded_String("cancel") then
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      CreateProgressDialog("Extract progress");
      -- Set the path for the files which will be extracted
      loop
         ParentId :=
           To_Unbounded_String
             (Parent(CurrentDirectoryView, To_String(Selected)));
         exit when ParentId = To_Unbounded_String("");
         if Path /= Null_Unbounded_String then
            Path := Directory_Separator & Path;
         end if;
         Path :=
           To_Unbounded_String
             (Item(CurrentDirectoryView, To_String(Selected), "-text")) &
           Path;
         Selected := ParentId;
      end loop;
      -- Use archive folder name as an output folder for extraction
      if Answer = To_Unbounded_String("yes") then
         NewDirectory :=
           To_Unbounded_String
             (Directory & Directory_Separator &
              Ada.Directories.Base_Name(ArchiveName) & Directory_Separator);
      else
         NewDirectory := To_Unbounded_String(Directory & Directory_Separator);
      end if;
      -- Extract all files from the selected folder
      for I in 1 .. CurrentLastIndex loop
         if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
            FileName :=
              To_Unbounded_String
                (Set(CurrentFilesView, Positive'Image(I), "1"));
            FilePath :=
              To_Unbounded_String
                (Set(CurrentFilesView, Positive'Image(I), "10"));
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
               UpdateProgress;
            end if;
         end if;
      end loop;
      DeleteProgressDialog;
      return TCL_OK;
   end Extract_Command;

   -- ****if* ACommands/Add_Files_Command
   -- FUNCTION
   -- Add the selected files (with or without encryption) to the currently
   -- selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Add_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Encrypted: constant Boolean :=
        (if
           CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
           CArgv.Arg(Argv, 1) = "yes"
         then True
         else False);
   begin
      AddFiles
        (Get_Open_File
           ("-title ""Select the files to add to the archive"" -parent . -multiple true"),
         Encrypted);
      return TCL_OK;
   end Add_Files_Command;

   -- ****if* ACommands/Save_As_Command
   -- FUNCTION
   -- Save the currently selected archive with a new name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Save_As_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Save_As_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      SaveArchiveAs;
      return TCL_OK;
   end Save_As_Command;

   -- ****if* ACommands/Delete_Items_Command
   -- FUNCTION
   -- Delete the selected files from the currently selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Delete_Items_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Items_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      DeleteItems;
      return TCL_OK;
   end Delete_Items_Command;

   -- ****if* ACommands/Sort_Command
   -- FUNCTION
   -- Sort the currently selected archive files list
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      SortArchive(CArgv.Arg(Argv, 1));
      return TCL_OK;
   end Sort_Command;

   -- ****if* ACommands/Test_Archive_Command
   -- FUNCTION
   -- Test the currently selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Test_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Test_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      FileName: Unbounded_String;
   begin
      -- If the currently selected archive is empty, quit
      if CurrentLastIndex = 1 then
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      CreateProgressDialog("Test archive progress");
      -- Test the files in the selected archive
      for I in 1 .. CurrentLastIndex loop
         if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
            FileName :=
              To_Unbounded_String
                (Set(CurrentFilesView, Positive'Image(I), "1"));
            Ada.Text_IO.Put_Line("Testing file: " & To_String(FileName));
            Set(CurrentFilesView, Positive'Image(I), "12", "OK");
            UpdateProgress;
         end if;
      end loop;
      DeleteProgressDialog;
      return TCL_OK;
   end Test_Archive_Command;

   -- ****if* ACommands/Toggle_View_Command
   -- FUNCTION
   -- Toggle flat or tree view for all archives views
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Toggle_View_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

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

   -- ****if* ACommands/Add_Folder_Command
   -- FUNCTION
   -- Add the selected directory and its contents to the selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Add_Folder_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Folder_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Encrypted: constant Boolean :=
        (if
           CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
           CArgv.Arg(Argv, 1) = "yes"
         then True
         else False);
      ArchiveName: Unbounded_String := To_Unbounded_String(GetArchiveName);
      MainNode, DirectoryName: Unbounded_String;
      Tokens: Slice_Set;
      function GetInsertIndex(Parent, DirName: String) return String is
         Tokens2: Slice_Set;
      begin
         Create(Tokens2, Children(CurrentDirectoryView, Parent), " ");
         for I in 1 .. Slice_Count(Tokens2) loop
            if Slice(Tokens2, I) /= ""
              and then Item(CurrentDirectoryView, Slice(Tokens2, I), "-text") >
                DirName then
               return Index(CurrentDirectoryView, Slice(Tokens2, I));
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
            -- If the selected child is directory, go to it and add its content
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
                      (CurrentDirectoryView,
                       Parent & " " &
                       GetInsertIndex
                         (Parent, Simple_Name(FileName(1 .. Last))) &
                       " -text {" & Simple_Name(FileName(1 .. Last)) & "}"));
               AddDir
                 (DirName & Directory_Separator & FileName(1 .. Last),
                  To_String(NewParentIndex));
            -- Add the file to the archive
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
      DirectoryName :=
        To_Unbounded_String
          (Choose_Directory
             ("-parent . -title ""Select the directory to add to the archive"""));
      if DirectoryName = Null_Unbounded_String then
         return TCL_OK;
      end if;
      -- If the selected archive isn't saved yet, save it first
      if Length(ArchiveName) > 10
        and then Slice(ArchiveName, 1, 10) = "New Archiv" then
         SaveArchiveAs;
         ArchiveName := To_Unbounded_String(GetArchiveName);
         if Length(ArchiveName) > 10
           and then Slice(ArchiveName, 1, 10) = "New Archiv" then
            return TCL_OK;
         end if;
      end if;
      MainNode := To_Unbounded_String(Children(CurrentDirectoryView, "{}"));
      -- Check if the selected directory isn't exists in the selected archive
      Create(Tokens, Children(CurrentDirectoryView, To_String(MainNode)), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if Slice(Tokens, I) /= ""
           and then Item(CurrentDirectoryView, Slice(Tokens, I), "-text") =
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
      -- Add recursively the selected archive to the selected directory
      AddDir
        (To_String(DirectoryName),
         Insert
           (CurrentDirectoryView,
            To_String(MainNode) & " " &
            GetInsertIndex
              (To_String(MainNode), Simple_Name(To_String(DirectoryName))) &
            " -text {" & Simple_Name(To_String(DirectoryName)) & "}"));
      return TCL_OK;
   end Add_Folder_Command;

   -- ****if* ACommands/Directory_Selected_Command
   -- FUNCTION
   -- Show files in the selected directory when an user select new directory
   -- from the archive view directory list
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Directory_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Directory_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowFiles;
      return TCL_OK;
   end Directory_Selected_Command;

   -- ****if* ACommands/Update_Archive_Command
   -- FUNCTION
   -- Trigger update the selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Update_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      FileName: Unbounded_String;
   begin
      -- Confirm if update should be done
      if MessageBox
          ("-message {You are about to start an archive update." & LF &
           "Files than are newer and diffrent (according to their CRC32 code) will replace those in the archive} -icon question -type yesno -detail {Proceed?}") =
        "no" then
         return TCL_OK;
      end if;
      -- Don't update if the currenlty selected archive is empty
      if CurrentLastIndex = 1 then
         return TCL_OK;
      end if;
      -- Update each file in the selected archive
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      CreateProgressDialog("Update archive progress");
      for I in 1 .. CurrentLastIndex loop
         if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
            FileName :=
              To_Unbounded_String
                (Set(CurrentFilesView, Positive'Image(I), "1"));
            Ada.Text_IO.Put_Line("Updating file: " & To_String(FileName));
            Set(CurrentFilesView, Positive'Image(I), "12", "OK");
            UpdateProgress;
         end if;
      end loop;
      DeleteProgressDialog;
      -- Show update status to the user
      if MessageBox
          ("-message {Update completed.} -icon info -type ok -detail {No entry needed to be updated.}") =
        "" then
         return TCL_OK;
      end if;
      return TCL_OK;
   end Update_Archive_Command;

   -- ****if* ACommands/Recompress_Archive_Command
   -- FUNCTION
   -- Trigger recompressing the selected archive
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Recompress_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Recompress_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      FileName: Unbounded_String;
   begin
      -- Confirm if recompression should be done
      if MessageBox
          ("-message {You are about to recompress this archive." & LF &
           "Contents will remain identical, but data compression may be better. " &
           LF &
           "This operation  can take a long time depending on data size and content.} -icon question -type yesno -detail {Proceed?}") =
        "no" then
         return TCL_OK;
      end if;
      -- Don't recompress if the currently selected archive is empty
      if CurrentLastIndex = 1 then
         return TCL_OK;
      end if;
      -- Recompress every file in the selected archive
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      CreateProgressDialog("Update archive progress");
      for I in 1 .. CurrentLastIndex loop
         if Exists(CurrentFilesView, Positive'Image(I)) = "1" then
            FileName :=
              To_Unbounded_String
                (Set(CurrentFilesView, Positive'Image(I), "1"));
            Ada.Text_IO.Put_Line("Recompresing file: " & To_String(FileName));
            Set(CurrentFilesView, Positive'Image(I), "12", "OK");
            UpdateProgress;
         end if;
      end loop;
      DeleteProgressDialog;
      -- Show recompress status to the user
      if MessageBox
          ("-message {Recompression completed.} -icon info -type ok -detail {No entry could be recompressed to a smaller size.}") =
        "" then
         return TCL_OK;
      end if;
      return TCL_OK;
   end Recompress_Archive_Command;

   -- ****if* ACommands/Toggle_Select_Command
   -- FUNCTION
   -- Toggle selection (all or none) of files in the selected archive view
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Toggle_Select_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Select_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         Selection_Set
           (CurrentFilesView,
            "[list " & Children(CurrentFilesView, "{}") & " ]");
      else
         Selection_Set(CurrentFilesView, "{}");
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
      Selection_Set
        (CurrentFilesView, "[list " & Children(CurrentFilesView, "{}") & " ]");
      DeleteItems;
      Delete(CurrentDirectoryView, Selection(CurrentDirectoryView));
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
      ArchiveName: constant String := GetArchiveName;
      Path, Selected, FileName, NewDirectory, Answer,
      Directory: Unbounded_String;
      Tokens: Slice_Set;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      Directory :=
        To_Unbounded_String
          (Choose_Directory
             ("-parent . -title {Extract the selected items to...}"));
      Selected := To_Unbounded_String(Selection(CurrentFilesView));
      if Selected = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Answer :=
        To_Unbounded_String
          (MessageBox
             ("-message {Use archive's folder name for output? } -icon question -type yesnocancel"));
      if Answer = To_Unbounded_String("cancel") then
         return TCL_OK;
      end if;
      if Answer = To_Unbounded_String("yes") then
         NewDirectory :=
           Directory &
           To_Unbounded_String
             (Directory_Separator & Ada.Directories.Base_Name(ArchiveName) &
              Directory_Separator);
      else
         NewDirectory := Directory & Directory_Separator;
      end if;
      CreateProgressDialog("Extract progress");
      Create(Tokens, To_String(Selected), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         FileName :=
           To_Unbounded_String(Set(CurrentFilesView, Slice(Tokens, I), "1"));
         Path :=
           To_Unbounded_String(Set(CurrentFilesView, Slice(Tokens, I), "10"));
         if Length(Path) > 0 then
            Append(Path, Directory_Separator);
         end if;
         Create_Path(To_String(NewDirectory & Path));
         Ada.Text_IO.Put_Line
           ("Extracting: " & ArchiveName & " file: " & To_String(Path) &
            To_String(FileName) & " into: " & To_String(NewDirectory & Path));
         UpdateProgress;
      end loop;
      DeleteProgressDialog;
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
