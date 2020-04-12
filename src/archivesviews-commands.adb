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
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with AboutDialog; use AboutDialog;

package body ArchivesViews.Commands is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

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
   begin
      if Tcl.Ada.Tcl_Eval(Interp, "destroy " & ArchiveName) = TCL_ERROR then
         raise Program_Error with "Can't destroy archive view.";
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
   begin
      Create(Tokens, Panes(MDI), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         if Tcl.Ada.Tcl_Eval(Interp, "destroy " & Slice(Tokens, I)) =
           TCL_ERROR then
            raise Program_Error with "Can't destroy archive view.";
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
   begin
      LoadArchive
        (Get_Open_File
           ("-filetypes {{{Zip archives} {.zip}} {{JAR (Java archives)} {.jar}} {{All files} *}} -title ""Select the archive to open"" -parent . -multiple false"));
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
   begin
      ExtractArchive
        (Choose_Directory
           ("-parent . -title ""Select directory to which extract the archive"""));
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
   begin
      TestArchive;
      return TCL_OK;
   end Test_Archive_Command;

   function Show_Find_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_Find_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowFindDialog;
      return TCL_OK;
   end Show_Find_Dialog_Command;

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Dialog: Tk_Toplevel;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      Dialog.Interp := MainWindow.Interp;
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

   function Find_In_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Find_In_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      FindInArchive;
      return TCL_OK;
   end Find_In_Archive_Command;

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
   begin
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         Encrypted := True;
      else
         Encrypted := False;
      end if;
      AddDirectory
        (Choose_Directory
           ("-parent . -title ""Select the directory to add to the archive"""),
         Encrypted);
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
         UpdateArchive;
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
         RecompressArchive;
      end if;
      return TCL_OK;
   end Recompress_Archive_Command;

   function Show_Properties_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_Properties_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowProperties;
      return TCL_OK;
   end Show_Properties_Command;

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowAbout;
      return TCL_OK;
   end Show_About_Command;

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
   begin
      if CArgv.Arg(Argv, 1) = "1" or CArgv.Arg(Argv, 1) = "true" or
        CArgv.Arg(Argv, 1) = "yes" then
         ToggleSelect(True);
      else
         ToggleSelect(False);
      end if;
      return TCL_OK;
   end Toggle_Select_Command;

   procedure AddCommands is
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise Program_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
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
      AddCommand("ShowFindDialog", Show_Find_Dialog_Command'Access);
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("FindInArchive", Find_In_Archive_Command'Access);
      AddCommand("ToggleView", Toggle_View_Command'Access);
      AddCommand("AddFolder", Add_Folder_Command'Access);
      AddCommand("DirectorySelected", Directory_Selected_Command'Access);
      AddCommand("UpdateArchive", Update_Archive_Command'Access);
      AddCommand("RecompressArchive", Recompress_Archive_Command'Access);
      AddCommand("ShowProperties", Show_Properties_Command'Access);
      AddCommand("ShowAbout", Show_About_Command'Access);
      AddCommand("ToggleSelect", Toggle_Select_Command'Access);
   end AddCommands;

end ArchivesViews.Commands;
