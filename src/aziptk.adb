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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with CArgv;
with Interfaces.C;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with AboutDialog; use AboutDialog;
with ArchivesViews; use ArchivesViews;
with FindDialog;
with HelpDialog; use HelpDialog;
with MenuBar; use MenuBar;
with OptionsDialog; use OptionsDialog;
with PropertiesDialog;
with Toolbar; use Toolbar;

procedure AZipTk is

   use type Interfaces.C.int;

   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;
   MainWindow: Tk_Toplevel;
   CurrentDir: constant String := Current_Directory;
   Icon: Tk_Photo;
   pragma Unreferenced(Icon);

begin

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create(Argc, Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable(Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("AzipTk: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("AZipTk: Tcl.Tk.Tk_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Set_Context(Interp);

   -- Load required Tcl packages
   Tooltip_Init(Interp);

   -- Set default type of view for archives
   Tcl.Ada.Tcl_SetVar(Interp, "viewtype", "tree");

   -- Set default type of tiling for archives
   Tcl.Ada.Tcl_SetVar(Interp, "tiletype", "horizontal");

   -- Enable sorting of the files in the archives by default
   Tcl.Ada.Tcl_SetVar(Interp, "nosorting", "0");

   -- List of visible columns
   Tcl.Ada.Tcl_SetVar(Interp, "visiblecolumns", "1 2 3 4 5 6 7 8 9 11 12");

   -- Set color for active archive header
   Style_Configure
     ("aziptk.TFrame", "-background [ttk::style lookup . -selectbackground]");
   Style_Configure
     ("aziptk.TLabel",
      "-background [ttk::style lookup . -selectbackground] -foreground [ttk::style lookup . -selectforeground]");

   Set_Directory(Containing_Directory(Command_Name));
   -- Set the main window
   MainWindow := Get_Main_Window(Interp);
   Wm_Set(MainWindow, "title", "AZip");
   Icon := Create("logo", "-file ""azip.gif""");
   Wm_Set(MainWindow, "iconphoto", "-default logo");
   -- Set keyboard shortcuts for various actions
   Bind_To_Main_Window(Interp, "<Alt-F4>", "{exit}");
   Bind_To_Main_Window(Interp, "<Control-n>", "{Create}");
   Bind_To_Main_Window(Interp, "<Control-o>", "{Load}");
   Bind_To_Main_Window(Interp, "<F12>", "{SaveAs}");
   Bind_To_Main_Window(Interp, "<Control-w>", "{.menubar.file invoke 3}");
   Bind_To_Main_Window(Interp, "<Control-F4>", "{.menubar.file invoke 3}");
   Bind_To_Main_Window(Interp, "<Control-d>", "{ShowProperties}");
   Bind_To_Main_Window(Interp, "<Control-e>", "{Extract}");
   Bind_To_Main_Window(Interp, "<Delete>", "{DeleteItems}");
   Bind_To_Main_Window(Interp, "<minus>", "{DeleteItems}");
   Bind_To_Main_Window(Interp, "<plus>", "{AddFiles false}");
   Bind_To_Main_Window(Interp, "<Control-t>", "{TestArchive}");
   Bind_To_Main_Window(Interp, "<Control-f>", "{ShowFindDialog}");
   Bind_To_Main_Window(Interp, "<Control-p>", "{UpdateArchive}");
   Bind_To_Main_Window(Interp, "<Control-r>", "{RecompressArchive}");
   Bind_To_Main_Window(Interp, "<Control-a>", "{ToggleSelect true}");
   Bind_To_Main_Window(Interp, "<Control-u>", "{ToggleSelect false}");
   Bind_To_Main_Window(Interp, "<F1>", "{ShowHelp}");
   -- Create UI
   CreateMenuBar(MainWindow);
   CreateToolbar;
   CreateMDI;
   CreateAbout;
   CreateOptions;
   CreateHelp;
   FindDialog.AddCommands;
   PropertiesDialog.AddCommands;
   Set_Directory(CurrentDir);

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

exception
      -- Save exception traceback to the file and write it to the console
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
         ErrorText: Unbounded_String := Null_Unbounded_String;
      begin
         Append(ErrorText, Ada.Calendar.Formatting.Image(Clock) & LF);
         Append(ErrorText, "0.1" & LF);
         Append(ErrorText, "Exception: " & Exception_Name(An_Exception) & LF);
         Append(ErrorText, "Message: " & Exception_Message(An_Exception) & LF);
         Append
           (ErrorText,
            "-------------------------------------------------" & LF);
         Append(ErrorText, Symbolic_Traceback_No_Hex(An_Exception));
         Append
           (ErrorText, "-------------------------------------------------");
         if Ada.Directories.Exists("error.log") then
            Open(ErrorFile, Append_File, "error.log");
         else
            Create(ErrorFile, Append_File, "error.log");
         end if;
         Put_Line(ErrorFile, To_String(ErrorText));
         Close(ErrorFile);
         Ada.Text_IO.Put_Line(To_String(ErrorText));
      end;

end AZipTk;
