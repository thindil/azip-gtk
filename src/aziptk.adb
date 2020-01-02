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

with Ada.Text_IO;
with CArgv;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;

procedure AZipTk is

   use type Interfaces.C.int;

   Argc: Interfaces.C.int;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;
   Result: Interfaces.C.int;
   Menubar, FileMenu: Menu;
   Toolbar, MDI: Frame;
   Toolbutton: Button;
   pragma Unreferenced(Result);

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

   -- Create UI
   Result := Tcl_Eval(Interp, "wm title . AZip");
   Menubar := Create(".menubar", "-borderwidth 0");
   FileMenu := Create(".menubar.file", "-tearoff false");
   Add(FileMenu, "command", "-label ""New archive""");
   Add(FileMenu, "command", "-label ""Open archive...""");
   Add(FileMenu, "command", "-label ""Save archive as ...""");
   Add(FileMenu, "command", "-label ""Close archive""");
   Add(FileMenu, "separator");
   Add(FileMenu, "command", "-label Properties");
   Add(FileMenu, "separator");
   Add(FileMenu, "command", "-label Recent");
   Add(FileMenu, "separator");
   Add(FileMenu, "command", "-label Quit -command exit");
   Add(Menubar, "cascade", "-menu .menubar.file -label File");
   Add(Menubar, "command", "-label Edit");
   Add(Menubar, "command", "-label Tools");
   Add(Menubar, "command", "-label View");
   Add(Menubar, "command", "-label Options");
   Add(Menubar, "command", "-label Window");
   Add(Menubar, "command", "-label Help");
   Result := Tcl_Eval(Interp, New_String(". configure -menu .menubar"));
   Toolbar := Create(".toolbar", "-relief groove -borderwidth 1");
   Toolbutton := Create(".toolbar.new", "-text New");
   Pack(Toolbutton, "-side left");
   Toolbutton := Create(".toolbar.open", "-text Open");
   Pack(Toolbutton, "-side left");
   Pack(Toolbar, "-fill x");
   MDI := Create(".mdi");
   Pack(MDI, "-fill both -expand true");

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

end AZipTk;
