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

with Interfaces.C;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;

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
         if Panes(MDI) = "" then
            CreateView;
         end if;
         return 0;
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
         return 0;
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
         return 0;
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
         return 0;
      end Close_All_Command;

   procedure AddCommands is
      Command: Tcl.Tcl_Command;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "Close", Close_Command'Access, 0, null);
      if Command = null then
         raise Program_Error with "Can't add command Close";
      end if;
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "Create", Create_Command'Access, 0, null);
      if Command = null then
         raise Program_Error with "Can't add command Create";
      end if;
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "setactive", SetActive_Command'Access, 0, null);
      if Command = null then
         raise Program_Error with "Can't add command setactive";
      end if;
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, "CloseAll", Close_All_Command'Access, 0, null);
      if Command = null then
         raise Program_Error with "Can't add command CloseAll";
      end if;
   end AddCommands;

end ArchivesViews.Commands;
