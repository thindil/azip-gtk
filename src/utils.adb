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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;

package body Utils is

   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
      Command: constant Tcl.Tcl_Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, AdaCommand, 0, null);
      Aziptk_Add_Command_Error: exception;
   begin
      if Command = null then
         raise Aziptk_Add_Command_Error with "Can't add command " & Name;
      end if;
   end AddCommand;

   procedure SetDialog
     (Dialog: Tk_Toplevel; DialogTitle: String; Width, Height: Positive) is
      X, Y: Integer;
   begin
      Wm_Set(Dialog, "title", "{" & DialogTitle & "}");
      Wm_Set(Dialog, "transient", ".");
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(Dialog, "attributes", "-type dialog");
      end if;
      X := (Positive'Value(Winfo_Get(Dialog, "vrootwidth")) - Width) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(Dialog, "vrootheight")) - Height) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Dialog, "geometry",
         Trim(Positive'Image(Width), Both) & "x" &
         Trim(Positive'Image(Height), Both) & "+" &
         Trim(Positive'Image(X), Both) & "+" & Trim(Positive'Image(Y), Both));
      Bind(Dialog, "<Destroy>", "{CloseDialog " & Value(Dialog.Name) & "}");
   end SetDialog;

end Utils;
