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

with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;

package body Utils is

   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Aziptk_Add_Command_Error: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, AdaCommand, 0, null);
      if Command = null then
         raise Aziptk_Add_Command_Error with "Can't add command " & Name;
      end if;
   end AddCommand;

end Utils;
