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

with Tcl.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;

-- ****h* AzipTk/Utils
-- FUNCTION
-- Provides various helper procedures not related directly to any UI
-- SOURCE
package Utils is
-- ****

   -- ****t* Utils/CreateCommands
   -- FUNCTION
   -- Used to create Tcl commands
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   -- ****

   -- ****f* Utils/AddCommand
   -- FUNCTION
   -- Add the selected Ada code as a Tcl command
   -- PARAMETERS
   -- Name       - The name of Tcl command which will be executing the Ada code
   -- AdaCommand - The Ada code which will be executed on the selected Tcl
   --              command
   -- SOURCE
   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc);
   -- ****

   -- ****f* Utils/SetDialog
   -- FUNCTION
   -- Set title, parent and size of the selected dialog
   -- PARAMETERS
   -- Dialog      - Tk_Toplevel widget which will be set as a dialog
   -- DialogTitle - The title of the dialog
   -- Width       - The width of the dialog window
   -- Height      - The height of the dialog window
   -- SOURCE
   procedure SetDialog
     (Dialog: Tk_Toplevel; DialogTitle: String; Width, Height: Positive);
   -- ****

end Utils;
