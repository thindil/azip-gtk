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

with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;

-- ****h* AzipTk/MenuBar
-- FUNCTION
-- Provide code for manipulate the main program menu
-- SOURCE
package MenuBar is
-- ****

   -- ****f* MenuBar/CreateMenuBar
   -- FUNCTION
   -- Create the menu bar and show it to the user
   -- PARAMETERS
   -- MainWindow - The main window of the program
   -- SOURCE
   procedure CreateMenuBar(MainWindow: Tk_Toplevel);
   -- ****

   -- ****f* MenuBar/SetCloseCommand
   -- FUNCTION
   -- Set close command to currently selected archive view
   -- PARAMETERS
   -- Index - Index of the archive view which will be closed by the menu entry
   -- SOURCE
   procedure SetCloseCommand(Index: Positive);
   -- ****

   -- ****f* MenuBar/ToggleEntries
   -- FUNCTION
   -- Enable or disable the menu entries, depends on the state of currently
   -- selected archive view
   -- PARAMETERS
   -- Enable - If true, enable the selected entries in menu. Default value
   -- is True
   -- SOURCE
   procedure ToggleEntries(Enable: Boolean := True);
   -- ****

end MenuBar;
