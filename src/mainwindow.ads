-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Window; use Gtk.Window;
with Gtkada.MDI; use Gtkada.MDI;

-- ****h* AZipGtk/MainWindow
-- FUNCTION
-- Provides code for the main window of the program
-- SOURCE
package MainWindow is
-- ****

   -- ****v* MainWindow/Window
   -- FUNCTION
   -- Main window of the program.
   -- SOURCE
   Window: Gtk_Window;
   -- ****

   -- ****v* MainWindow/MWindow
   -- FUNCTION
   -- MDI window which contains all open archives data
   -- SOURCE
   MWindow: MDI_Window;
   -- ****

   -- ****f* MainWindow/NewArchive
   -- FUNCTION
   -- Create new archive view.
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was pressed. Unused, can be null.
   -- SOURCE
   procedure NewArchive(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/OpenFile
   -- FUNCTION
   -- Open selected file and show it content to the user.
   -- PARAMETERS
   -- FileName - Full path to the archive which should be open.
   -- SOURCE
   procedure OpenFile(FileName: String);
      -- ****

      -- ****f* MainWindow/DeleteItems
      -- FUNCTION
      -- Remove selected file or directory from selected archive.
      -- PARAMETERS
      -- Model - Gtk_Tree_Model with names of all files and directories in
      --         selected archive
   -- Path  - Gtk_Tree_Path to selected file or directory in selected archive.
      --         Unused.
   -- Iter  - Gtk_Tree_Iter to selected fiel or directory in selected archive.
   -- SOURCE
   procedure DeleteItems
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter);
   -- ****

   -- ****f* MainWindow/CreateMainWindow
   -- FUNCTION
   -- Create main window of the program.
   -- SOURCE
   procedure CreateMainWindow;
   -- ****

end MainWindow;
