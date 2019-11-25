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

   -- ****f* MainWindow/DeleteFiles
   -- FUNCTION
   -- Show delete files confirmation dialog and start deleting them on press
   -- the toolbar button
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure DeleteFiles(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/TestArchive
   -- FUNCTION
   -- Test archive and show it result to the user.
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure TestArchive(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/UpdateArchive
   -- FUNCTION
   -- Ask if update archive and if user answer "yes", update it
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure UpdateArchive(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/RecompressArchive
   -- FUNCTION
   -- Ask if recompress archive and if user answer "yes", recompress it.
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure RecompressArchive(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/ChangeView
   -- FUNCTION
   -- Show or hide directory tree view in selected archive view
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure ChangeView(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/CreateMainWindow
   -- FUNCTION
   -- Create main window of the program.
   -- SOURCE
   procedure CreateMainWindow;
   -- ****

end MainWindow;
