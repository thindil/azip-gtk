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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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

   -- ****t* MainWindow/ColumnData
   -- FUNCTION
   -- Data structure for columns in archive list view
   -- PARAMETERS
   -- Name    - Title of column
   -- Visible - If true, column should be visible
   -- SOURCE
   type ColumnData is record
      Name: Unbounded_String;
      Visible: Boolean;
   end record;
   -- ****

   -- ****v* MainWindow/Columns
   -- FUNCTION
   -- Store data for all columns in archives list views. Columns from 0 to 11:
   -- Name, Type, Modified, Attributes, Size, Packed, Ratio, Format, CRC 32,
   -- Path, Name encoding, Result.
   -- SOURCE
   Columns: array(0 .. 11) of ColumnData :=
     ((To_Unbounded_String("Name"), True), (To_Unbounded_String("Type"), True),
      (To_Unbounded_String("Modified"), True),
      (To_Unbounded_String("Attributes"), True),
      (To_Unbounded_String("Size"), True),
      (To_Unbounded_String("Packed"), True),
      (To_Unbounded_String("Ratio"), True),
      (To_Unbounded_String("Format"), True),
      (To_Unbounded_String("CRC 32"), True),
      (To_Unbounded_String("Path"), False),
      (To_Unbounded_String("Name encoding"), True),
      (To_Unbounded_String("Result"), True));
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

   -- ****f* MainWindow/AddFile
   -- FUNCTION
   -- Show add file dialog on press the tool button.
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure AddFile(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/AddFileEncrypted
   -- FUNCTION
   -- Show add file with encryption dialog on press the tool button.
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure AddFileEncrypted(Self: access Gtk_Tool_Button_Record'Class);
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

   -- ****f* MainWindow/ShowInfo
   -- FUNCTION
   -- Show information about selected archive
   -- PARAMERTERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure ShowInfo(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MainWindow/ChangeName
   -- FUNCTION
   -- Change name of currently selected archive
   -- PARAMETERS
   -- NewName - Full path to the new archive
   -- SOURCE
   procedure ChangeName(NewName: String);
   -- ****

   function TreePathToPath
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return String;

   -- ****f* MainWindow/CreateMainWindow
   -- FUNCTION
   -- Create main window of the program.
   -- SOURCE
   procedure CreateMainWindow;
   -- ****

end MainWindow;
