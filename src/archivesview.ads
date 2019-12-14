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
with Gtkada.MDI; use Gtkada.MDI;

-- ****h* AZipGtk/ArchivesView
-- FUNCTION
-- Provides code to manipulate archives views
-- SOURCE
package ArchivesView is
-- ****

   -- ****t* ArchivesView/ColumnData
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

   -- ****v* ArchivesView/MWindow
   -- FUNCTION
   -- MDI window which contains all open archives data
   -- SOURCE
   MWindow: MDI_Window;
   -- ****

   -- ****v* ArchivesView/Columns
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

   -- ****f* ArchivesView/NewArchive
   -- FUNCTION
   -- Create new archive view.
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was pressed. Unused, can be null.
   -- SOURCE
   procedure NewArchive(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* ArchivesView/OpenFile
   -- FUNCTION
   -- Open selected file and show it content to the user.
   -- PARAMETERS
   -- FileName - Full path to the archive which should be open.
   -- SOURCE
   procedure OpenFile(FileName: String);
   -- ****

   -- ****f* ArchivesView/TreePathToPath
   -- FUNCTION
   -- Convert TreePath from directories tree to file path
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with directories tree
   -- Iter  - Gtk_Tree_Iter to selected directory
   -- RESULT
   -- Full in-archive path to selected directory
   -- SOURCE
   function TreePathToPath
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return String;
   -- ****

   -- ****f* ArchivesView/AddItem
   -- FUNCTION
   -- Add file or directory (with its contents) to the
   -- selected archive
   -- PARAMETERS
   -- Path          - Full path to the file or directory which will be added
   --                 to the archive
   -- MainDirectory - Full path to the directory containing item to add
   -- SOURCE
   procedure AddItem(Path, MainDirectory: String);
   -- ****

end ArchivesView;
