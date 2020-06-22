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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;

-- ****h* AzipTk/ArchivesViews
-- FUNCTION
-- Provide code to manipulate the archives views
-- SOURCE
package ArchivesViews is
-- ****

   -- ****v* ArchivesViews/ColumnsNames
   -- FUNCTION
   -- The names of the columns in the files view in each archive's view
   -- SOURCE
   ColumnsNames: constant array(1 .. 12) of Unbounded_String :=
     (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
      To_Unbounded_String("Modified"), To_Unbounded_String("Attributes"),
      To_Unbounded_String("Size"), To_Unbounded_String("Packed"),
      To_Unbounded_String("Ratio"), To_Unbounded_String("Format"),
      To_Unbounded_String("CRC 32"), To_Unbounded_String("Path"),
      To_Unbounded_String("Name encoding"), To_Unbounded_String("Result"));
   -- ****

   -- ****v* ArchivesViews/ActiveArchive
   -- FUNCTION
   -- Index of the currently selected archive. Default value is 0
   -- SOURCE
   ActiveArchive: Natural := 0;
   -- ****

   -- ****v* ArchivesViews/ArchiveNumber
   -- FUNCTION
   -- Last index for the archive in the archives list
   -- SOURCE
   ArchiveNumber: Positive;
   -- ****

   -- ****v* ArchivesViews/CurrentLastIndex
   -- FUNCTION
   -- Last index of the file in the currently selected archive. Default value
   -- is 1
   -- SOURCE
   CurrentLastIndex: Positive := 1;
   -- ****

   -- ****v* ArchivesViews/CurrentFilesView (private)
   -- FUNCTION
   -- The Ttk_Tree_View with list of files in the currently selected archive
   -- SOURCE
   CurrentFilesView: Ttk_Tree_View;
   -- ****

   -- ****f* ArchivesViews/CreateMDI
   -- FUNCTION
   -- Create main archives view UI and fill it with one empty archive view
   -- SOURCE
   procedure CreateMDI;
   -- ****

   -- ****f* ArchivesViews/SortArchive
   -- FUNCTION
   -- Sort the selected archive by the selected column. If column is currently
   -- used to sorting, revert sorting
   -- PARAMETERS
   -- Column - The index of the column which will be used for sorting
   -- SOURCE
   procedure SortArchive(Column: String);
   -- ****

   -- ****f* ArchivesViews/ToggleView
   -- FUNCTION
   -- Toggle tree or flat view for the archive's views
   -- SOURCE
   procedure ToggleView;
   -- ****

private

   -- ****v* ArchivesViews/MDI (private)
   -- FUNCTION
   -- Main view for archives views
   -- SOURCE
   MDI: Ttk_PanedWindow;
   -- ****

   -- ****v* ArchivesViews/CurrentDirectoryView (private)
   -- FUNCTION
   -- The Ttk_Tree_View with list of directories in the currently selected
   -- archive
   -- SOURCE
   CurrentDirectoryView: Ttk_Tree_View;
   -- ****

   -- ****f* ArchivesViews/SetActive (private)
   -- FUNCTION
   -- Set the archive as the currently selected
   -- PARAMETERS
   -- NewActive - Index of the archive view to set as the currently active
   -- Created   - If true, the archive was just created. Default value is
   --             false (currently selected archive is existing archive)
   -- SOURCE
   procedure SetActive(NewActive: Positive; Created: Boolean := False);
   -- ****

   -- ****f* ArchivesViews/CreateView (private)
   -- FUNCTION
   -- Create a new, empty archive view
   -- SOURCE
   procedure CreateView;
   -- ****

   -- ****f* ArchivesViews/AddFile (private)
   -- FUNCTION
   -- Add the selected file to the currently selected archive view (not to the
   -- archive itself)
   -- PARAMETERS
   -- FileName - Name of the file to add
   -- Path     - Full path to the file (without its name)
   -- Hide     - If true, the selected file will not be displayed after add.
   --            Default value is false (display it)
   -- SOURCE
   procedure AddFile(FileName, Path: String; Hide: Boolean := False);
   -- ****

   -- ****f* ArchivesViews/GetArchiveName (private)
   -- FUNCTION
   -- Get the name of currently selected archive
   -- SOURCE
   function GetArchiveName return String;
   -- ****

   -- ****f* ArchivesViews/AddFiles (private)
   -- FUNCTION
   -- Add the selected files to the currently selected archive
   -- PARAMETERS
   -- FileName  - Names of the files to add to the archive
   -- Encrypted - If true, add the selected files as encrypted to the archive
   -- Path      - Full path to the directory in archive where the selected
   --             files will be added. Can be empty. Default value is empty
   -- SOURCE
   procedure AddFiles
     (FileName: String; Encrypted: Boolean; Path: String := "";
      Hide: Boolean := False);
   -- ****

   -- ****f* ArchivesViews/SaveArchiveAs
   -- FUNCTION
   -- Save the currently selected archive with as a new file
   -- SOURCE
   procedure SaveArchiveAs;
   -- ****

   -- ****f* ArchivesViews/DeleteItems
   -- FUNCTION
   -- Delete the selected files from the currently selected archive view
   -- SOURCE
   procedure DeleteItems;
   -- ****

   -- ****f* ArchivesViews/ShowFiles
   -- FUNCTION
   -- Show the files which are inside of the selected directory in the current
   -- selected archive
   -- SOURCE
   procedure ShowFiles;
   -- ****

end ArchivesViews;
