-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- ****h* AZipGtk/FileDialogs
-- FUNCTION
-- Provide code to show file/directory dialogs and manipulate selected
-- archives
-- SOURCE
package FileDialogs is
-- ****

   -- ****f* FileDialogs/ShowDirectoryDialog
   -- FUNCTION
   -- Show directory dialog and extract selected archive to it.
   -- PARAMETERS
   -- Path - In archive path for files which will be extracted
   -- SOURCE
   procedure ShowDirectoryDialog(Path: String := "/");
   -- ****

   -- ****f* FileDialogs/ShowFileDialog
   -- FUNCTION
   -- Show select file dialog.
   -- RESULT
   -- Full path to the selected file or empty string if dialog closed
   -- without selection
   -- SOURCE
   function ShowFileDialog return String;
   -- ****

   -- ****f* FileDialogs/ShowSaveDialog
   -- FUNCTION
   -- Show save as dialog and save selected archive with that name.
   -- PARAMETERS
   -- Archive - Full path to the archive which will be extracted
   -- RESULT
   -- Full path to the new archive name or empty string in dialog closed
   -- without selection
   -- SOURCE
   function ShowSaveDialog(Archive: String) return String;
   -- ****

   -- ****f* FileDialogs/ShowAddFileDialog
   -- FUNCTION
   -- Show add file dialog and add selected file(s) to selected archive.
   -- PARAMETERS
   -- Encrypted - If True, add file(s) with encryption. Default is False.
   -- Directory - If True, add directory instead file(s). Default is False.
   -- SOURCE
   procedure ShowAddFileDialog
     (Encrypted: Boolean := False; Directory: Boolean := False);
   -- ****

end FileDialogs;
