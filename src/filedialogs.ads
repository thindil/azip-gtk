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

with Gtk.Window; use Gtk.Window;

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
   -- Parent  - Gtk_Window which will be parent for the dialog. Should be
   --           always main window
   -- Archive - Full path to the archive which will be extracted
   -- SOURCE
   procedure ShowDirectoryDialog(Parent: Gtk_Window; Archive: String);
   -- ****

   -- ****f* FileDialogs/ShowFileDialog
   -- FUNCTION
   -- Show select file dialog.
   -- PARAMETERS
   -- Parent - Gtk_Window which will be parent for the dialog. Should be
   --          always main window
   -- RESULT
   -- Full path to the selected file or empty string if dialog closed
   -- without selection
   -- SOURCE
   function ShowFileDialog(Parent: Gtk_Window) return String;
   -- ****

end FileDialogs;
