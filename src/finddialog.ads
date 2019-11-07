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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Window; use Gtk.Window;

-- ****h* AzipGtk/FindDialog
-- FUNCTION
-- Provide code to show find dialog and search inside files
-- SOURCE
package FindDialog is
-- ****

   -- ****f* FindDialog/ShowFindDialog
   -- FUNCTION
   -- Show Find dialog to the user and search in selected archive
   -- PARAMETERS
   -- Parent - Gtk_Window which will be parent for the dialog. Should be always
   --          main window.
   -- Model  - Gtk_Tree_Model with files names from the selected archive
   -- SOURCE
   procedure ShowFindDialog(Parent: Gtk_Window; Model: Gtk_Tree_Model);
   -- ****

end FindDialog;
