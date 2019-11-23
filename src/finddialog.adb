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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Gtk.Box; use Gtk.Box;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;

package body FindDialog is

   -- ****iv* FindDialog/SearchName
   -- FUNCTION
   -- String to search in the files names in the archive
   -- SOURCE
   SearchName: Unbounded_String;
   -- ****

   -- ****iv* FindDialog/SearchContent
   -- FUNCTION
   -- String to search in the content of the files in the archive
   -- SOURCE
   SearchContent: Unbounded_String;
   -- ****

   -- ****if* FindDialog/SearchItems
   -- FUNCTION
   -- Search inside files names and their contents for selected strings
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains names of files in selected archive
   -- Path  - Gtk_Tree_Path to the current file name (Unused)
   -- Iter  - Gtk_Tree_Iter to the current file name in selected archive
   -- RESULT
   -- This function always returns False, so we can iterate by all element of
   -- the list
   -- SOURCE
   function SearchItems
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      FileName: constant String := Get_String(Model, Iter, 0);
      Result: Gint := 0;
   begin
      -- Just a placeholder to show available data
      Ada.Text_IO.Put_Line("Looking inside: " & FileName);
      Ada.Text_IO.Put_Line("Content to search: " & To_String(SearchContent));
      -- Search for the string in the current file name.
      if Index(FileName, To_String(SearchName)) > 0 then
         Result := 1;
      end if;
      -- Here is probably the best place to search content for SearchContent
      -- string
      -- Set Result value to list, to show it to the user
      Gtk.List_Store.Set(-(Model), Iter, 11, Gint'Image(Result));
      return False;
   end SearchItems;

   procedure ShowFindDialog(Parent: Gtk_Window; Model: Gtk_Tree_Model) is
      Dialog: constant Gtk_Dialog := Gtk_Dialog_New("Find", Parent, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      Label: Gtk_Label;
      NameEntry: constant Gtk_Entry := Gtk_Entry_New;
      ContentEntry: constant Gtk_Entry := Gtk_Entry_New;
   begin
      -- Add buttons to the dialog
      if Add_Button(Dialog, "Search", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      if Add_Button(Dialog, "Cancel", Gtk_Response_Cancel) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Add labels and text entries for searching to the dialog
      Label := Gtk_Label_New("Entry name (if empty: all names):");
      Add(Box, Label);
      Add(Box, NameEntry);
      Label := Gtk_Label_New("Content (if empty: all content):");
      Add(Box, Label);
      Add(Box, ContentEntry);
      -- Show everything and run
      Show_All(Gtk_Widget(Box));
      if Run(Dialog) = Gtk_Response_OK then
         -- If the user press button Search, set everthing and start searching
         SearchName := To_Unbounded_String(Get_Text(NameEntry));
         SearchContent := To_Unbounded_String(Get_Text(ContentEntry));
         Gtk.List_Store.Foreach(-(Model), SearchItems'Access);
      end if;
      Destroy(Gtk_Widget(Dialog));
   end ShowFindDialog;

end FindDialog;
