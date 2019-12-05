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

with Gtk.About_Dialog; use Gtk.About_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Widget; use Gtk.Widget;
with Glib.Error; use Glib.Error;
with Gdk.Pixbuf; use Gdk.Pixbuf;

package body AboutDialog is

   procedure ShowAboutDialog(Parent: Gtk_Window) is
      AboutDialog: constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      Logo: Gdk_Pixbuf;
      Error: GError;
   begin
      -- Set parent for dialog
      Set_Transient_For(Gtk_Window(AboutDialog), Parent);
      -- Set various information about the program (for more options, look at
      -- documenation of Gtk.About_Dialog package)
      Set_Program_Name(AboutDialog, "AZip");
      Set_License_Type(AboutDialog, License_Mit_X11);
      Set_Copyright(AboutDialog, "(c) 2019 Gautier de Montmollin");
      Gdk_New_From_File(Logo, "azip.ico", Error);
      Set_Logo(AboutDialog, Logo);
      Set_Authors
        (AboutDialog,
         (new String'
            ("Gautier de Montmollin <gautier.de.montmollin@gmail.com>"),
          new String'("")));
      -- Show dialog to the user
      if Run(Gtk_Dialog(AboutDialog)) = Gtk_Response_Delete_Event then
         Destroy(Gtk_Widget(AboutDialog));
      end if;
   end ShowAboutDialog;

end AboutDialog;
