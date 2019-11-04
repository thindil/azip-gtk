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
with Ada.Text_IO;
with Gtk.Box; use Gtk.Box;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;

package body InfoDialog is

   procedure ShowInfoDialog(Parent: Gtk_Window; FileName: String) is
      Dialog: constant Gtk_Dialog :=
        Gtk_Dialog_New("Archive properties", Parent, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      Grid: constant Gtk_Grid := Gtk_Grid_New;
      Label: Gtk_Label;
      Labels: constant array(0 .. 5) of Unbounded_String :=
        (To_Unbounded_String("Uncompressed size:"),
         To_Unbounded_String("0 bytes"),
         To_Unbounded_String("Compressed size:"),
         To_Unbounded_String("0 bytes"), To_Unbounded_String("Entries:"),
         To_Unbounded_String("0 (empty)"));
      Row, Column: Gint := 0;
   begin
      Ada.Text_IO.Put_Line("Showing info about file: " & FileName);
      if Add_Button(Dialog, "OK", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      Set_Column_Homogeneous(Grid, True);
      for Text of Labels loop
         Label := Gtk_Label_New(To_String(Text));
         Set_Halign(Gtk_Widget(Label), Align_Start);
         Attach(Grid, Label, Column, Row);
         Column := Column + 1;
         if Column = 2 then
            Row := Row + 1;
            Column := 0;
         end if;
      end loop;
      Add(Box, Grid);
      Show_All(Gtk_Widget(Box));
      if Run(Dialog) /= Gtk_Response_None then
         Destroy(Gtk_Widget(Dialog));
      end if;
   end ShowInfoDialog;

end InfoDialog;
