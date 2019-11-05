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
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
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
      InfoList: constant Gtk_List_Store :=
        Gtk_List_Store_Newv
          ((GType_String, GType_Uint, GType_String, GType_String));
      InfoTree: constant Gtk_Tree_View :=
        Gtk_Tree_View_New_With_Model(+(InfoList));
      CellNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Format (""method"")"),
         To_Unbounded_String("Entries"), To_Unbounded_String("% of data"),
         To_Unbounded_String("Ratio"));
      Area: Gtk_Cell_Area_Box;
      Renderer: Gtk_Cell_Renderer_Text;
      TreeColumn: Gtk_Tree_View_Column;
      Scroll: Gtk_Scrolled_Window;
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
      Set_Headers_Clickable(InfoTree, True);
      for I in 0 .. 3 loop
         Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", Gint(I));
         TreeColumn := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Sort_Column_Id(TreeColumn, Gint(I));
         Set_Title(TreeColumn, To_String(CellNames(I + 1)));
         if Append_Column(InfoTree, TreeColumn) < Gint(I) then
            Ada.Text_IO.Put_Line("Error in adding columns.");
         end if;
      end loop;
      Scroll := Gtk_Scrolled_Window_New;
      Set_Min_Content_Width(Scroll, 400);
      Set_Min_Content_Height(Scroll, 200);
      Add(Gtk_Container(Scroll), Gtk_Widget(InfoTree));
      Add(Box, Scroll);
      Show_All(Gtk_Widget(Box));
      if Run(Dialog) /= Gtk_Response_None then
         Destroy(Gtk_Widget(Dialog));
      end if;
   end ShowInfoDialog;

end InfoDialog;
