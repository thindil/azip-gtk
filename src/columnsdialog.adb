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

with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.MDI; use Gtkada.MDI;
with Glib; use Glib;
with MainWindow; use MainWindow;

package body ColumnsDialog is

   -- ****if* ColumnsDialog/SetVisibility
   -- FUNCTION
   -- Set visibility of selected column
   -- PARAMETERS
   -- Self - Gtk_Check_Button which was pressed
   -- SOURCE
   procedure SetVisibility(Self: access Gtk_Toggle_Button_Record'Class) is
      -- ****
      Iter: Child_Iterator := First_Child(MWindow);
      MChild: MDI_Child := Get(Iter);
      TreeView: Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child(Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
      Column: Gtk_Tree_View_Column;
   begin
      -- FIXME: change it to array, may crash when no MDI children available
      for I in 0 .. 11 loop
         Column := Get_Column(TreeView, Gint(I));
         if Get_Title(Column) = Get_Label(Self) then
            Set_Visible(Column, Get_Active(Self));
            Columns(I).Visible := Get_Active(Self);
            loop
               Next(Iter);
               MChild := Get(Iter);
               exit when MChild = null;
               TreeView :=
                 Gtk_Tree_View
                   (Get_Child
                      (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
               Column := Get_Column(TreeView, Gint(I));
               Set_Visible(Column, Get_Active(Self));
            end loop;
            return;
         end if;
      end loop;
   end SetVisibility;

   procedure ShowColumnsDialog is
      Dialog: constant Gtk_Dialog :=
        Gtk_Dialog_New("Select displayed columns", Window, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      TreeView: constant Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child(Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
      Column: Gtk_Tree_View_Column;
      Button: Gtk_Check_Button;
   begin
      -- Center dialog
      Set_Position(Dialog, Win_Pos_Center);
      -- Set all buttons, based on columns in archive list
      -- FIXME: change it to array
      for I in 0 .. 11 loop
         Column := Get_Column(TreeView, Gint(I));
         Button := Gtk_Check_Button_New_With_Label(Get_Title(Column));
         if I in 0 | 9 | 11 then
            Set_Sensitive(Button, False);
         end if;
         Set_Active(Button, Get_Visible(Column));
         On_Toggled(Button, SetVisibility'Access);
         Add(Box, Button);
      end loop;
      Show_All(Box);
      -- Add Ok button to dialog
      if Add_Button(Dialog, "Ok", Gtk_Response_None) = null then
         return;
      end if;
      -- Show dialog to the user
      -- FIXME: not working after close dialog with X button
      if Run(Dialog) = Gtk_Response_None then
         Destroy(Dialog);
      end if;
   end ShowColumnsDialog;

end ColumnsDialog;
