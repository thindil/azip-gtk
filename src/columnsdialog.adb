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

with Gtk.Box; use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with MainWindow; use MainWindow;

package body ColumnsDialog is

   procedure ShowColumnsDialog is
      Dialog: constant Gtk_Dialog :=
        Gtk_Dialog_New("Select displayed columns", Window, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      procedure AddButton(Label: String; Enabled: Boolean := True) is
         Button: constant Gtk_Check_Button :=
           Gtk_Check_Button_New_With_Label(Label);
      begin
         Set_Sensitive(Button, Enabled);
         Add(Box, Button);
      end AddButton;
   begin
      Set_Position(Dialog, Win_Pos_Center);
      AddButton("Name", False);
      Show_All(Box);
      if Add_Button(Dialog, "Ok", Gtk_Response_None) = null then
         return;
      end if;
      if Run(Dialog) = Gtk_Response_None then
         Destroy(Dialog);
      end if;
   end ShowColumnsDialog;

end ColumnsDialog;
