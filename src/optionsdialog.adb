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
with Gtk.Button; use Gtk.Button;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Widget; use Gtk.Widget;
with MainWindow; use MainWindow;

package body OptionsDialog is

   procedure ShowOptionsDialog is
      Dialog: constant Gtk_Dialog := Gtk_Dialog_New("Options", Window, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      Frame: constant Gtk_Frame :=
        Gtk_Frame_New
          ("Directory suggested for archive extraction (if empty: archive's location)");
      GEntry: constant Gtk_GEntry := Gtk_Entry_New;
      Button: constant Gtk_Button := Gtk_Button_New_With_Label("Choose");
      HBox: constant Gtk_Hbox := Gtk_Hbox_New;
   begin
      -- Center dialog
      Set_Position(Dialog, Win_Pos_Center);
      Pack_Start(HBox, GEntry, True);
      Pack_Start(HBox, Button, False);
      Add(Frame, HBox);
      Pack_Start(Box, Frame);
      Show_All(Box);
      -- Add Ok button to dialog
      if Add_Button(Dialog, "Ok", Gtk_Response_OK) = null then
         return;
      end if;
      -- Add Cancel button to dialog
      if Add_Button(Dialog, "Cancel", Gtk_Response_Cancel) = null then
         return;
      end if;
      -- Show dialog to the user
      if Run(Dialog) /= Gtk_Response_Reject then
         Destroy(Dialog);
      end if;
   end ShowOptionsDialog;

end OptionsDialog;
