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
with Gtk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Widget; use Gtk.Widget;
with MainWindow; use MainWindow;

package body OptionsDialog is

   -- ****iv* OptionsDialog/DirectoryEntry
   -- FUNCTION
   -- Gtk_GEntry with full path to default directory where archives will be
   -- extracted
   -- SOURCE
   DirectoryEntry: Gtk_GEntry;
   -- ****

   -- ****if* OptionsDialog/ShowDirectories
   -- FUNCTION
   -- Show dialog with directories to select as default directory and
   -- set it if the user press Ok button
   -- PARAMETERS
   -- Self - Gtk_Button pressed. Unused. Can be null.
   -- SOURCE
   procedure ShowDirectories(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      Dialog: constant Gtk_File_Chooser_Dialog :=
        Gtk_File_Chooser_Dialog_New
          ("Select directory", Window, Action_Create_Folder);
   begin
      -- Add Cancel button
      if Add_Button(Dialog, "Cancel", Gtk_Response_Cancel) = null then
         return;
      end if;
      -- Add Ok button
      if Add_Button(Dialog, "OK", Gtk_Response_OK) = null then
         return;
      end if;
      -- Show dialog to the user
      if Run(Dialog) = Gtk_Response_OK then
         -- If button Ok was pressed, set selected directory as
         -- default directory for extracting archives
         Set_Text(DirectoryEntry, Get_Filename(Dialog));
      end if;
      Destroy(Dialog);
   end ShowDirectories;

   procedure ShowOptionsDialog is
      Dialog: constant Gtk_Dialog := Gtk_Dialog_New("Options", Window, Modal);
      Box: constant Gtk_Box := Get_Content_Area(Dialog);
      Frame: constant Gtk_Frame :=
        Gtk_Frame_New
          ("Directory suggested for archive extraction (if empty: archive's location)");
      Button: constant Gtk_Button := Gtk_Button_New_With_Label("Choose");
      HBox: constant Gtk_Hbox := Gtk_Hbox_New;
   begin
      DirectoryEntry := Gtk_Entry_New;
      Set_Text(DirectoryEntry, To_String(DefaultPath));
      -- Center dialog
      Set_Position(Dialog, Win_Pos_Center);
      Pack_Start(HBox, DirectoryEntry, True);
      On_Clicked(Button, ShowDirectories'Access);
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
      if Run(Dialog) = Gtk_Response_OK then
         DefaultPath := To_Unbounded_String(Get_Text(DirectoryEntry));
      end if;
      Destroy(Dialog);
   end ShowOptionsDialog;

end OptionsDialog;
