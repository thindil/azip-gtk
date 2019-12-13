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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Box; use Gtk.Box;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Gtk.File_Filter; use Gtk.File_Filter;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.MDI; use Gtkada.MDI;
with Glib; use Glib;
with ArchivesView; use ArchivesView;
with MainWindow; use MainWindow;
with OptionsDialog; use OptionsDialog;

package body FileDialogs is

   -- ****iv* FileDialogs/CurrentDialog
   -- FUNCTION
   -- Currently showed Dialog. Needed to apply files filter
   -- SOURCE
   CurrentDialog: Gtk_File_Chooser_Dialog;
   -- ****

   procedure ShowDirectoryDialog(Parent: Gtk_Window; Archive: String) is
      Dialog: constant Gtk_File_Chooser_Dialog :=
        Gtk_File_Chooser_Dialog_New
          ("Extract archive", Parent, Action_Select_Folder);
   begin
      -- Add Cancel button
      if Add_Button(Dialog, "Cancel", Gtk_Response_Cancel) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Add Ok button
      if Add_Button(Dialog, "OK", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      if DefaultPath /= Null_Unbounded_String
        and then not Set_Current_Folder(Dialog, To_String(DefaultPath)) then
         Ada.Text_IO.Put_Line
           ("Can't set this directory as a current directory.");
      end if;
      -- Show dialog to the user
      if Run(Dialog) = Gtk_Response_OK then
         -- If button Ok was pressed, extract archive to selected directory
         -- This code is a placeholder, probably whole extraction code should
         -- go here.
         Put_Line("Selected directory: " & Get_Filename(Dialog));
         Put_Line("Archive to extract: " & Archive);
      end if;
      Destroy(Dialog);
   end ShowDirectoryDialog;

   -- ****if* FileDialogs/ApplyFilter
   -- FUNCTION
   -- Apply file filter to selected dialog
   -- PARAMETERS
   -- Self - Combo box with list of file filters
   -- SOURCE
   procedure ApplyFilter(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      FileFilter: constant Gtk_File_Filter := Gtk_File_Filter_New;
   begin
      Add_Pattern(FileFilter, Get_Active_Id(Self));
      Set_Filter(CurrentDialog, FileFilter);
   end ApplyFilter;

   -- ****if* FileDialogs/AddFilter
   -- FUNCTION
   -- Add UI for file filter in file dialogs
   -- SOURCE
   procedure AddFilter is
      -- ****
      FilterCombo: constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
   begin
      -- Add list of file filters
      Append(FilterCombo, "*.zip", "Zip files");
      Append(FilterCombo, "*.jar", "Java files");
      Append(FilterCombo, "*", "All files");
      On_Changed(FilterCombo, ApplyFilter'Access);
      Set_Active(FilterCombo, 0);
      Set_Halign(Gtk_Widget(FilterCombo), Align_End);
      Add(Get_Content_Area(CurrentDialog), Gtk_Widget(FilterCombo));
      Show_All(Gtk_Widget(Get_Content_Area(CurrentDialog)));
   end AddFilter;

   function ShowFileDialog(Parent: Gtk_Window) return String is
   begin
      CurrentDialog :=
        Gtk_File_Chooser_Dialog_New("Select archive", Parent, Action_Open);
      AddFilter;
      -- Add Cancel button
      if Add_Button(CurrentDialog, "Cancel", Gtk_Response_Cancel) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Add Ok button
      if Add_Button(CurrentDialog, "OK", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Show dialog to the user
      if Run(CurrentDialog) = Gtk_Response_OK then
         declare
            ArchiveName: constant String := Get_Filename(CurrentDialog);
         begin
            Destroy(CurrentDialog);
            return ArchiveName;
         end;
      end if;
      Destroy(CurrentDialog);
      return "";
   end ShowFileDialog;

   function ShowSaveDialog
     (Parent: Gtk_Window; Archive: String) return String is
      ArchivePath: Unbounded_String := Null_Unbounded_String;
   begin
      CurrentDialog :=
        Gtk_File_Chooser_Dialog_New("Save archive as", Parent, Action_Save);
      Set_Do_Overwrite_Confirmation(CurrentDialog, True);
      Set_Create_Folders(CurrentDialog, True);
      AddFilter;
      -- Add Cancel button
      if Add_Button(CurrentDialog, "Cancel", Gtk_Response_Cancel) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Add Ok button
      if Add_Button(CurrentDialog, "OK", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Show dialog to the user
      if Run(CurrentDialog) = Gtk_Response_OK then
         -- If button Ok was pressed, save selected archive to selected
         -- directory This code is a placeholder, probably whole compress
         -- or move code should go here.
         ArchivePath := To_Unbounded_String(Get_Filename(CurrentDialog));
         if Length(ArchivePath) < 4
           or else Tail(ArchivePath, 4) /= To_Unbounded_String(".zip") then
            Append(ArchivePath, ".zip");
         end if;
         Put_Line("New full path: " & To_String(ArchivePath));
         Put_Line("Archive to save: " & Archive);
      end if;
      Destroy(CurrentDialog);
      return To_String(ArchivePath);
   end ShowSaveDialog;

   procedure ShowAddFileDialog
     (Parent: Gtk_Window; Encrypted: Boolean := False;
      Directory: Boolean := False) is
      Dialog: Gtk_File_Chooser_Dialog;
      FilesNames: String_SList.GSlist;
   begin
      -- Create dialog with proper title depending if files will be encrypted
      -- or not
      if not Directory then
         if not Encrypted then
            Dialog :=
              Gtk_File_Chooser_Dialog_New("Add file", Parent, Action_Open);
         else
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add file with encryption", Parent, Action_Open);
         end if;
      else
         if not Encrypted then
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add folder", Parent, Action_Select_Folder);
         else
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add folder with encryption", Parent, Action_Select_Folder);
         end if;
      end if;
      Set_Select_Multiple(Dialog, True);
      -- Add Cancel button
      if Add_Button(Dialog, "Cancel", Gtk_Response_Cancel) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Add Ok button
      if Add_Button(Dialog, "OK", Gtk_Response_OK) = null then
         Ada.Text_IO.Put_Line("Can't add button to dialog.");
      end if;
      -- Show dialog to the user
      if Run(Dialog) = Gtk_Response_OK then
         -- If this is a new, empty archive, ask the use under which name save
         -- it
         declare
            MChild: constant MDI_Child := Get_Focus_Child(MWindow);
            MessageDialog: constant Gtk_Message_Dialog :=
              Gtk_Message_Dialog_New
                (Window, Modal, Message_Question, Buttons_Ok,
                 "You'll be asked under which name the archive will be created.");
            NewName: Unbounded_String;
         begin
            if Get_Title(MChild) = "New Archive" then
               Hide(Dialog);
               if Run(MessageDialog) /= Gtk_Response_Yes then
                  Destroy(MessageDialog);
                  NewName :=
                    To_Unbounded_String
                      (ShowSaveDialog(Window, Get_Title(MChild)));
                  if NewName /= Null_Unbounded_String then
                     ChangeName(To_String(NewName));
                  else
                     Destroy(Dialog);
                     return;
                  end if;
               end if;
            end if;
         end;
         FilesNames := Get_Filenames(Dialog);
         for I in 0 .. String_SList.Length(FilesNames) - 1 loop
            AddItem
              (String_SList.Nth_Data(FilesNames, I),
               Containing_Directory(String_SList.Nth_Data(FilesNames, 0)));
         end loop;
         Child_Selected(MWindow, Get_Focus_Child(MWindow));
      end if;
      Destroy(Dialog);
   end ShowAddFileDialog;

end FileDialogs;
