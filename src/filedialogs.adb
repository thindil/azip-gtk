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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.File_Chooser; use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Gtk.File_Filter; use Gtk.File_Filter;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_View; use Gtk.Tree_View;
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

   -- ****iv* FileDialogs/ExtractPath
   -- FUNCTION
   -- In archive path from which files will be extracted. For example, if
   -- ExtractPath is "/" then all files will be extracted. If value is
   -- "/tmp" then all files from directory "tmp" and subdirectories will
   -- be extracted.
   -- SOURCE
   ExtractPath: Unbounded_String;
   -- ****

   -- ****iv* FileDialogs/ArchivePath
   -- FUNCTION
   -- Full path to the selected archive to extract
   -- SOURCE
   ArchivePath: Unbounded_String;
   -- ****

   -- ****iv* FileDialogs/ExtractDirectory
   -- FUNCTION
   -- Full path to the directory where files will be extracted.
   -- SOURCE
   ExtractDirectory: Unbounded_String;
   -- ****

   -- ****if* FileDialogs/ExtractFile
   -- FUNCTION
   -- Extract selected file.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains list of files in selected archive.
   -- Path  - Gtk_Tree_Path to the current file in selected archive. Unused.
   -- Iter  - Gtk_Tree_Iter to the current file in selected archive.
   -- RESULT
   -- This function always returns False.
   -- SOURCE
   function ExtractFile
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      FileName: constant String := Get_String(Model, Iter, 0);
      FilePath: constant String := Get_String(Model, Iter, 9);
   begin
      -- If file is in proper extract path in archive, extract it
      if Index(FilePath, To_String(ExtractPath), 1) = 1 then
         -- This is a placeholder code. Probably whole extraction
         -- code should go here.
         Ada.Text_IO.Put_Line
           ("Extracting: '" & FileName & "' from archive: '" &
            To_String(ArchivePath) & "' from archive path: '" & FilePath &
            "' to directory: '" & To_String(ExtractDirectory) & "'");
      end if;
      return False;
   end ExtractFile;

   procedure ShowDirectoryDialog(Path: String := "/") is
      Dialog: constant Gtk_File_Chooser_Dialog :=
        Gtk_File_Chooser_Dialog_New
          ("Extract archive", Window, Action_Select_Folder);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      FilesList: constant Gtk_List_Store :=
        -(Gtk.Tree_Model_Filter.Get_Model
           (-(Gtk.Tree_Model_Sort.Get_Model
               (-(Get_Model
                   (Gtk_Tree_View
                      (Get_Child
                         (Gtk_Bin
                            (Get_Child2
                               (Gtk_Paned(Get_Widget(MChild))))))))))));
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
         -- If button Ok was pressed, extract archive to selected directory,
         -- each file one by one.
         ExtractPath := To_Unbounded_String(Path);
         ArchivePath := To_Unbounded_String(Get_Title(MChild));
         ExtractDirectory := To_Unbounded_String(Get_Filename(Dialog));
         Foreach(+(FilesList), ExtractFile'Access);
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

   function ShowFileDialog return String is
   begin
      CurrentDialog :=
        Gtk_File_Chooser_Dialog_New("Select archive", Window, Action_Open);
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

   function ShowSaveDialog(Archive: String) return String is
      ArchivePath: Unbounded_String := Null_Unbounded_String;
   begin
      CurrentDialog :=
        Gtk_File_Chooser_Dialog_New("Save archive as", Window, Action_Save);
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
     (Encrypted: Boolean := False; Directory: Boolean := False) is
      Dialog: Gtk_File_Chooser_Dialog;
      FilesNames: String_SList.GSlist;
   begin
      -- Create dialog with proper title depending if files will be encrypted
      -- or not
      if not Directory then
         if not Encrypted then
            Dialog :=
              Gtk_File_Chooser_Dialog_New("Add file", Window, Action_Open);
         else
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add file with encryption", Window, Action_Open);
         end if;
      else
         if not Encrypted then
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add folder", Window, Action_Select_Folder);
         else
            Dialog :=
              Gtk_File_Chooser_Dialog_New
                ("Add folder with encryption", Window, Action_Select_Folder);
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
                    To_Unbounded_String(ShowSaveDialog(Get_Title(MChild)));
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
