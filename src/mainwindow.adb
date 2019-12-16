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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Image; use Gtk.Image;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.MDI; use Gtkada.MDI;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with ArchivesView; use ArchivesView;
with FileDialogs; use FileDialogs;
with FindDialog; use FindDialog;
with InfoDialog; use InfoDialog;
with Menu; use Menu;
with OptionsDialog; use OptionsDialog;

package body MainWindow is

   -- ****if* ArchivesView/UpdateToolbar
   -- FUNCTION
   -- Enable or disable toolbar buttons based on state of currently selected
   -- archive
   -- PARAMETERS
   -- Self  - Main MDI_Window which contains all archives windows. Unused.
   -- Child - Currently selected MDI_Child window with archive
   -- SOURCE
   procedure UpdateToolbar
     (Self: access MDI_Window_Record'Class;
      Child: not null access MDI_Child_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      List: constant Gtk_List_Store :=
        -(Gtk.Tree_Model_Filter.Get_Model
           (-(Gtk.Tree_Model_Sort.Get_Model
               (-(Get_Model
                   (Gtk_Tree_View
                      (Get_Child
                         (Gtk_Bin
                            (Get_Child2(Gtk_Paned(Get_Widget(Child))))))))))));
      Enabled: Boolean := True;
      ToolBar: constant Gtk_Toolbar :=
        Gtk_Toolbar(Get_Child(Gtk_Box(Get_Child(Window)), 1));
      Buttons: constant array(1 .. 6) of Gint := (2, 6, 8, 9, 11, 12);
   begin
      -- If currently selected is empty archive, block buttons, otherwise
      -- enable them
      if N_Children(List) = 0 then
         Enabled := False;
      end if;
      for Button of Buttons loop
         Set_Sensitive(Gtk_Widget(Get_Nth_Item(ToolBar, Button)), Enabled);
      end loop;
   end UpdateToolbar;

   -- ****if* MainWindow/ExtractArchive
   -- FUNCTION
   -- Show extract archive dialog on press the proper tool button.
   -- PARAMETERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure ExtractArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowDirectoryDialog;
   end ExtractArchive;

   procedure AddFile(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
   begin
      ShowAddFileDialog;
   end AddFile;

   procedure AddFileEncrypted(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
   begin
      ShowAddFileDialog(True);
   end AddFileEncrypted;

   procedure DeleteFiles(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      use Gtk_Tree_Path_List;
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal, Message_Question, Buttons_Yes_No,
           "Do you want to delete selected item(s)?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      TreeView: constant Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child(Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
      Model: Gtk_Tree_Model;
      SelectedList, List: Glist;
      FileName: Unbounded_String;
      Iter, ListIter: Gtk_Tree_Iter;
   begin
      -- Show dialog with question to the user. If the user answer Yes, start
      -- removing selected files one by one.
      if Run(MessageDialog) = Gtk_Response_Yes then
         Get_Selected_Rows(Get_Selection(TreeView), Model, SelectedList);
         for I in reverse 0 .. Gtk_Tree_Path_List.Length(SelectedList) - 1 loop
            List := Gtk_Tree_Path_List.Nth(SelectedList, I);
            Iter :=
              Gtk.Tree_Model_Sort.Get_Iter
                (-(Model), Gtk.Tree_Model.Convert(Get_Data_Address(List)));
            FileName :=
              To_Unbounded_String
                (Gtk.Tree_Model_Sort.Get_String(-(Model), Iter, 0));
            -- Here probably should go all the code to delete selected file
            -- from the archive. This line is just a placeholder code
            Ada.Text_IO.Put_Line("Deleting: " & To_String(FileName));
            -- Remove file from archive list
            Gtk.Tree_Model_Sort.Convert_Iter_To_Child_Iter
              (-(Model), ListIter, Iter);
            Gtk.Tree_Model_Filter.Convert_Iter_To_Child_Iter
              (-(Gtk.Tree_Model_Sort.Get_Model(-(Model))), ListIter, ListIter);
            Gtk.List_Store.Remove
              (-(Gtk.Tree_Model_Filter.Get_Model
                  (-(Gtk.Tree_Model_Sort.Get_Model(-(Model))))),
               ListIter);
         end loop;
         Child_Selected(MWindow, MChild);
      end if;
      Destroy(MessageDialog);
   end DeleteFiles;

   -- ****iv* MainWindow/ValidArchive
   -- FUNCTION
   -- If true, selected archive is valid. Default is true
   -- SOURCE
   ValidArchive: Boolean := True;
   -- ****

   -- ****if* MainWindow/ValidateArchive
   -- FUNCTION
   -- Validate each file and directory in the archive
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains names of files in selected archive
   -- Path  - Gtk_Tree_Path to the current file name (Unused)
   -- Iter  - Gtk_Tree_Iter to the current file name in selected archive
   -- RESULT
   -- This function always returns False, so we can iterate by all element of
   -- the list
   -- SOURCE
   function ValidateArchive
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      FileName: constant String := Get_String(Model, Iter, 0);
      Valid: Boolean;
      ArchiveName: constant String := Get_Title(Get_Focus_Child(MWindow));
   begin
      -- Here probably should go all code to test/validate archive entries.
      -- If entry is invalid, set Valid to False. Below is some placeholder
      -- code for silencing warnings.
      Put_Line("Archive name: " & ArchiveName);
      Put_Line("Testing entry: " & FileName);
      if FileName(1 .. 2) = " 1" then
         Valid := False;
      else
         Valid := True;
      end if;
      -- Set fields in archive entries list to proper values, depending on
      -- validation result.
      if Valid then
         Gtk.List_Store.Set(-(Model), Iter, 11, "OK");
         Gtk.List_Store.Set(-(Model), Iter, 12, "#00ff00");
      else
         Gtk.List_Store.Set(-(Model), Iter, 11, "Bad");
         Gtk.List_Store.Set(-(Model), Iter, 12, "#ff0000");
         ValidArchive := False;
      end if;
      return False;
   end ValidateArchive;

   procedure TestArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New(Window, Modal, Message_Info, Buttons_Close, "");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Gtk.List_Store.Foreach
        (-(Gtk.Tree_Model_Filter.Get_Model
            (-(Gtk.Tree_Model_Sort.Get_Model
                (-(Get_Model
                    (Gtk_Tree_View
                       (Get_Child
                          (Gtk_Bin
                             (Get_Child2
                                (Gtk_Paned(Get_Widget(MChild)))))))))))),
         ValidateArchive'Access);
      -- Set text to show to the user
      if ValidArchive then
         Set_Markup
           (MessageDialog,
            "All entries in archive " & Get_Title(MChild) & " are OK.");
      else
         Set_Markup
           (MessageDialog,
            "Some entries in archive " & Get_Title(MChild) & " are broken.");
      end if;
      -- Show dialog to the user
      if Run(MessageDialog) = Gtk_Response_Close then
         Destroy(MessageDialog);
      end if;
   end TestArchive;

   -- ****if* MainWindow/Find
   -- FUNCTION
   -- Show find dialog and after, update archive listing with result of search
   -- PARAMETERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure Find(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowFindDialog
        (Window,
         Gtk.Tree_Model_Filter.Get_Model
           (-(Gtk.Tree_Model_Sort.Get_Model
               (-(Get_Model
                   (Gtk_Tree_View
                      (Get_Child
                         (Gtk_Bin
                            (Get_Child2
                               (Gtk_Paned(Get_Widget(MChild))))))))))));
   end Find;

   -- ****if* MainWindow/UpdateSelectedArchive
   -- FUNCTION
   -- Update each file and directory in the selected archive
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains names of files in selected archive
   -- Path  - Gtk_Tree_Path to the current file name (Unused)
   -- Iter  - Gtk_Tree_Iter to the current file name in selected archive
   -- RESULT
   -- This function always returns False, so we can iterate by all element of
   -- the list
   -- SOURCE
   function UpdateSelectedArchive
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      FileName: constant String := Get_String(Model, Iter, 0);
      ArchiveName: constant String := Get_Title(Get_Focus_Child(MWindow));
   begin
      -- Here probably should go all code to update each file and directory
      -- in the selected archive. Below is some placeholder code.
      Put_Line("Archive name: " & ArchiveName);
      Put_Line("Updating entry: " & FileName);
      -- Placeholder code. Here should go all updates for the archive listing.
      -- Columns from 0 to 11: Name, Type, Modified, Attributes, Size, Packed,
      -- Ratio, Format, CRC 32, Path, Name encoding, Result.
      -- Last value is value of color (name, rgb, rgba) which will be used as
      -- background for Result cell. All values are Strings.
      for I in 1 .. 11 loop
         if I /= 9 then
            Gtk.List_Store.Set(-(Model), Iter, Gint(I), Integer'Image(I + 10));
         end if;
      end loop;
      Gtk.List_Store.Set(-(Model), Iter, 12, "rgba(0.0, 0.0, 0.0, 0.0)");
      return False;
   end UpdateSelectedArchive;

   procedure UpdateArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal, Message_Question, Buttons_Yes_No,
           "You are about to start an archive update. Files that are newer and different (according to their CRC32 code) will replace those in the archive. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Gtk.List_Store.Foreach
           (-(Gtk.Tree_Model_Filter.Get_Model
               (-(Gtk.Tree_Model_Sort.Get_Model
                   (-(Get_Model
                       (Gtk_Tree_View
                          (Get_Child
                             (Gtk_Bin
                                (Get_Child2
                                   (Gtk_Paned(Get_Widget(MChild)))))))))))),
            UpdateSelectedArchive'Access);
         -- If you need to do something with archive after upgrading, here
         -- is probably best place to do.
      end if;
      Destroy(MessageDialog);
   end UpdateArchive;

   procedure RecompressArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal, Message_Question, Buttons_Yes_No,
           "You are about to recompress this archive. Contents will remain identical, but data compression may be better. This operation can take a long time depending on data size and content. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         -- Here probably should go all the code related to the recopressing.
         -- This is just a placeholder code.
         Ada.Text_IO.Put_Line("Recompressing: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end RecompressArchive;

   procedure ChangeView(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      TreeView: constant Gtk_Widget :=
        Get_Child1(Gtk_Paned(Get_Widget(MChild)));
      FilesView: constant Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child(Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
   begin
      Set_Visible(TreeView, not Get_Visible(TreeView));
      Set_Visible(Get_Column(FilesView, 9), not Get_Visible(TreeView));
      Refilter(-(Gtk.Tree_Model_Sort.Get_Model(-(Get_Model(FilesView)))));
   end ChangeView;

   procedure ShowInfo(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowInfoDialog(Window, Get_Title(MChild));
   end ShowInfo;

   -- ****if* MainWindow/OpenArchive
   -- FUNCTION
   -- Show file dialog and open selected archive
   -- PARAMETERS
   -- Self - Gtk_Tool_Button pressed. Can be null. Unused.
   -- SOURCE
   procedure OpenArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      OpenFile(ShowFileDialog);
   end OpenArchive;

   -- ****if* MainWindow/ProgramQuit
   -- FUNCTION
   -- Quit from the program and close main GTK loop
   -- PARAMETERS
   -- Self - Gtk_Widget which called this procedure. Ununsed.
   -- SOURCE
   procedure ProgramQuit(Self: access Gtk_Widget_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Main_Quit;
   end ProgramQuit;

   procedure ChangeName(NewName: String) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      Tree: constant Gtk_Tree_Store :=
        -(Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child1(Gtk_Paned(Get_Widget(MChild))))))));
      Iter: constant Gtk_Tree_Iter := Get_Iter_From_String(Tree, "0");
   begin
      Set_Title(MChild, NewName);
      Set(Tree, Iter, 0, Simple_Name(NewName));
   end ChangeName;

   procedure CreateMainWindow is
      Error: GError;
      ToolsIcons: Gdk_Pixbuf;
      Toolbar: constant Gtk_Toolbar := Gtk_Toolbar_New;
      WindowBox: constant Gtk_Vbox := Gtk_Vbox_New;
      procedure AddButton
        (IconStarts: Gint; Label: String; Subprogram: Cb_Gtk_Tool_Button_Void;
         Enabled: Boolean := False) is
         Button: Gtk_Tool_Button;
      begin
         Button :=
           Gtk_Tool_Button_New
             (Gtk_Widget
                (Gtk_Image_New_From_Pixbuf
                   (Gdk_New_Subpixbuf(ToolsIcons, IconStarts, 0, 32, 32))),
              Label);
         On_Clicked(Button, Subprogram);
         Set_Tooltip_Text(Gtk_Widget(Button), Label);
         Set_Sensitive(Button, Enabled);
         Add(Toolbar, Button);
      end AddButton;
   begin
      Window := Gtk_Window_New;
      Set_Title(Window, "AZip");
      Set_Position(Window, Win_Pos_Center);
      Set_Default_Size(Window, 800, 600);
      On_Destroy(Gtk_Widget(Window), ProgramQuit'Access);
      Gdk_New_From_File(ToolsIcons, "az_tools.bmp", Error);
      if Error /= null then
         Put_Line(Get_Message(Error));
         return;
      end if;
      ToolsIcons := Add_Alpha(ToolsIcons, True, 163, 73, 164);
      Gtk_New(MWindow, null);
      On_Child_Selected(MWindow, UpdateToolbar'Access);
      AddButton(320, "New archive", NewArchive'Access, True);
      AddButton(352, "Open archive", OpenArchive'Access, True);
      AddButton(64, "Extract archive", ExtractArchive'Access);
      Add(Toolbar, Gtk_Separator_Tool_Item_New);
      AddButton(0, "Add files...", AddFile'Access);
      AddButton(192, "Add files with encryption...", AddFileEncrypted'Access);
      AddButton(32, "Delete entries", DeleteFiles'Access);
      Add(Toolbar, Gtk_Separator_Tool_Item_New);
      AddButton(128, "Test archive", TestArchive'Access);
      AddButton(96, "Find in archive...", Find'Access);
      Add(Toolbar, Gtk_Separator_Tool_Item_New);
      AddButton(160, "Update archive", UpdateArchive'Access);
      AddButton(288, "Recompress archive", RecompressArchive'Access);
      Add(Toolbar, Gtk_Separator_Tool_Item_New);
      AddButton(384, "Toggle flat/tree view", ChangeView'Access);
      Add(Toolbar, Gtk_Separator_Tool_Item_New);
      AddButton(224, "Properties", ShowInfo'Access);
      CreateMenu(WindowBox);
      Pack_Start(WindowBox, Toolbar, False);
      Pack_Start(WindowBox, Gtk_Widget(MWindow));
      Pack_Start(WindowBox, Gtk_Widget(Gtk_Status_Bar_New), False);
      Add(Gtk_Container(Window), Gtk_Widget(WindowBox));
      Show_All(Gtk_Widget(Window));
      NewArchive(null);
      -- Here probably should go code to set the program settings from the
      -- configuration file (like default path to where extract archives)
      DefaultPath := Null_Unbounded_String;
   end CreateMainWindow;

end MainWindow;
