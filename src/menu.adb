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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Recent_Chooser; use Gtk.Recent_Chooser;
with Gtk.Recent_Chooser_Menu; use Gtk.Recent_Chooser_Menu;
with Gtk.Recent_Filter; use Gtk.Recent_Filter;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.MDI; use Gtkada.MDI;
with AboutDialog; use AboutDialog;
with ColumnsDialog; use ColumnsDialog;
with FileDialogs; use FileDialogs;
with FindDialog; use FindDialog;
with HelpDialog; use HelpDialog;
with MainWindow; use MainWindow;
with OptionsDialog; use OptionsDialog;

package body Menu is

   -- ****if* Menu/NewArchiveMenu
   -- FUNCTION
   -- Create new archive view.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure NewArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      NewArchive(null);
   end NewArchiveMenu;

   -- ****if* Menu/OpenArchiveMenu
   -- FUNCTION
   -- Show open archive dialog and show selected archive to the user.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure OpenArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      OpenFile(ShowFileDialog(Window));
   end OpenArchiveMenu;

   -- ****if* Menu/SaveArchiveMenu
   -- FUNCTION
   -- Show save selected archive dialog and then save it with new name (or
   -- old, if user want).
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure SaveArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowSaveDialog(Window, Get_Title(MChild));
   end SaveArchiveMenu;

   -- ****if* Menu/CloseArchiveMenu
   -- FUNCTION
   -- Close selected archive view.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure CloseArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if MChild = null then
         return;
      end if;
      Close_Child(MChild);
   end CloseArchiveMenu;

   -- ****if* Menu/QuitMenu
   -- FUNCTION
   -- Quit from the program.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure QuitMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Main_Quit;
   end QuitMenu;

   -- ****if* Menu/ExtractArchiveMenu
   -- FUNCTION
   -- Extract selected archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure ExtractArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowDirectoryDialog(Window, Get_Title(MChild));
   end ExtractArchiveMenu;

   -- ****if* Menu/DeleteFiles
   -- FUNCTION
   -- Delete selected files from the archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure DeleteFiles(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      MainWindow.DeleteFiles(null);
   end DeleteFiles;

   -- ****if* Menu/AddFileMenu
   -- FUNCTION
   -- Show add file dialog and add selected file(s) to the archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure AddFileMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      AddFile(null);
   end AddFileMenu;

   -- ****if* Menu/AddFileEncryptionMenu
   -- FUNCTION
   -- Show add file dialog and add selected file(s) to the archive with
   -- encryption.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure AddFileEncryptionMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      AddFileEncrypted(null);
   end AddFileEncryptionMenu;

   -- ****if* Menu/AddFolderMenu
   -- FUNCTION
   -- Show add directory dialog and add selected directory content to the
   -- archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure AddFolderMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowAddFileDialog
        (Window,
         -(Get_Model
            (Gtk_Tree_View
               (Get_Child
                  (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
         False, True);
   end AddFolderMenu;

   -- ****if* Menu/AddFolderEncryptionMenu
   -- FUNCTION
   -- Show add directory dialog and add selected directory content with
   -- encryption to the archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure AddFolderEncryptionMenu
     (Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowAddFileDialog
        (Window,
         -(Get_Model
            (Gtk_Tree_View
               (Get_Child
                  (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
         True, True);
   end AddFolderEncryptionMenu;

   -- ****if* Menu/TestArchiveMenu
   -- FUNCTION
   -- Show result of test of the selected archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure TestArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      TestArchive(null);
   end TestArchiveMenu;

   -- ****if* Menu/FindMenu
   -- FUNCTION
   -- Show find dialog and then result of searching in the archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure FindMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowFindDialog
        (Window,
         Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end FindMenu;

   -- ****if* Menu/UpdateArchiveMenu
   -- FUNCTION
   -- Update selected archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure UpdateArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      UpdateArchive(null);
   end UpdateArchiveMenu;

   -- ****if* Menu/RecompressArchiveMenu
   -- FUNCTION
   -- Recompress selected archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure RecompressArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      RecompressArchive(null);
   end RecompressArchiveMenu;

   -- ****if* Menu/ChangeViewMenu
   -- FUNCTION
   -- Show or hide tree view for the selected archive
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure ChangeViewMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ChangeView(null);
   end ChangeViewMenu;

   -- ****if* Menu/SplitWindow
   -- FUNCTION
   -- Set how to split archives view. All new archives will be show in that
   -- orientation.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Used to determine how
   -- view will be splitted
   -- SOURCE
   procedure SplitWindow(Self: access Gtk_Menu_Item_Record'Class) is
   -- ****
   begin
      if Get_Label(Self) = "Tile _Horizontal" then
         Orientation := Orientation_Horizontal;
      else
         Orientation := Orientation_Vertical;
      end if;
      Split(MWindow, Orientation);
   end SplitWindow;

   -- ****if* Menu/CloseAll
   -- FUNCTION
   -- Close all open archives
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure CloseAll(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: MDI_Child;
   begin
      loop
         MChild := Get_Focus_Child(MWindow);
         exit when MChild = null;
         Close_Child(MChild);
      end loop;
   end CloseAll;

   -- ****if* Menu/ShowAbout
   -- FUNCTION
   -- Show dialog with information about the program
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure ShowAbout(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowAboutDialog(Window);
   end ShowAbout;

   -- ****if* Menu/OpenRecent
   -- FUNCTION
   -- Open selected in recent menu archive
   -- Self - Menu with recently used archives
   -- SOURCE
   procedure OpenRecent(Self: Gtk_Recent_Chooser) is
      -- ****
      Uri: constant String := Get_Current_Uri(Self);
   begin
      OpenFile(Uri(8 .. Uri'Length));
   end OpenRecent;

   -- ****if* Menu/SelectAll
   -- FUNCTION
   -- Select all files in selected archive
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure SelectAll(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Select_All
        (Get_Selection
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end SelectAll;

   -- ****if* Menu/UnselectAll
   -- FUNCTION
   -- Unselect all selected files in currently selected archive.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure UnselectAll(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Unselect_All
        (Get_Selection
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end UnselectAll;

   -- ****if* Menu/PropertiesMenu
   -- FUNCTION
   -- Show information about selected archive
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure PropertiesMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowInfo(null);
   end PropertiesMenu;

   -- ****if* Menu/SetSorting
   -- FUNCTION
   -- Set if new archives and newly opened archives should have sorted
   -- list of entries or not
   -- PARAMETERS
   -- Self - Gtk_Check_Menu_Item which was activated. Used to set sorting.
   -- SOURCE
   procedure SetSorting(Self: access Gtk_Check_Menu_Item_Record'Class) is
      -- ****
      MessageDialog: Gtk_Message_Dialog;
   begin
      SortFiles := not Get_Active(Self);
      if SortFiles then
         MessageDialog :=
           Gtk_Message_Dialog_New
             (Window, Modal, Message_Info, Buttons_Ok,
              "The list of newly opened archive windows will be sorted until you not disable it again.");
      else
         MessageDialog :=
           Gtk_Message_Dialog_New
             (Window, Modal, Message_Info, Buttons_Ok,
              "The list of newly opened archive windows won't be sorted until one of following conditions are met (in any order):a) you enable it again, b) you restart AZip.");
      end if;
      if Run(MessageDialog) = Gtk_Response_OK then
         Destroy(MessageDialog);
      end if;
   end SetSorting;

   -- ****if* Menu/SetColumnsMenu
   -- FUNCTION
   -- Set visible columns for all opened archives
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure SetColumnsMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowColumnsDialog;
   end SetColumnsMenu;

   -- ****if* Menu/OptionsMenu
   -- FUNCTION
   -- Set the program options
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure OptionsMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowOptionsDialog;
   end OptionsMenu;

   -- ****if* Menu/HelpMenu
   -- FUNCTION
   -- Show the program help
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure HelpMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowHelpDialog;
   end HelpMenu;

   procedure ShowWebpage(URL: String) is
      Executable: access String := Locate_Exec_On_Path("xdg-open");
      MessageDialog: Gtk_Message_Dialog;
      Pid: Process_Id;
   begin
      if Executable = null then
         Executable := Locate_Exec_On_Path("xdg-open");
      end if;
      if Executable = null then
         MessageDialog :=
           Gtk_Message_Dialog_New
             (Window, Modal, Message_Error, Buttons_Close,
              "Can't find the program to open webpage.");
         if Run(MessageDialog) = Gtk_Response_Close then
            Destroy(MessageDialog);
         end if;
         return;
      end if;
      Pid :=
        Non_Blocking_Spawn(Executable.all, Argument_String_To_List(URL).all);
      if Pid = Invalid_Pid then
         MessageDialog :=
           Gtk_Message_Dialog_New
             (Window, Modal, Message_Error, Buttons_Close,
              "Can't start the program to open webpage.");
         if Run(MessageDialog) = Gtk_Response_Close then
            Destroy(MessageDialog);
         end if;
      end if;
   end ShowWebpage;

   procedure WebpageMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowWebpage("http://azip.sf.net/");
   end WebpageMenu;

   procedure NewsMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowWebpage("https://sourceforge.net/p/azip/news/");
   end NewsMenu;

   -- ****if* Menu/EmptyMenu
   -- FUNCTION
   -- Placeholder code, will be removed later
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was activated. Unused, can be null.
   -- SOURCE
   procedure EmptyMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      null;
   end EmptyMenu;

   procedure CreateMenu(WindowBox: Gtk_Box) is
      Menubar: constant Gtk_Menu_Bar := Gtk_Menu_Bar_New;
      Menu: Gtk_Menu;
      RadioGroup: Widget_SList.GSlist;
      procedure AddSubmenu(Label: String) is
         Item: constant Gtk_Menu_Item :=
           Gtk_Menu_Item_New_With_Mnemonic(Label);
      begin
         Set_Submenu(Item, Menu);
         Append(Menubar, Item);
      end AddSubmenu;
      procedure AddMenuItem
        (Label: String; Subprogram: Cb_Gtk_Menu_Item_Void;
         Enabled: Boolean := True) is
         Item: constant Gtk_Menu_Item :=
           Gtk_Menu_Item_New_With_Mnemonic(Label);
      begin
         On_Activate(Item, Subprogram);
         Set_Sensitive(Item, Enabled);
         Append(Menu, Item);
      end AddMenuItem;
      procedure AddRadioMenuItem
        (Label: String; Subprogram: Cb_Gtk_Menu_Item_Void;
         Active: Boolean := False) is
         Item: constant Gtk_Radio_Menu_Item :=
           Gtk_Radio_Menu_Item_New_With_Mnemonic(RadioGroup, Label);
      begin
         RadioGroup := Get_Group(Item);
         Set_Active(Item, Active);
         On_Activate(Item, Subprogram);
         Append(Menu, Item);
      end AddRadioMenuItem;
      procedure AddCheckMenuItem
        (Label: String; Subprogram: Cb_Gtk_Check_Menu_Item_Void;
         Active: Boolean := False) is
         Item: constant Gtk_Check_Menu_Item :=
           Gtk_Check_Menu_Item_New_With_Mnemonic(Label);
      begin
         Set_Active(Item, Active);
         On_Toggled(Item, Subprogram);
         Append(Menu, Item);
      end AddCheckMenuItem;
   begin
      -- Add File menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_File");
      AddMenuItem("_New", NewArchiveMenu'Access);
      AddMenuItem("_Open", OpenArchiveMenu'Access);
      AddMenuItem("Save _as", SaveArchiveMenu'Access);
      AddMenuItem("_Close", CloseArchiveMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Properties", PropertiesMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      declare
         Item: constant Gtk_Menu_Item :=
           Gtk_Menu_Item_New_With_Mnemonic("_Recent");
         RecentMenu: constant Gtk_Recent_Chooser_Menu :=
           Gtk_Recent_Chooser_Menu_New;
         Filter: constant Gtk_Recent_Filter := Gtk_Recent_Filter_New;
      begin
         Add_Pattern(Filter, "*.zip");
         Add_Pattern(Filter, "*.jar");
         Set_Filter(RecentMenu, Filter);
         Set_Limit(RecentMenu, 10);
         Set_Show_Numbers(RecentMenu, True);
         Set_Local_Only(RecentMenu, True);
         Set_Submenu(Item, RecentMenu);
         On_Item_Activated(+(RecentMenu), OpenRecent'Access);
         Append(Menu, Item);
      end;
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Quit", QuitMenu'Access);
      -- Add Edit menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_Edit");
      AddMenuItem("Select _all", SelectAll'Access);
      AddMenuItem("_Unselect all", UnselectAll'Access);
      AddMenuItem("_Extract", ExtractArchiveMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("Delete entries", DeleteFiles'Access);
      AddMenuItem("A_dd files...", AddFileMenu'Access);
      AddMenuItem
        ("Add files with encr_yption...", AddFileEncryptionMenu'Access);
      AddMenuItem("Add folder...", AddFolderMenu'Access);
      AddMenuItem
        ("Add folder with encryption...", AddFolderEncryptionMenu'Access);
      -- Add Tools menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_Tools");
      AddMenuItem("_Test archive", TestArchiveMenu'Access);
      AddMenuItem("_Find in archive...", FindMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Update archive", UpdateArchiveMenu'Access);
      AddMenuItem("_Recompress archive", RecompressArchiveMenu'Access);
      AddMenuItem("_Touch time stamps", EmptyMenu'Access, False);
      AddMenuItem("Encr_ypt archive", EmptyMenu'Access, False);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Compare archives", EmptyMenu'Access, False);
      AddMenuItem("_Merge archives", EmptyMenu'Access, False);
      -- Add View menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_View");
      AddRadioMenuItem("Tree view", ChangeViewMenu'Access, True);
      AddRadioMenuItem("Flat view", ChangeViewMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddCheckMenuItem("_No sorting", SetSorting'Access);
      AddMenuItem("_Select columns", SetColumnsMenu'Access);
      -- Add Options menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_Options");
      AddMenuItem("_General options", OptionsMenu'Access);
      -- Add Window menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_Window");
      RadioGroup := Widget_SList.Null_List;
      AddRadioMenuItem("Tile _Vertical", SplitWindow'Access, True);
      AddRadioMenuItem("Tile _Horizontal", SplitWindow'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Close all", CloseAll'Access);
      -- Add Help menu
      Menu := Gtk_Menu_New;
      AddSubmenu("_Help");
      AddMenuItem("_Quick help", HelpMenu'Access);
      AddMenuItem("AZip _Webpage (contact, support)", WebpageMenu'Access);
      AddMenuItem("Azip _news", NewsMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_About AZip", ShowAbout'Access);
      Pack_Start(WindowBox, Menubar, False);
   end CreateMenu;

end Menu;
