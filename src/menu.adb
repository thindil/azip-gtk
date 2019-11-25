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
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.MDI; use Gtkada.MDI;
with AboutDialog; use AboutDialog;
with FileDialogs; use FileDialogs;
with FindDialog; use FindDialog;
with MainWindow; use MainWindow;

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
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowAddFileDialog
        (Window,
         -(Get_Model
            (Gtk_Tree_View
               (Get_Child
                  (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))));
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
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowAddFileDialog
        (Window,
         -(Get_Model
            (Gtk_Tree_View
               (Get_Child
                  (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
         True);
   end AddFileEncryptionMenu;

   procedure AddFolderMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
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

   procedure AddFolderEncryptionMenu
     (Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
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

   procedure TestArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      TestArchive(null);
   end TestArchiveMenu;

   procedure FindMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowFindDialog
        (Window,
         Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end FindMenu;

   procedure UpdateArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      UpdateArchive(null);
   end UpdateArchiveMenu;

   procedure RecompressArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      RecompressArchive(null);
   end RecompressArchiveMenu;

   procedure ChangeViewMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      ChangeView(null);
   end ChangeViewMenu;

   procedure SplitWindow(Self: access Gtk_Menu_Item_Record'Class) is
   begin
      if Get_Label(Self) = "Tile _Horizontal" then
         Orientation := Orientation_Horizontal;
      else
         Orientation := Orientation_Vertical;
      end if;
      Split(MWindow, Orientation);
   end SplitWindow;

   procedure CloseAll(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MChild: MDI_Child;
   begin
      loop
         MChild := Get_Focus_Child(MWindow);
         exit when MChild = null;
         Close_Child(MChild);
      end loop;
   end CloseAll;

   procedure ShowAbout(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      ShowAboutDialog(Window);
   end ShowAbout;

   procedure EmptyMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
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
        (Label: String; Subprogram: Cb_Gtk_Menu_Item_Void) is
         Item: constant Gtk_Menu_Item :=
           Gtk_Menu_Item_New_With_Mnemonic(Label);
      begin
         On_Activate(Item, Subprogram);
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
   begin
      Menu := Gtk_Menu_New;
      AddSubmenu("_File");
      AddMenuItem("_New", NewArchiveMenu'Access);
      AddMenuItem("_Open", OpenArchiveMenu'Access);
      AddMenuItem("Save _as", SaveArchiveMenu'Access);
      AddMenuItem("_Close", CloseArchiveMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Recent", EmptyMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Quit", QuitMenu'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_Edit");
      AddMenuItem("Select _all", EmptyMenu'Access);
      AddMenuItem("_Unselect all", EmptyMenu'Access);
      AddMenuItem("_Extract", ExtractArchiveMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("Delete entries", DeleteFiles'Access);
      AddMenuItem("A_dd files...", AddFileMenu'Access);
      AddMenuItem
        ("Add files with encr_yption...", AddFileEncryptionMenu'Access);
      AddMenuItem("Add folder...", AddFolderMenu'Access);
      AddMenuItem
        ("Add folder with encryption...", AddFolderEncryptionMenu'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_Tools");
      AddMenuItem("_Test archive", TestArchiveMenu'Access);
      AddMenuItem("_Find in archive...", FindMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Update archive", UpdateArchiveMenu'Access);
      AddMenuItem("_Recompress archive", RecompressArchiveMenu'Access);
      AddMenuItem("_Touch time stamps", EmptyMenu'Access);
      AddMenuItem("Encr_ypt archive", EmptyMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Compare archives", EmptyMenu'Access);
      AddMenuItem("_Merge archives", EmptyMenu'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_View");
      AddRadioMenuItem("Tree view", ChangeViewMenu'Access, True);
      AddRadioMenuItem("Flat view", ChangeViewMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_No sorting", EmptyMenu'Access);
      AddMenuItem("_Select columns", EmptyMenu'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_Options");
      AddMenuItem("_General options", EmptyMenu'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_Window");
      RadioGroup := Widget_SList.Null_List;
      AddRadioMenuItem("Tile _Vertical", SplitWindow'Access, True);
      AddRadioMenuItem("Tile _Horizontal", SplitWindow'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_Close all", CloseAll'Access);
      Menu := Gtk_Menu_New;
      AddSubmenu("_Help");
      AddMenuItem("_Quick help", EmptyMenu'Access);
      AddMenuItem("AZip _Webpage (contact, support)", EmptyMenu'Access);
      AddMenuItem("Azip _news", EmptyMenu'Access);
      Append(Menu, Gtk_Separator_Menu_Item_New);
      AddMenuItem("_About AZip", ShowAbout'Access);
      Pack_Start(WindowBox, Menubar, False);
   end CreateMenu;

end Menu;
