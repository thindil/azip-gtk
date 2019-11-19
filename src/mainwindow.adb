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
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Image; use Gtk.Image;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtkada.MDI; use Gtkada.MDI;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with AboutDialog; use AboutDialog;
with FileDialogs; use FileDialogs;
with FindDialog; use FindDialog;
with InfoDialog; use InfoDialog;

package body MainWindow is

   MWindow: MDI_Window;
   Orientation: Gtk_Orientation := Orientation_Vertical;
   Window: Gtk_Window;

   procedure NewArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      ArchivePaned: constant Gtk_Paned :=
        Gtk_Paned_New(Orientation_Horizontal);
      Scroll: Gtk_Scrolled_Window;
      Tree: constant Gtk_Tree_Store :=
        Gtk_Tree_Store_Newv((0 => GType_String));
      Column: Gtk_Tree_View_Column;
      Iter: Gtk_Tree_Iter;
      Area: Gtk_Cell_Area_Box;
      Renderer: Gtk_Cell_Renderer_Text;
      MChild: MDI_Child;
   begin
      Set_Position
        (ArchivePaned,
         Gint
           (Float
              (Get_Allocated_Width
                 (Gtk_Widget(Window))) *
            0.3));
      -- Tree view
      Append(Tree, Iter, Null_Iter);
      Set(Tree, Iter, 0, "New archive");
      declare
         View: constant Gtk_Tree_View := Gtk_Tree_View_New_With_Model(+(Tree));
      begin
         Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         if Append_Column(View, Column) /= 1 then
            Ada.Text_IO.Put_Line("Error in adding columns.");
         end if;
         Expand_All(View);
         Scroll := Gtk_Scrolled_Window_New;
         Set_Policy(Scroll, Policy_Never, Policy_Never);
         Add(Gtk_Container(Scroll), Gtk_Widget(View));
         Pack1(ArchivePaned, Gtk_Widget(Scroll));
         Set_Cursor(View, Gtk_Tree_Path_New_From_String("0"), null, False);
      end;
      -- Flat view
      declare
         List: constant Gtk_List_Store :=
           Gtk_List_Store_Newv
             ((GType_String, GType_String, GType_String, GType_String,
               GType_String, GType_String, GType_String, GType_String,
               GType_String, GType_String, GType_String, GType_Int));
         View: constant Gtk_Tree_View := Gtk_Tree_View_New_With_Model(+(List));
         CellNames: constant array(Positive range <>) of Unbounded_String :=
           (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
            To_Unbounded_String("Modified"), To_Unbounded_String("Attributes"),
            To_Unbounded_String("Size"), To_Unbounded_String("Packed"),
            To_Unbounded_String("Ratio"), To_Unbounded_String("Format"),
            To_Unbounded_String("CRC 32"), To_Unbounded_String("Path"),
            To_Unbounded_String("Name encoding"),
            To_Unbounded_String("Result"));
      begin
         Set_Headers_Clickable(View, True);
         for I in 0 .. 11 loop
            Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
            Area := Gtk_Cell_Area_Box_New;
            Pack_Start(Area, Renderer, True);
            Add_Attribute(Area, Renderer, "text", Gint(I));
            Column := Gtk_Tree_View_Column_New_With_Area(Area);
            Set_Sort_Column_Id(Column, Gint(I));
            Set_Title(Column, To_String(CellNames(I + 1)));
            if Append_Column(View, Column) < Gint(I) then
               Ada.Text_IO.Put_Line("Error in adding columns.");
            end if;
         end loop;
         Scroll := Gtk_Scrolled_Window_New;
         Add(Gtk_Container(Scroll), Gtk_Widget(View));
         Pack2(ArchivePaned, Gtk_Widget(Scroll));
      end;
      Gtk_New(MChild, Gtk_Widget(ArchivePaned));
      Set_Title(MChild, "New Archive");
      Put(MWindow, MChild);
      Split(MWindow, Orientation, MChild);
      Set_Focus_Child(MChild);
   end NewArchive;

   procedure OpenFile(FileName: String) is
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
      Tree: Gtk_Tree_Store;
      MChild: MDI_Child;
   begin
      if FileName = "" then
         return;
      end if;
      MChild := Get_Focus_Child(MWindow);
      if MChild /= null and then Get_Title(MChild) = "New Archive" then
         Close_Child(MChild, True);
      end if;
      NewArchive(null);
      MChild := Get_Focus_Child(MWindow);
      Set_Title(MChild, FileName);
      Tree :=
        -(Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child1(Gtk_Paned(Get_Widget(MChild))))))));
      Iter := Get_Iter_From_String(Tree, "0");
      Set(Tree, Iter, 0, Simple_Name(FileName));
      List :=
        -(Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
      -- Some test data
      for I in 0 .. 11 loop
         Append(List, Iter);
         for J in 0 .. 11 loop
            if J < 11 then
               Set(List, Iter, Gint(J), Integer'Image(I));
            else
               Set(List, Iter, Gint(J), Gint(I));
            end if;
         end loop;
      end loop;
   end OpenFile;

   procedure ExtractArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowDirectoryDialog
        (Window, Get_Title(MChild));
   end ExtractArchive;

   procedure AddFile(Self: access Gtk_Tool_Button_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Get_Label(Self) = "Add files..." then
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))));
      elsif Get_Label(Self) = "Add files with encryption..." then
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            True);
      end if;
   end AddFile;

   procedure DeleteItems
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter) is
      pragma Unreferenced(Path);
      NewIter: Gtk_Tree_Iter := Iter;
   begin
      Ada.Text_IO.Put_Line("Deleting: " & Get_String(Model, Iter, 0));
      Gtk.List_Store.Remove(-(Model), NewIter);
   end DeleteItems;

   procedure DeleteFiles(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "Do you want to delete selected item(s)?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Selected_Foreach
           (Get_Selection
              (Gtk_Tree_View
                 (Get_Child
                    (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))),
            DeleteItems'Access);
      end if;
      Destroy(MessageDialog);
   end DeleteFiles;

   procedure TestArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal, Message_Info,
           Buttons_Close, "");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Set_Markup
        (MessageDialog, "Here is result of test of " & Get_Title(MChild));
      if Run(MessageDialog) = Gtk_Response_Close then
         Destroy(MessageDialog);
      end if;
   end TestArchive;

   procedure Find(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowFindDialog
        (Window,
         Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end Find;

   procedure UpdateArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to start an archive update. Files that are newer and different (according to their CRC32 code) will replace those in the archive. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Updating: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end UpdateArchive;

   procedure RecompressArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to recompress this archive. Contents will remain identical, but data compression may be better. This operation can take a long time depending on data size and content. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Recompressing: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end RecompressArchive;

   procedure ChangeView(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Set_Visible
        (Get_Child1(Gtk_Paned(Get_Widget(MChild))),
         not Get_Visible(Get_Child1(Gtk_Paned(Get_Widget(MChild)))));
   end ChangeView;

   procedure ShowInfo(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowInfoDialog
        (Window, Get_Title(MChild));
   end ShowInfo;

   procedure OpenArchive(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
   begin
      OpenFile(ShowFileDialog(Window));
   end OpenArchive;

   procedure NewArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      NewArchive(null);
   end NewArchiveMenu;

   procedure OpenArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      OpenFile(ShowFileDialog(Window));
   end OpenArchiveMenu;

   procedure SaveArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowSaveDialog
        (Window, Get_Title(MChild));
   end SaveArchiveMenu;

   procedure CloseArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if MChild = null then
         return;
      end if;
      Close_Child(MChild);
   end CloseArchiveMenu;

   procedure QuitMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      Main_Quit;
   end QuitMenu;

   procedure ExtractArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowDirectoryDialog
        (Window, Get_Title(MChild));
   end ExtractArchiveMenu;

   procedure DeleteFiles(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "Do you want to delete selected item(s)?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Selected_Foreach
           (Get_Selection
              (Gtk_Tree_View
                 (Get_Child
                    (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))),
            DeleteItems'Access);
      end if;
      Destroy(MessageDialog);
   end DeleteFiles;

   procedure AddFileMenu(Self: access Gtk_Menu_Item_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Get_Label(Self) = "A_dd files..." then
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))));
      elsif Get_Label(Self) = "Add files with encr_yption..." then
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            True);
      elsif Get_Label(Self) = "Add folder..." then
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            False, True);
      else
         ShowAddFileDialog
           (Window,
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            True, True);
      end if;
   end AddFileMenu;

   procedure TestArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal, Message_Info,
           Buttons_Close, "");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Set_Markup
        (MessageDialog, "Here is result of test of " & Get_Title(MChild));
      if Run(MessageDialog) = Gtk_Response_Close then
         Destroy(MessageDialog);
      end if;
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
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to start an archive update. Files that are newer and different (according to their CRC32 code) will replace those in the archive. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Updating: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end UpdateArchiveMenu;

   procedure RecompressArchiveMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Window, Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to recompress this archive. Contents will remain identical, but data compression may be better. This operation can take a long time depending on data size and content. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Recompressing: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end RecompressArchiveMenu;

   procedure ChangeViewMenu(Self: access Gtk_Menu_Item_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Get_Label(Self) = "Tree view" then
         Show_All(Get_Child1(Gtk_Paned(Get_Widget(MChild))));
      else
         Hide(Get_Child1(Gtk_Paned(Get_Widget(MChild))));
      end if;
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

   procedure ProgramQuit(Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced(Self);
   begin
      Main_Quit;
   end ProgramQuit;

   procedure EmptyMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      null;
   end EmptyMenu;

   procedure CreateMainWindow is
      Error: GError;
      ToolsIcons: Gdk_Pixbuf;
      Toolbar: constant Gtk_Toolbar := Gtk_Toolbar_New;
      Menubar: constant Gtk_Menu_Bar := Gtk_Menu_Bar_New;
      Menu: Gtk_Menu;
      RadioGroup: Widget_SList.GSlist;
      WindowBox: constant Gtk_VBox := Gtk_Vbox_New;
      procedure AddButton
        (IconStarts: Gint; Label: String;
         Subprogram: Cb_Gtk_Tool_Button_Void) is
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
         Add(Toolbar, Button);
      end AddButton;
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
         AddButton(320, "New archive", NewArchive'Access);
         AddButton(352, "Open archive", OpenArchive'Access);
         AddButton(64, "Extract archive", ExtractArchive'Access);
         Add(Toolbar, Gtk_Separator_Tool_Item_New);
         AddButton(0, "Add files...", AddFile'Access);
         AddButton(192, "Add files with encryption...", AddFile'Access);
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
         AddMenuItem("Add files with encr_yption...", AddFileMenu'Access);
         AddMenuItem("Add folder...", AddFileMenu'Access);
         AddMenuItem("Add folder with encryption...", AddFileMenu'Access);
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
         Pack_Start(WindowBox, Toolbar, False);
         Pack_Start(WindowBox, Gtk_Widget(MWindow));
         Pack_Start(WindowBox, Gtk_Widget(Gtk_Status_Bar_New), False);
         Add(Gtk_Container(Window), Gtk_Widget(WindowBox));
      Show_All(Gtk_Widget(Window));
      NewArchive(null);
   end CreateMainWindow;

end MainWindow;
