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
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Container; use Gtk.Container;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;
with Gtk.Image; use Gtk.Image;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
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
with Glib.Object; use Glib.Object;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with AboutDialog; use AboutDialog;
with FileDialogs; use FileDialogs;
with FindDialog; use FindDialog;
with InfoDialog; use InfoDialog;

package body MainWindow is

   Builder: Gtkada_Builder;
   MWindow: MDI_Window;
   Orientation: Gtk_Orientation := Orientation_Vertical;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
   end Quit;

   procedure ShowAbout(Object: access Gtkada_Builder_Record'Class) is
   begin
      ShowAboutDialog(Gtk_Window(Get_Object(Object, "mainwindow")));
   end ShowAbout;

   procedure NewArchive(Object: access Gtkada_Builder_Record'Class) is
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
                 (Gtk_Widget(Get_Object(Object, "mainwindow")))) *
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
      NewArchive(Builder);
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

   procedure OpenDialog(User_Data: access GObject_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if User_Data = Get_Object(Builder, "btnopen") then
         OpenFile
           (ShowFileDialog(Gtk_Window(Get_Object(Builder, "mainwindow"))));
      elsif User_Data = Get_Object(Builder, "menusaveas") then
         ShowSaveDialog
           (Gtk_Window(Get_Object(Builder, "mainwindow")), Get_Title(MChild));
      else
         if Run(Gtk_Dialog(User_Data)) = Gtk_Response_Delete_Event then
            Hide(Gtk_Widget(User_Data));
         end if;
      end if;
   end OpenDialog;

   procedure ExtractArchive(Object: access Gtkada_Builder_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowDirectoryDialog
        (Gtk_Window(Get_Object(Object, "mainwindow")), Get_Title(MChild));
   end ExtractArchive;

   procedure ToggleView(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Get_Active
          (Gtk_Check_Menu_Item(Get_Object(Object, "treeviewitem"))) then
         Set_Active
           (Gtk_Check_Menu_Item(Get_Object(Object, "flatviewitem")), True);
      else
         Set_Active
           (Gtk_Check_Menu_Item(Get_Object(Object, "treeviewitem")), True);
      end if;
   end ToggleView;

   procedure AddFile(User_Data: access GObject_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if User_Data = Get_Object(Builder, "btnadd") then
         ShowAddFileDialog
           (Gtk_Window(Get_Object(Builder, "mainwindow")),
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))));
      elsif User_Data = Get_Object(Builder, "btnadd2") then
         ShowAddFileDialog
           (Gtk_Window(Get_Object(Builder, "mainwindow")),
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            True);
      elsif User_Data = Get_Object(Builder, "menuaddfolder") then
         ShowAddFileDialog
           (Gtk_Window(Get_Object(Builder, "mainwindow")),
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            False, True);
      else
         ShowAddFileDialog
           (Gtk_Window(Get_Object(Builder, "mainwindow")),
            -(Get_Model
               (Gtk_Tree_View
                  (Get_Child
                     (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild)))))))),
            True, True);
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

   procedure DeleteFiles(Object: access Gtkada_Builder_Record'Class) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal,
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

   procedure TestArchive(Object: access Gtkada_Builder_Record'Class) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal, Message_Info,
           Buttons_Close, "");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Set_Markup
        (MessageDialog, "Here is result of test of " & Get_Title(MChild));
      if Run(MessageDialog) = Gtk_Response_Close then
         Destroy(MessageDialog);
      end if;
   end TestArchive;

   procedure Find(Object: access Gtkada_Builder_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowFindDialog
        (Gtk_Window(Get_Object(Object, "mainwindow")),
         Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
   end Find;

   procedure UpdateArchive(Object: access Gtkada_Builder_Record'Class) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to start an archive update. Files that are newer and different (according to their CRC32 code) will replace those in the archive. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Updating: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end UpdateArchive;

   procedure RecompressArchive(Object: access Gtkada_Builder_Record'Class) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal,
           Message_Question, Buttons_Yes_No,
           "You are about to recompress this archive. Contents will remain identical, but data compression may be better. This operation can take a long time depending on data size and content. Proceed?");
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Ada.Text_IO.Put_Line("Recompressing: " & Get_Title(MChild));
      end if;
      Destroy(MessageDialog);
   end RecompressArchive;

   procedure SaveFile(Object: access Gtkada_Builder_Record'Class) is
      SaveDialog: constant GObject := Get_Object(Object, "savedialog");
      FileName: constant String :=
        Get_Current_Name(Gtk_File_Chooser_Dialog(SaveDialog));
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Hide(Gtk_Widget(SaveDialog));
      Put_Line("Saving " & Get_Title(MChild) & " as " & FileName);
   end SaveFile;

   procedure CloseArchive(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      if MChild = null then
         return;
      end if;
      Close_Child(MChild);
   end CloseArchive;

   procedure ChangeView(Object: access Gtkada_Builder_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      Set_Visible
        (Get_Child1(Gtk_Paned(Get_Widget(MChild))),
         Get_Active(Gtk_Check_Menu_Item(Get_Object(Object, "treeviewitem"))));
   end ChangeView;

   procedure CloseAll(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      MChild: MDI_Child;
   begin
      loop
         MChild := Get_Focus_Child(MWindow);
         exit when MChild = null;
         Close_Child(MChild);
      end loop;
   end CloseAll;

   procedure SplitWindow(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Get_Active(Gtk_Check_Menu_Item(Get_Object(Object, "splithorizontalitem"))) then
         Orientation := Orientation_Horizontal;
      else
         Orientation := Orientation_Vertical;
      end if;
      Split(MWindow, Orientation);
   end SplitWindow;

   procedure ShowInfo(Object: access Gtkada_Builder_Record'Class) is
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
   begin
      ShowInfoDialog
        (Gtk_Window(Get_Object(Object, "mainwindow")), Get_Title(MChild));
   end ShowInfo;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder) is
      Error: GError;
      ToolsIcons, Icon: Gdk_Pixbuf;
      StartX: Gint := 0;
      ImagesNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("imgadd"), To_Unbounded_String("imgdelete"),
         To_Unbounded_String("imgextract"), To_Unbounded_String("imgfind"),
         To_Unbounded_String("imgtest"), To_Unbounded_String("imgupdate"),
         To_Unbounded_String("imgadd2"), To_Unbounded_String("imgproperties"),
         Null_Unbounded_String, To_Unbounded_String("imgrecompress"),
         To_Unbounded_String("imgnew"), To_Unbounded_String("imgopen"),
         To_Unbounded_String("imgview"));
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_About", ShowAbout'Access);
      Register_Handler(Builder, "New_Archive", NewArchive'Access);
      Register_Handler(Builder, "Extract_Archive", ExtractArchive'Access);
      Register_Handler(Builder, "Toggle_View", ToggleView'Access);
      Register_Handler(Builder, "Open_Dialog", OpenDialog'Access);
      Register_Handler(Builder, "Add_File", AddFile'Access);
      Register_Handler(Builder, "Delete_Files", DeleteFiles'Access);
      Register_Handler(Builder, "Test_Archive", TestArchive'Access);
      Register_Handler(Builder, "Find", Find'Access);
      Register_Handler(Builder, "Update_Archive", UpdateArchive'Access);
      Register_Handler
        (Builder, "Recompress_Archive", RecompressArchive'Access);
      Register_Handler(Builder, "Save_File", SaveFile'Access);
      Register_Handler(Builder, "Close_Archive", CloseArchive'Access);
      Register_Handler(Builder, "Change_View", ChangeView'Access);
      Register_Handler(Builder, "Close_All", CloseAll'Access);
      Register_Handler(Builder, "Split_Window", SplitWindow'Access);
      Register_Handler(Builder, "Show_Info", ShowInfo'Access);
      Do_Connect(Builder);
      Gdk_New_From_File(ToolsIcons, "az_tools.bmp", Error);
      if Error /= null then
         Quit(Builder);
         Put_Line(Get_Message(Error));
         return;
      end if;
      ToolsIcons := Add_Alpha(ToolsIcons, True, 163, 73, 164);
      for ImageName of ImagesNames loop
         if ImageName /= Null_Unbounded_String then
            Icon := Gdk_New_Subpixbuf(ToolsIcons, StartX, 0, 32, 32);
            Set(Gtk_Image(Get_Object(Builder, To_String(ImageName))), Icon);
         end if;
         StartX := StartX + 32;
      end loop;
      Gtk_New(MWindow, null);
      Pack_End
        (Gtk_Box(Get_Object(Builder, "windowbox")), Gtk_Widget(MWindow));
      Reorder_Child(Gtk_Box(Get_Object(Builder, "windowbox")), Gtk_Widget(MWindow), 3);
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
      NewArchive(Builder);
   end CreateMainWindow;

end MainWindow;
