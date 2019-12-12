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
with Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with MainWindow; use MainWindow;
with Menu; use Menu;

package body ArchivesView is

   -- ****if* ArchivesView/VisibleFiles
   -- FUNCTION
   -- Check if selected file should be visible on the files list in selected
   -- archive.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model containing all files in selected archive
   -- Iter  - Gtk_Tree_Iter to selected file
   -- RESULT
   -- Return True if file should be visible, otherwise False.
   -- SOURCE
   function VisibleFiles
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
      -- ****
      SelectedModel: Gtk_Tree_Model;
      SelectedIter: Gtk_Tree_Iter;
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      Path: Unbounded_String;
   begin
      if not Get_Visible(Get_Child1(Gtk_Paned(Get_Widget(MChild)))) then
         return True;
      end if;
      Get_Selected
        (Get_Selection
           (Gtk_Tree_View
              (Get_Child(Gtk_Bin(Get_Child1(Gtk_Paned(Get_Widget(MChild))))))),
         SelectedModel, SelectedIter);
      Path := To_Unbounded_String(TreePathToPath(SelectedModel, SelectedIter));
      if Get_String(Model, Iter, 9) = To_String(Path) then
         return True;
      end if;
      return False;
   end VisibleFiles;

   -- ****if* ArchivesView/RefreshFilesList
   -- FUNCTION
   -- Refresh list of files in selected archive when the user select new
   -- directory from the tree in that archive.
   -- PARAMETERS
   -- Self   - Gtk_Tree_View with tree of directories which was clicked.
   --          Unused.
   -- Path   - Gtk_Tree_Path to directory which was clicked. Unused.
   -- Column - Gtk_Tree_View_Column which was clicked. Unused.
   -- SOURCE
   procedure RefreshFilesList
     (Self: access Gtk_Tree_View_Record'Class; Path: Gtk_Tree_Path;
      Column: not null access Gtk_Tree_View_Column_Record'Class) is
      pragma Unreferenced(Self, Path, Column);
      -- ****
      MChild: constant MDI_Child := Get_Focus_Child(MWindow);
      View: constant Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child(Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))));
   begin
      Refilter(-(Gtk.Tree_Model_Sort.Get_Model(-(Get_Model(View)))));
   end RefreshFilesList;

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
      ToolBar: constant Gtk_Toolbar :=
        Gtk_Toolbar(Get_Child(Gtk_Box(Get_Child(Window)), 1));
      Buttons: constant array(1 .. 4) of Gint := (4, 5, 14, 16);
   begin
      -- Split archive window on 1/3 for tree and rest for list.
      Set_Position
        (ArchivePaned,
         Gint(Float(Get_Allocated_Width(Gtk_Widget(Window))) * 0.3));
      -- Add tree view with directory tree for the archive
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
         Set_Activate_On_Single_Click(View, True);
         On_Row_Activated(View, RefreshFilesList'Access);
         Scroll := Gtk_Scrolled_Window_New;
         Set_Policy(Scroll, Policy_Never, Policy_Never);
         Add(Gtk_Container(Scroll), Gtk_Widget(View));
         Pack1(ArchivePaned, Gtk_Widget(Scroll));
         Set_Cursor(View, Gtk_Tree_Path_New_From_String("0"), null, False);
      end;
      -- Add flat list for content of the archive
      declare
         List: constant Gtk_List_Store :=
           Gtk_List_Store_Newv
             ((GType_String, GType_String, GType_String, GType_String,
               GType_String, GType_String, GType_String, GType_String,
               GType_String, GType_String, GType_String, GType_String,
               GType_String));
         Filter: constant Gtk_Tree_Model_Filter :=
           Gtk_Tree_Model_Filter_Filter_New(+(List));
         Sort: constant Gtk_Tree_Model_Sort :=
           Gtk_Tree_Model_Sort_Sort_New_With_Model(+(Filter));
         View: constant Gtk_Tree_View := Gtk_Tree_View_New_With_Model(+(Sort));
      begin
         Set_Headers_Clickable(View, True);
         Set_Mode(Get_Selection(View), Selection_Multiple);
         for I in 0 .. 11 loop
            Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
            Area := Gtk_Cell_Area_Box_New;
            Pack_Start(Area, Renderer, True);
            Add_Attribute(Area, Renderer, "text", Gint(I));
            if I = 11 then
               Add_Attribute(Area, Renderer, "background", 12);
            end if;
            Column := Gtk_Tree_View_Column_New_With_Area(Area);
            Set_Sort_Column_Id(Column, Gint(I));
            Set_Title(Column, To_String(Columns(I).Name));
            Set_Visible(Column, Columns(I).Visible);
            if Append_Column(View, Column) < Gint(I) then
               Ada.Text_IO.Put_Line("Error in adding columns.");
            end if;
         end loop;
         if SortFiles then
            Set_Sort_Column_Id(Sort, 0, Sort_Ascending);
         end if;
         Set_Visible_Func(Filter, VisibleFiles'Access);
         Scroll := Gtk_Scrolled_Window_New;
         Add(Gtk_Container(Scroll), Gtk_Widget(View));
         Pack2(ArchivePaned, Gtk_Widget(Scroll));
      end;
      -- Add archive window to the main window
      Gtk_New(MChild, Gtk_Widget(ArchivePaned));
      Set_Title(MChild, "New Archive");
      Put(MWindow, MChild);
      Split(MWindow, Orientation, MChild);
      Set_Focus_Child(MChild);
      -- Enable some buttons in toolbar
      for Button of Buttons loop
         Set_Sensitive(Gtk_Widget(Get_Nth_Item(ToolBar, Button)), True);
      end loop;
   end NewArchive;

   procedure OpenFile(FileName: String) is
      Iter: Gtk_Tree_Iter;
      Sort: Gtk_Tree_Model_Sort;
      List: Gtk_List_Store;
      Tree: Gtk_Tree_Store;
      MChild: MDI_Child;
   begin
      if FileName = "" then
         return;
      end if;
      MChild := Get_Focus_Child(MWindow);
      if MChild = null or else Get_Title(MChild) /= "New Archive" then
         NewArchive(null);
      end if;
      MChild := Get_Focus_Child(MWindow);
      Set_Title(MChild, FileName);
      Tree :=
        -(Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child1(Gtk_Paned(Get_Widget(MChild))))))));
      Iter := Get_Iter_From_String(Tree, "0");
      Set(Tree, Iter, 0, Simple_Name(FileName));
      Sort :=
        -(Get_Model
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Bin(Get_Child2(Gtk_Paned(Get_Widget(MChild))))))));
      List :=
        -(Gtk.Tree_Model_Filter.Get_Model
           (-(Gtk.Tree_Model_Sort.Get_Model(Sort))));
      -- Placeholder code. Here should go all data read from the selected
      -- archive. Columns from 0 to 11: Name, Type, Modified, Attributes,
      -- Size, Packed, Ratio, Format, CRC 32, Path, Name encoding, Result.
      -- Last value is value of color (name, rgb, rgba) which will be used as
      -- background for Result cell. All values are Strings.
      for I in 0 .. 11 loop
         Append(List, Iter);
         for J in 0 .. 11 loop
            Set(List, Iter, Gint(J), Integer'Image(I));
         end loop;
         Set(List, Iter, 12, "rgba(0.0, 0.0, 0.0, 0.0)");
      end loop;
      Child_Selected(MWindow, MChild);
   end OpenFile;

   function TreePathToPath
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return String is
      Path: Unbounded_String :=
        To_Unbounded_String(Get_String(Model, Iter, 0));
      NewIter: Gtk_Tree_Iter := Iter;
   begin
      loop
         NewIter := Gtk.Tree_Store.Parent(-(Model), NewIter);
         if NewIter = Null_Iter then
            Path := Null_Unbounded_String;
         end if;
         exit when NewIter = Null_Iter
           or else Iter_Depth(-(Model), NewIter) = 0;
         Path :=
           To_Unbounded_String
             (Get_String(Model, NewIter, 0) & Directory_Separator) &
           Path;
      end loop;
      Path := To_Unbounded_String("" & Directory_Separator) & Path;
      return To_String(Path);
   end TreePathToPath;

end ArchivesView;
