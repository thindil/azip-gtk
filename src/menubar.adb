-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;

package body MenuBar is

   procedure CreateMenuBar(MainWindow: Tk_Toplevel) is
      Menubar: constant Tk_Menu := Create(".menubar", "-borderwidth 0");
      type Menu_Item is record
         Label: Unbounded_String;
         Command: Unbounded_String;
         Underline: Integer;
      end record;
      type Menu_Item_Array is array(Positive range <>) of Menu_Item;
      Separator: constant Menu_Item :=
        (Null_Unbounded_String, Null_Unbounded_String, 0);
      procedure CreateSubMenu
        (MenuName, MenuLabel: String; MenuItems: Menu_Item_Array) is
         SubMenu: constant Tk_Menu := Create(MenuName, "-tearoff false");
         Options: Unbounded_String;
      begin
         for MenuItem of MenuItems loop
            if MenuItem.Label /= Null_Unbounded_String then
               Options :=
                 To_Unbounded_String("-label """) & MenuItem.Label &
                 To_Unbounded_String("""");
               if MenuItem.Command /= Null_Unbounded_String then
                  Append(Options, " -command ");
                  Append(Options, MenuItem.Command);
               end if;
               if MenuItem.Underline > -1 then
                  Append(Options, " -underline");
                  Append(Options, Integer'Image(MenuItem.Underline));
               end if;
               Add(SubMenu, "command", To_String(Options));
            else
               Add(SubMenu, "separator");
            end if;
         end loop;
         Add
           (Menubar, "cascade",
            "-menu " & MenuName & " -label " & MenuLabel & " -underline 0");
      end CreateSubMenu;
   begin
      CreateSubMenu
        (".menubar.file", "File",
         ((Label => To_Unbounded_String("New archive"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Open archive..."),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Save archive as..."),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Close archive"),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("Properties"),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("Recent"),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("Quit"),
           Command => To_Unbounded_String("exit"), Underline => 0)));
      CreateSubMenu
        (".menubar.edit", "Edit",
         ((Label => To_Unbounded_String("Select all"),
           Command => Null_Unbounded_String, Underline => 7),
          (Label => To_Unbounded_String("Unselect all"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Extract..."),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("Delete entries"),
           Command => Null_Unbounded_String, Underline => -1),
          (Label => To_Unbounded_String("Add files..."),
           Command => Null_Unbounded_String, Underline => 1),
          (Label => To_Unbounded_String("Add files with encryption..."),
           Command => Null_Unbounded_String, Underline => 19),
          (Label => To_Unbounded_String("Add folder..."),
           Command => Null_Unbounded_String, Underline => -1),
          (Label => To_Unbounded_String("Add folder with encryption..."),
           Command => Null_Unbounded_String, Underline => -1)));
      CreateSubMenu
        (".menubar.tools", "Tools",
         ((Label => To_Unbounded_String("Test archive"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Find in archive..."),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("Update archive"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Recompress archive"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Touch time stamps"),
           Command => Null_Unbounded_String, Underline => 1),
          (Label => To_Unbounded_String("Encrypt archive"),
           Command => Null_Unbounded_String, Underline => 4),
          Separator,
          (Label => To_Unbounded_String("Compare archives"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Merge archives"),
           Command => Null_Unbounded_String, Underline => 0)));
      CreateSubMenu
        (".menubar.view", "View",
         ((Label => To_Unbounded_String("Flat view"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Tree view"),
           Command => Null_Unbounded_String, Underline => 0),
          Separator,
          (Label => To_Unbounded_String("No sorting"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("Select columns"),
           Command => Null_Unbounded_String, Underline => 0)));
      CreateSubMenu
        (".menubar.options", "Options",
         (1 =>
            (Label => To_Unbounded_String("General options"),
             Command => Null_Unbounded_String, Underline => 0)));
      CreateSubMenu
        (".menubar.window", "Window",
         ((Label => To_Unbounded_String("Tile horizontal"),
           Command => Null_Unbounded_String, Underline => 5),
          (Label => To_Unbounded_String("Tile vertical"),
           Command => Null_Unbounded_String, Underline => 5),
          (Label => To_Unbounded_String("Close all"),
           Command => Null_Unbounded_String, Underline => 0)));
      CreateSubMenu
        (".menubar.help", "Help",
         ((Label => To_Unbounded_String("Quick help"),
           Command => Null_Unbounded_String, Underline => 0),
          (Label => To_Unbounded_String("AZip Web page(contact, support)"),
           Command => Null_Unbounded_String, Underline => -1),
          (Label => To_Unbounded_String("AZip news"),
           Command => Null_Unbounded_String, Underline => -1),
          Separator,
          (Label => To_Unbounded_String("About AZip"),
           Command => Null_Unbounded_String, Underline => 0)));
      configure(MainWindow, "-menu .menubar");
   end CreateMenuBar;

end MenuBar;
