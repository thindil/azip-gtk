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

   procedure CreateMenuBar(MainWindow: Tk_Frame) is
      Menubar: constant Tk_Menu := Create(".menubar", "-borderwidth 0");
      type Menu_Item is record
         Label: Unbounded_String;
         Command: Unbounded_String;
      end record;
      type Menu_Item_Array is array(Positive range <>) of Menu_Item;
      Separator: constant Menu_Item :=
        (Null_Unbounded_String, Null_Unbounded_String);
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
               Add(SubMenu, "command", To_String(Options));
            else
               Add(SubMenu, "separator");
            end if;
         end loop;
         Add(Menubar, "cascade", "-menu " & MenuName & " -label " & MenuLabel);
      end CreateSubMenu;
   begin
      CreateSubMenu
        (".menubar.file", "File",
         ((Label => To_Unbounded_String("New archive"),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Open archive..."),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Save archive as..."),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Close archive"),
           Command => Null_Unbounded_String),
          Separator,
          (Label => To_Unbounded_String("Properties"),
           Command => Null_Unbounded_String),
          Separator,
          (Label => To_Unbounded_String("Recent"),
           Command => Null_Unbounded_String),
          Separator,
          (Label => To_Unbounded_String("Quit"),
           Command => To_Unbounded_String("exit"))));
      CreateSubMenu
        (".menubar.edit", "Edit",
         ((Label => To_Unbounded_String("Select all"),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Unselect all"),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Extract..."),
           Command => Null_Unbounded_String),
          Separator,
          (Label => To_Unbounded_String("Delete entries"),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add files..."),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add files with encryption..."),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add folder..."),
           Command => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add folder with encryption..."),
           Command => Null_Unbounded_String)));
      Add(Menubar, "command", "-label Tools");
      Add(Menubar, "command", "-label View");
      Add(Menubar, "command", "-label Options");
      Add(Menubar, "command", "-label Window");
      Add(Menubar, "command", "-label Help");
      configure(MainWindow, "-menu .menubar");
   end CreateMenuBar;

end MenuBar;
