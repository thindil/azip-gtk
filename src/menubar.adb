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
      type Menu_Item_Array is array(Positive range <>) of Menu_Entry_Options;
      MSeparator: constant Menu_Entry_Options :=
        (SEPARATOR, Null_Unbounded_String, Null_Unbounded_String, -1,
         Null_Unbounded_String, NORMAL, Null_Unbounded_String);
      procedure CreateSubMenu
        (MenuName, MenuLabel: String; MenuItems: Menu_Item_Array) is
         SubMenu: constant Tk_Menu := Create(MenuName, "-tearoff false");
      begin
         for MenuItem of MenuItems loop
            Add(SubMenu, MenuItem);
         end loop;
         Add
           (Menubar,
            (MType => CASCADE, Label => To_Unbounded_String(MenuLabel),
             Command => Null_Unbounded_String, Underline => 0,
             Accelerator => Null_Unbounded_String, State => NORMAL,
             Other => Null_Unbounded_String,
             SubMenu => To_Unbounded_String(MenuName)));
      end CreateSubMenu;
   begin
      CreateSubMenu
        (".menubar.file", "File",
         ((Label => To_Unbounded_String("New archive"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+N"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Open archive..."),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+O"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Save archive as..."),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("F12"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Close archive"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+W/Ctrl+F4"),
           State => NORMAL, MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Properties"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+D"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Recent"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Quit"),
           Command => To_Unbounded_String("exit"), Underline => 0,
           Accelerator => To_Unbounded_String("Alt+F4"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.edit", "Edit",
         ((Label => To_Unbounded_String("Select all"),
           Command => Null_Unbounded_String, Underline => 7,
           Accelerator => To_Unbounded_String("Ctrl+A"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Unselect all"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+U"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Extract..."),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+E"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Delete entries"),
           Command => Null_Unbounded_String, Underline => -1,
           Accelerator => To_Unbounded_String("Del/-"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add files..."),
           Command => Null_Unbounded_String, Underline => 1,
           Accelerator => To_Unbounded_String("+"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add files with encryption..."),
           Command => Null_Unbounded_String, Underline => 19,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add folder..."),
           Command => Null_Unbounded_String, Underline => -1,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Add folder with encryption..."),
           Command => Null_Unbounded_String, Underline => -1,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.tools", "Tools",
         ((Label => To_Unbounded_String("Test archive"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+T"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Find in archive..."),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+F"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Update archive"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+P"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Recompress archive"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("Ctrl+R"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Touch time stamps"),
           Command => Null_Unbounded_String, Underline => 1,
           Accelerator => Null_Unbounded_String, State => DISABLED,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Encrypt archive"),
           Command => Null_Unbounded_String, Underline => 4,
           Accelerator => Null_Unbounded_String, State => DISABLED,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("Compare archives"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => DISABLED,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Merge archives"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => DISABLED,
           MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.view", "View",
         ((Label => To_Unbounded_String("Flat view"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => RADIOBUTTON, Other => Null_Unbounded_String,
           RadioVariable => To_Unbounded_String("view"),
           Value => To_Unbounded_String("flat")),
          (Label => To_Unbounded_String("Tree view"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => RADIOBUTTON, Other => Null_Unbounded_String,
           RadioVariable => To_Unbounded_String("view"),
           Value => To_Unbounded_String("tree")),
          MSeparator,
          (Label => To_Unbounded_String("No sorting"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Select columns"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.options", "Options",
         (1 =>
            (Label => To_Unbounded_String("General options"),
             Command => Null_Unbounded_String, Underline => 0,
             Accelerator => Null_Unbounded_String, State => NORMAL,
             MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.window", "Window",
         ((Label => To_Unbounded_String("Tile horizontal"),
           Command => Null_Unbounded_String, Underline => 5,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Tile vertical"),
           Command => Null_Unbounded_String, Underline => 5,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("Close all"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String)));
      CreateSubMenu
        (".menubar.help", "Help",
         ((Label => To_Unbounded_String("Quick help"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => To_Unbounded_String("F1"), State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("AZip Web page(contact, support)"),
           Command => Null_Unbounded_String, Underline => -1,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          (Label => To_Unbounded_String("AZip news"),
           Command => Null_Unbounded_String, Underline => -1,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String),
          MSeparator,
          (Label => To_Unbounded_String("About AZip"),
           Command => Null_Unbounded_String, Underline => 0,
           Accelerator => Null_Unbounded_String, State => NORMAL,
           MType => COMMAND, Other => Null_Unbounded_String)));
      configure(MainWindow, "-menu .menubar");
   end CreateMenuBar;

end MenuBar;
