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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;

package body MenuBar is

   procedure CreateMenuBar(MainWindow: Tk_Toplevel) is
      Menubar: constant Tk_Menu := Create(".menubar", "-borderwidth 0");
      type Menu_Item is record
         EntryType: Unbounded_String;
         Options: Unbounded_String;
      end record;
      type Menu_Item_Array is array(Positive range <>) of Menu_Item;
      Separator: constant Menu_Item :=
        (EntryType => To_Unbounded_String("separator"),
         Options => Null_Unbounded_String);
      procedure CreateSubMenu
        (MenuName, MenuLabel: String; MenuItems: Menu_Item_Array) is
         SubMenu: constant Tk_Menu := Create(MenuName, "-tearoff false");
      begin
         for MenuItem of MenuItems loop
            Add
              (SubMenu, To_String(MenuItem.EntryType),
               To_String(MenuItem.Options));
         end loop;
         Add
           (Menubar, "cascade",
            "-label """ & MenuLabel & """ -underline 0 -menu " & MenuName);
      end CreateSubMenu;
      function CreateRecent return Menu_Item is
         SubMenu: constant Tk_Menu := Create(".recent", "-tearoff false");
      begin
         for I in 1 .. 9 loop
            Add
              (SubMenu, "command",
               "-label " & Trim(Positive'Image(I), Both) & " -underline 0");
         end loop;
         return (To_Unbounded_String("cascade"),
            To_Unbounded_String("-label Recent -underline 0 -menu .recent"));
      end CreateRecent;
   begin
      CreateSubMenu
        (".menubar.file", "File",
         ((To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""New archive"" -underline 0 -accelerator ""Ctrl+N"" -command Create")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Open archive..."" -underline 0 -accelerator Ctrl+O -command Load")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Save archive as..."" -underline 0 -accelerator F12")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Close archive"" -underline 0 -accelerator Ctrl+W/Ctrl+F4")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Properties"" -underline 0 -accelerator Ctrl+D")),
          Separator, CreateRecent, Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Quit"" -underline 0 -accelerator Alt+F4 -command exit"))));
      CreateSubMenu
        (".menubar.edit", "Edit",
         ((To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Select all"" -underline 7 -accelerator Ctrl+A")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Unselect all"" -underline 0 -accelerator Ctrl+U")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Extract..."" -underline 0 -accelerator Ctrl+E")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Delete entries"" -accelerator Del/-")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Add files..."" -underline 1 -accelerator +")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Add files with encryption..."" -underline 19")),
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""Add folder...""")),
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""Add folder with encryption..."""))));
      CreateSubMenu
        (".menubar.tools", "Tools",
         ((To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Test archive"" -underline 0 -accelerator Ctrl+T")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Find in archive..."" -underline 0 -accelerator Ctrl+F")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Update archive"" -underline 0 -accelerator Ctrl+P")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Recompress archive"" -underline 0 -accelerator Ctrl+R")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Touch time stamps"" -underline 1 -state disabled")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Encrypt archive"" -underline 4 -state disabled")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Compare archives"" -underline 0 -state disabled")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Merge archives"" -underline 0 -state disabled"))));
      CreateSubMenu
        (".menubar.view", "View",
         ((To_Unbounded_String("radiobutton"),
           To_Unbounded_String
             ("-label ""Flat view"" -underline 0 -variable viewtype -value flat")),
          (To_Unbounded_String("radiobutton"),
           To_Unbounded_String
             ("-label ""Tree view"" -underline 0 -variable viewtype -value tree")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""No sorting"" -underline 0")),
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""Select columns"" -underline 0"))));
      CreateSubMenu
        (".menubar.options", "Options",
         (1 =>
            (To_Unbounded_String("command"),
             To_Unbounded_String("-label ""General options"" -underline 0"))));
      CreateSubMenu
        (".menubar.window", "Window",
         ((To_Unbounded_String("radiobutton"),
           To_Unbounded_String
             ("-label ""Tile horizontal"" -underline 5 -variable tiletype -value horizontal")),
          (To_Unbounded_String("radiobutton"),
           To_Unbounded_String
             ("-label ""Tile vertical"" -underline 5 -variable tiletype -value vertical")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Close all"" -underline 0 -command CloseAll"))));
      CreateSubMenu
        (".menubar.help", "Help",
         ((To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""Quick help"" -underline 0 -accelerator F1")),
          (To_Unbounded_String("command"),
           To_Unbounded_String
             ("-label ""AZip Web page(contact, support)"" -underline 5")),
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""AZip news"" -underline 5")),
          Separator,
          (To_Unbounded_String("command"),
           To_Unbounded_String("-label ""About AZip"" -underline 0"))));
      configure(MainWindow, "-menu .menubar");
   end CreateMenuBar;

end MenuBar;
