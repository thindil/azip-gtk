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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with MenuBar; use MenuBar;

package body Toolbar is

   procedure CreateToolbar is
      Toolbar: constant Ttk_Frame := Create(".toolbar");
      Image: Tk_Photo;
      -- Add button to the toolbar
      procedure AddButton
        (Name: String; StartX: Natural; ToolTip: String; Command: String := "";
         Disabled: Boolean := False) is
         Icon: constant Tk_Photo := Create(Name & "icon");
         Toolbutton: constant Ttk_Button :=
           (if Command'Length > 0 then
              Create(Name, "-style Toolbutton -command {" & Command & "}")
            else Create(Name, "-style Toolbutton"));
      begin
         Copy
           (Image, Icon,
            "-from" & Natural'Image(StartX) & " 0 " &
            Natural'Image(StartX + 32) & " 32");
         configure(Toolbutton, "-image " & Name & "icon");
         if Disabled then
            State(Toolbutton, "disabled");
         end if;
         Tcl.Tk.Ada.Pack.Pack(Toolbutton, "-side left");
         Add(Toolbutton, ToolTip);
      end AddButton;
      -- Add separator to the toolbar
      procedure AddSeparator(Number: String) is
         Separator: constant Ttk_Separator :=
           Create(".toolbar.separator" & Number, "-orient vertical");
      begin
         Tcl.Tk.Ada.Pack.Pack(Separator, "-side left -padx 5 -fill y");
      end AddSeparator;
   begin
      Image := Create("toolbaricons", "-file ""az_tools.gif""");
      AddButton(".toolbar.new", 320, "New archive", "Create");
      AddButton(".toolbar.open", 352, "Open archive...", "Load");
      AddButton(".toolbar.extract", 64, "Extract...", "Extract", True);
      AddSeparator("1");
      AddButton(".toolbar.add", 0, "Add files...", "AddFiles false");
      AddButton
        (".toolbar.add2", 192, "Add files with encryption...",
         "AddFiles true");
      AddButton(".toolbar.delete", 32, "Delete entries", "DeleteItems", True);
      AddSeparator("2");
      AddButton(".toolbar.test", 128, "Test archive", "TestArchive", True);
      AddButton
        (".toolbar.find", 96, "Find in archive", "ShowFindDialog", True);
      AddSeparator("3");
      AddButton
        (".toolbar.update", 160, "Update archive", "UpdateArchive", True);
      AddButton
        (".toolbar.recompress", 288, "Recompress archive", "RecompressArchive",
         True);
      AddSeparator("4");
      AddButton
        (".toolbar.view", 384, "Toggle flat/tree view", "ToggleView button");
      AddSeparator("5");
      AddButton(".toolbar.properties", 224, "Properties", "ShowProperties");
      Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill x");
      Delete(Image);
   end CreateToolbar;

   procedure ToggleButtons(Enable: Boolean := True) is
      Toolbutton: Ttk_Button;
      ButtonsNames: constant array(1 .. 6) of Unbounded_String :=
        (To_Unbounded_String("extract"), To_Unbounded_String("delete"),
         To_Unbounded_String("test"), To_Unbounded_String("find"),
         To_Unbounded_String("update"), To_Unbounded_String("recompress"));
   begin
      Toolbutton.Interp := Get_Context;
      for ButtonName of ButtonsNames loop
         Toolbutton.Name := New_String(".toolbar." & To_String(ButtonName));
         if Enable then
            State(Toolbutton, "!disabled");
         else
            State(Toolbutton, "disabled");
         end if;
      end loop;
      ToggleEntries(Enable);
   end ToggleButtons;

end Toolbar;
