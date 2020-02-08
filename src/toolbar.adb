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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;

package body Toolbar is

   procedure CreateToolbar is
      Toolbar: constant Ttk_Frame := Create(".toolbar");
      CurrentDir: constant String := Current_Directory;
      Image: Tk_Photo;
      procedure AddButton
        (Name: String; StartX: Natural; ToolTip: String;
         Command: String := "") is
         Icon: constant Tk_Photo := Create(Name & "icon");
         Toolbutton: Ttk_Button;
      begin
         if Command /= "" then
            Toolbutton :=
              Create(Name, "-style Toolbutton -command " & Command);
         else
            Toolbutton := Create(Name, "-style Toolbutton");
         end if;
         Copy
           (Image, Icon,
            "-from" & Natural'Image(StartX) & " 0 " &
            Natural'Image(StartX + 32) & " 32");
         configure(Toolbutton, "-image " & Name & "icon");
         Tcl.Tk.Ada.Pack.Pack(Toolbutton, "-side left");
         Add(Toolbutton, ToolTip);
      end AddButton;
      procedure AddSeparator(Number: String) is
         Separator: constant Ttk_Separator :=
           Create(".toolbar.separator" & Number, "-orient vertical");
      begin
         Tcl.Tk.Ada.Pack.Pack(Separator, "-side left -padx 5 -fill y");
      end AddSeparator;
   begin
      Set_Directory(Containing_Directory(Command_Name));
      Image := Create("toolbaricons", "-file ""az_tools.gif""");
      Set_Directory(CurrentDir);
      AddButton(".toolbar.new", 320, "New archive", "Create");
      AddButton(".toolbar.open", 352, "Open archive...", "Load");
      AddButton(".toolbar.extract", 64, "Extract...");
      AddSeparator("1");
      AddButton(".toolbar.add", 0, "Add files...");
      AddButton(".toolbar.add2", 192, "Add files with encryption...");
      AddButton(".toolbar.delete", 32, "Delete entries");
      AddSeparator("2");
      AddButton(".toolbar.test", 128, "Test archive");
      AddButton(".toolbar.find", 96, "Find in archive");
      AddSeparator("3");
      AddButton(".toolbar.update", 160, "Update archive");
      AddButton(".toolbar.recompress", 288, "Recompress archive");
      AddSeparator("4");
      AddButton(".toolbar.view", 384, "Toggle flat/tree view");
      AddSeparator("5");
      AddButton(".toolbar.properties", 224, "Properties");
      Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill x");
      Delete(Image);
   end CreateToolbar;

end Toolbar;
