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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Button; use Tcl.Tk.Ada.Widgets.Button;
with Tcl.Tk.Ada.Widgets.Frame; use Tcl.Tk.Ada.Widgets.Frame;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;

package body Toolbar is

   procedure CreateToolbar is
      Toolbar: constant Tk_Frame := Create(".toolbar");
      Image: Tk_Photo :=
        Create
          ("toolbaricons",
           "-file """ & Containing_Directory(Command_Name) & Dir_Separator &
           "az_tools.png""");
      procedure AddButton(Name: String; StartX: Natural) is
         Icon: constant Tk_Photo := Create(Name & "icon");
         Toolbutton: constant Tk_Button := Create(Name);
      begin
         Copy
           (Image, Icon,
            "-from" & Natural'Image(StartX) & " 0 " &
            Natural'Image(StartX + 32) & " 32");
         configure(Toolbutton, "-image " & Name & "icon");
         Pack(Toolbutton, "-side left");
      end AddButton;
   begin
      AddButton(".toolbar.new", 320);
      AddButton(".toolbar.open", 352);
      AddButton(".toolbar.extract", 64);
      AddButton(".toolbar.add", 0);
      AddButton(".toolbar.add2", 192);
      AddButton(".toolbar.delete", 32);
      AddButton(".toolbar.test", 128);
      AddButton(".toolbar.find", 96);
      AddButton(".toolbar.update", 160);
      AddButton(".toolbar.recompress", 288);
      AddButton(".toolbar.view", 384);
      AddButton(".toolbar.properties", 224);
      Pack(Toolbar, "-fill x");
      Delete(Image);
   end CreateToolbar;

end Toolbar;
