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

with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Button; use Tcl.Tk.Ada.Widgets.Button;
with Tcl.Tk.Ada.Widgets.Frame; use Tcl.Tk.Ada.Widgets.Frame;

package body Toolbar is

   procedure CreateToolbar is
      Toolbar: constant Tk_Frame := Create(".toolbar");
      procedure AddButton(Name, Text: String) is
         Toolbutton: constant Tk_Button := Create(Name, "-text " & Text);
      begin
         Pack(Toolbutton, "-side left");
      end AddButton;
   begin
      AddButton(".toolbar.new", "New");
      AddButton(".toolbar.open", "Open");
      AddButton(".toolbar.extract", "Extract");
      AddButton(".toolbar.add", "Add");
      AddButton(".toolbar.add2", "Add2");
      AddButton(".toolbar.delete", "Delete");
      AddButton(".toolbar.test", "Test");
      AddButton(".toolbar.find", "Find");
      AddButton(".toolbar.update", "Update");
      AddButton(".toolbar.recompress", "Recompress");
      AddButton(".toolbar.view", "View");
      AddButton(".toolbar.properties", "Properties");
      Pack(Toolbar, "-fill x");
   end CreateToolbar;

end Toolbar;
