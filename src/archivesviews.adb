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
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;

package body ArchivesViews is

   ArchiveNumber: Positive;

   procedure CreateView is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ArchiveNumber), Both);
      ArchiveView: constant Ttk_Frame :=
        Create(ViewName);
      Header: constant Ttk_Frame := Create(ViewName & ".header");
      CloseButton: constant Ttk_Button :=
        Create(ViewName & ".header.close", "-text X -style Toolbutton");
      NameLabel: constant Ttk_Label := Create(ViewName & ".header.label", "-text ""New Archive""");
   begin
      Pack(NameLabel, "-side left");
      Pack(CloseButton, "-side right");
      Pack(Header, "-fill x");
      Pack(ArchiveView, "-fill both -expand true");
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
      MDI: constant Ttk_Frame := Create(".mdi");
   begin
      ArchiveNumber := 1;
      Pack(MDI, "-fill both -expand true");
      CreateView;
   end CreateMDI;

end ArchivesViews;
