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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;

package body ArchivesViews is

   use type Interfaces.C.int;

   ArchiveNumber: Positive;

   procedure CreateView is
      ViewName: constant String :=
        ".mdi.archive" & Trim(Positive'Image(ArchiveNumber), Both);
      ArchiveView: constant Ttk_Frame := Create(ViewName);
      Header: constant Ttk_Frame := Create(ViewName & ".header");
      CloseButton: constant Ttk_Button :=
        Create(ViewName & ".header.close", "-text x -style Toolbutton");
      NameLabel: constant Ttk_Label :=
        Create
          (ViewName & ".header.label",
           "-text ""New Archive" & Positive'Image(ArchiveNumber) & """");
      Paned: constant Ttk_PanedWindow :=
        Create(ViewName & ".paned", "-orient horizontal");
      DirectoryTree: constant Ttk_Tree_View :=
        Create(ViewName & ".directorytree", "-show tree");
      FilesList: constant Ttk_Tree_View := Create(ViewName & ".fileslist");
      procedure Run_Command(Tcl_Command: String) is
      begin
         if Tcl_Eval(Get_Context, New_String(Tcl_Command)) /= 0 then
            raise Program_Error with Tcl.Ada.Tcl_GetStringResult(Get_Context);
         end if;
      end Run_Command;
   begin
      Tcl.Tk.Ada.Pack.Pack(NameLabel, "-side left");
      Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
      Tcl.Tk.Ada.Pack.Pack(Header, "-fill x");
      Run_Command
        (Widget_Image(Paned) & " add " & Widget_Image(DirectoryTree) &
         " -weight 1");
      Run_Command
        (Widget_Image(Paned) & " add " & Widget_Image(FilesList) &
         " -weight 20");
      Tcl.Tk.Ada.Pack.Pack(Paned, "-fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack(ArchiveView, "-fill both -expand true");
      ArchiveNumber := ArchiveNumber + 1;
   end CreateView;

   procedure CreateMDI is
      MDI: constant Ttk_Frame := Create(".mdi");
   begin
      ArchiveNumber := 1;
      Tcl.Tk.Ada.Pack.Pack(MDI, "-fill both -expand true");
      CreateView;
   end CreateMDI;

end ArchivesViews;
