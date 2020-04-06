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
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with ArchivesViews; use ArchivesViews;

package body AboutDialog is

   procedure ShowAbout is
      AboutDialog: constant Tk_Toplevel :=
        Create(".aboutdialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      ButtonBox: constant Ttk_Frame := Create(".aboutdialog.buttonbox");
      Button: Ttk_Button;
      Label: Ttk_Label;
      InfoFrame: constant Ttk_Frame := Create(".aboutdialog.infoframe");
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      procedure AddLinkButton(Name, URL: String; Row: Natural) is
         Command: Unbounded_String;
      begin
         if OsName = "Windows" then
            Command := To_Unbounded_String("start");
         elsif OsName = "Linux" then
            Command := To_Unbounded_String("xdg-open");
         elsif OsName = "Darwin" then
            Command := To_Unbounded_String("open");
         end if;
         Button :=
           Create
             (Name,
              "-text {" & URL & "} -style Toolbutton -command {exec " &
              To_String(Command) & " " & URL & "}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row" & Natural'Image(Row));
      end AddLinkButton;
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      Label := Create(".aboutdialog.logo", "-image logo");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label :=
        Create
          (".aboutdialog.infoframe.general",
           "-text {AZip - A portable Zip Archive Manager}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Label :=
        Create
          (".aboutdialog.infoframe.copyright",
           "-text {Copyright © Gautier de Montmollin 2012 .. 2019}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 1 -columnspan 2");
      Label :=
        Create
          (".aboutdialog.infoframe.license",
           "-text {MIT Open Source License}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 2 -columnspan 2");
      Label := Create(".aboutdialog.infoframe.website", "-text {Internet:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 3");
      AddLinkButton
        (".aboutdialog.infoframe.websitebutton", "http://azip.sf.net", 3);
      Label := Create(".aboutdialog.infoframe.version", "-text {Version:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 4");
      Label := Create(".aboutdialog.infoframe.versionnumber", "-text {2.36}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 4");
      Tcl.Tk.Ada.Grid.Grid(InfoFrame, "-column 1 -row 0");
      Button :=
        Create
          (".aboutdialog.buttonbox.credits",
           "-text Credits -command {CloseDialog .aboutdialog}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".aboutdialog.buttonbox.close",
           "-text Close -command {CloseDialog .aboutdialog}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid
        (ButtonBox, "-column 1 -row 2 -sticky we -columnspan 2");
      SetDialog(AboutDialog, "About AZip", 500, 400);
   end ShowAbout;

end AboutDialog;
