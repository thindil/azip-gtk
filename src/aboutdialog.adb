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
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with AboutDialog.Commands;
with Utils; use Utils;

package body AboutDialog is

   procedure CreateAbout is
   begin
      AboutDialog.Commands.AddCommands;
   end CreateAbout;

   procedure ShowAbout is
      AboutDialog: constant Tk_Toplevel :=
        Create(".aboutdialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      ButtonBox: constant Ttk_Frame := Create(".aboutdialog.buttonbox");
      Button: Ttk_Button;
      Label: Ttk_Label;
      InfoFrame: constant Ttk_Frame := Create(".aboutdialog.infoframe");
      LibrariesFrame: constant Ttk_LabelFrame :=
        Create
          (".aboutdialog.librariesframe",
           "-text {AZip is made with the following free, open source components}");
      procedure AddLinkButton
        (Name, URL: String; Row: Natural; Column: Natural := 1;
         Text: String := "") is
         ButtonText: Unbounded_String;
      begin
         if Text = "" then
            ButtonText := To_Unbounded_String(URL);
         else
            ButtonText := To_Unbounded_String(Text);
         end if;
         Button :=
           Create
             (Name,
              "-text {" & To_String(ButtonText) &
              "} -style Toolbutton -command {OpenLink " & " " & URL & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Button,
            "-column" & Natural'Image(Column) & " -row" & Natural'Image(Row));
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
      AddLinkButton
        (".aboutdialog.librariesframe.gnatlink",
         "https://www.adacore.com/community", 0, 0,
         "GNAT - free Ada compiler");
      Label :=
        Create
          (".aboutdialog.librariesframe.gnatversion",
           "-text {version GPL 2017 (20170515-63)}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      AddLinkButton
        (".aboutdialog.librariesframe.gnavi",
         "https://sourceforge.net/projects/gnavi/", 1, 0, "GNAVI / GWindows");
      AddLinkButton
        (".aboutdialog.librariesframe.reseditlink", "http://www.resedit.net/",
         2, 0, "ResEdit");
      Label :=
        Create
          (".aboutdialog.librariesframe.reseditinfo",
           "-text {(freeware, not open-source)}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2");
      AddLinkButton
        (".aboutdialog.librariesframe.zipadalink",
         "https://unzip-ada.sourceforge.io/", 3, 0, "Zip-Ada");
      Label :=
        Create
          (".aboutdialog.librariesframe.zipadaversion",
           "-text {version 55, ref 22-Now-2018}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 3");
      AddLinkButton
        (".aboutdialog.librariesframe.inifilelink",
         "https://sourceforge.net/projects/ini-files/", 4, 0,
         "Ini file manager");
      Tcl.Tk.Ada.Grid.Grid
        (LibrariesFrame, "-column 0 -row 1 -columnspan 2 -sticky we");
      Button :=
        Create
          (".aboutdialog.buttonbox.credits",
           "-text Credits -command {ShowCredits}");
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

   procedure ShowCredits is
      CreditsDialog: constant Tk_Toplevel :=
        Create(".creditsdialog", "-class Dialog");
      Frame: Ttk_LabelFrame;
      Label: Ttk_Label;
      CloseButton: constant Ttk_Button :=
        Create
          (".creditsdialog.close",
           "-text Close -command {CloseDialog .creditsdialog}");
   begin
      Frame :=
        Create
          (".creditsdialog.framezipada",
           "-text {Zip-Ada - Zip archive management library}");
      Label :=
        Create
          (".creditsdialog.framezipada.label1",
           "-text {Stratégies Software team: intensive profiling and contributions}");
      Tcl.Tk.Ada.Pack.Pack(Label);
      Label :=
        Create
          (".creditsdialog.framezipada.label2",
           "-text {ITEC team at NXP Semiconductors: contributions}");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x -expand true");
      Tcl.Tk.Ada.Pack.Pack(Frame, "-fill x -expand true");
      Frame :=
        Create
          (".creditsdialog.framegwindows",
           "-text {GWindows - native MS Windows framework}");
      Label :=
        Create
          (".creditsdialog.framegwindows.label1",
           "-text {David Botton: main author}");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x -expand true");
      Label :=
        Create
          (".creditsdialog.framegwindows.label2",
           "-text {André van Splunter}");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x -expand true");
      Label :=
        Create
          (".creditsdialog.framegwindows.label3",
           "-text {Frank Piron, Falk Maier at KonAd GmbH: authors of GWindows Extended}");
      Tcl.Tk.Ada.Pack.Pack(Label);
      Tcl.Tk.Ada.Pack.Pack(Frame);
      Frame :=
        Create
          (".creditsdialog.framemisc",
           "-text {Miscellaneous comments - contributions - ideas}");
      Label :=
        Create
          (".creditsdialog.framemisc.label1",
           "-text {Asen Anastassov, Nicolas F. Mirkov}");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x -expand true");
      Tcl.Tk.Ada.Pack.Pack(Frame, "-fill x -expand true");
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      SetDialog(CreditsDialog, "AZip Credits", 500, 300);
   end ShowCredits;

end AboutDialog;
