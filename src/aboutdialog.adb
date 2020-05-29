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
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
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
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Utils; use Utils;

package body AboutDialog is

   Azip_Execute_Error: exception;

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
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
      Width, ReqestedWidth, Height: Natural := 0;
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
            "-column" & Natural'Image(Column) & " -row" & Natural'Image(Row) &
            " -sticky w");
      end AddLinkButton;
      procedure AddLabel(Name, Text, GridOptions: String) is
         Label: constant Ttk_Label := Create(Name, "-text {" & Text & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid(Label, GridOptions);
         ReqestedWidth := Natural'Value(Winfo_Get(Label, "reqwidth"));
         Height := Natural'Value(Winfo_Get(Label, "reqheight"));
         if ReqestedWidth > Width then
            Width := ReqestedWidth;
         end if;
      end AddLabel;
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      Label := Create(".aboutdialog.logo", "-image logo");
      Tcl.Tk.Ada.Grid.Grid(Label);
      AddLabel
        (".aboutdialog.infoframe.general",
         "AZip - A portable Zip Archive Manager", "-columnspan 2 -sticky w");
      AddLabel
        (".aboutdialog.infoframe.copyright",
         "Copyright © Gautier de Montmollin 2012 .. 2019",
         "-row 1 -columnspan 2");
      AddLabel
        (".aboutdialog.infoframe.license", "MIT Open Source License",
         "-row 2 -columnspan 2 -sticky w");
      AddLabel
        (".aboutdialog.infoframe.websize", "Internet: ", "-row 3 -sticky w");
      AddLinkButton
        (".aboutdialog.infoframe.websitebutton", "http://azip.sf.net", 3);
      AddLabel
        (".aboutdialog.infoframe.version", "Version:", "-row 4 -sticky w");
      AddLabel
        (".aboutdialog.infoframe.versionnumber", "2.36", "-column 1 -row 4");
      Tcl.Tk.Ada.Grid.Grid(InfoFrame, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Column_Configure(AboutDialog, InfoFrame, "-weight 1");
      AddLinkButton
        (".aboutdialog.librariesframe.gnatlink",
         "https://www.adacore.com/community", 0, 0,
         "GNAT - free Ada compiler");
      AddLabel
        (".aboutdialog.librariesframe.gnatversion",
         "version GPL 2017 (20170515-63)", "-column 1 -row 0");
      AddLinkButton
        (".aboutdialog.librariesframe.gnavi",
         "https://sourceforge.net/projects/gnavi/", 1, 0, "GNAVI / GWindows");
      AddLinkButton
        (".aboutdialog.librariesframe.reseditlink", "http://www.resedit.net/",
         2, 0, "ResEdit");
      AddLabel
        (".aboutdialog.librariesframe.reseditinfo",
         "(freeware, not open-source)", "-column 1 -row 2 -sticky w");
      AddLinkButton
        (".aboutdialog.librariesframe.zipadalink",
         "https://unzip-ada.sourceforge.io/", 3, 0, "Zip-Ada");
      AddLabel
        (".aboutdialog.librariesframe.zipadaversion",
         "version 55, ref 22-Now-2018", "-column 1 -row 3 -sticky w");
      AddLinkButton
        (".aboutdialog.librariesframe.inifilelink",
         "https://sourceforge.net/projects/ini-files/", 4, 0,
         "Ini file manager");
      Tcl.Tk.Ada.Grid.Grid
        (LibrariesFrame, "-column 0 -row 1 -columnspan 2 -sticky we");
      Height := Height * 14;
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
      Height := Height + Natural'Value(Winfo_Get(Button, "reqheight"));
      SetDialog(AboutDialog, "About AZip", Width, Height);
      return TCL_OK;
   end Show_About_Command;

   function Credits_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Credits_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      CreditsDialog: constant Tk_Toplevel :=
        Create(".creditsdialog", "-class Dialog");
      Frame: Ttk_LabelFrame;
      CloseButton: constant Ttk_Button :=
        Create
          (".creditsdialog.close",
           "-text Close -command {CloseDialog .creditsdialog}");
      Width, ReqestedWidth, Height: Natural := 0;
      procedure AddLabel(Name, Text: String) is
         Label: constant Ttk_Label := Create(Name, "-text {" & Text & "}");
      begin
         Tcl.Tk.Ada.Pack.Pack(Label, "-fill x -expand true");
         ReqestedWidth := Natural'Value(Winfo_Get(Label, "reqwidth"));
         Height := Natural'Value(Winfo_Get(Label, "reqheight"));
         if ReqestedWidth > Width then
            Width := ReqestedWidth;
         end if;
      end AddLabel;
   begin
      Frame :=
        Create
          (".creditsdialog.framezipada",
           "-text {Zip-Ada - Zip archive management library}");
      AddLabel
        (".creditsdialog.framezipada.label1",
         "Stratégies Software team: intensive profiling and contributions");
      AddLabel
        (".creditsdialog.framezipada.label2",
         "ITEC team at NXP Semiconductors: contributions");
      Tcl.Tk.Ada.Pack.Pack(Frame, "-fill x -expand true");
      Frame :=
        Create
          (".creditsdialog.framegwindows",
           "-text {GWindows - native MS Windows framework}");
      AddLabel
        (".creditsdialog.framegwindows.label1", "David Botton: main author");
      AddLabel(".creditsdialog.framegwindows.label2", "André van Splunter");
      AddLabel
        (".creditsdialog.framegwindows.label3",
         "Frank Piron, Falk Maier at KonAd GmbH: authors of GWindows Extended");
      Tcl.Tk.Ada.Pack.Pack(Frame, "-fill x -expand true");
      Frame :=
        Create
          (".creditsdialog.framemisc",
           "-text {Miscellaneous comments - contributions - ideas}");
      AddLabel
        (".creditsdialog.framemisc.label1",
         "Asen Anastassov, Nicolas F. Mirkov");
      Tcl.Tk.Ada.Pack.Pack(Frame, "-fill x -expand true");
      Height := Height * 9;
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Height := Height + Natural'Value(Winfo_Get(CloseButton, "reqheight"));
      SetDialog(CreditsDialog, "AZip Credits", Width, Height);
      return TCL_OK;
   end Credits_Command;

   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      Command: Unbounded_String;
      ProcessId: Process_Id;
   begin
      if OsName = "Windows" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("start").all);
      elsif OsName = "Linux" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("xdg-open").all);
      elsif OsName = "Darwin" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("open").all);
      end if;
      ProcessId :=
        Non_Blocking_Spawn
          (To_String(Command),
           Argument_String_To_List(CArgv.Arg(Argv, 1)).all);
      if ProcessId = Invalid_Pid then
         raise Azip_Execute_Error with "Can't open link";
      end if;
      return TCL_OK;
   end Open_Link_Command;

   procedure CreateAbout is
   begin
      AddCommand("ShowAbout", Show_About_Command'Access);
      AddCommand("ShowCredits", Credits_Command'Access);
      AddCommand("OpenLink", Open_Link_Command'Access);
   end CreateAbout;

end AboutDialog;
