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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkNotebook; use Tcl.Tk.Ada.Widgets.TtkNotebook;
with ColumnsDialog; use ColumnsDialog;
with Utils; use Utils;

package body HelpDialog is

   procedure ShowHelp is
      OptionsDialog: constant Tk_Toplevel :=
        Create(".helpdialog", "-class Dialog");
      CloseButton: constant Ttk_Button :=
        Create
          (".helpdialog.closebutton",
           "-text Close -command {CloseDialog .helpdialog}");
      HelpNoteBook: constant Ttk_Notebook := Create(".helpdialog.notebook");
      HelpFrame: Ttk_Frame;
      procedure AddSubFrame(FrameName, FrameTitle, ImageName, Text: String) is
         SubHelpFrame: Ttk_LabelFrame;
         Label: Ttk_Label;
         Image: Tk_Photo;
         Length: Positive := 600;
      begin
         SubHelpFrame := Create(FrameName, "-text {" & FrameTitle & "}");
         if ImageName /= "" then
            Image :=
              Create
                (ImageName & "icon",
                 "-file {" & Containing_Directory(Command_Name) &
                 Directory_Separator & ImageName & "}");
            Label :=
              Create
                (Widget_Image(SubHelpFrame) & ".image",
                 "-image " & Widget_Image(Image));
            Tcl.Tk.Ada.Pack.Pack(Label, "-side left");
            Length := 550;
         end if;
         Label :=
           Create
             (Widget_Image(SubHelpFrame) & ".label",
              "-text {" & Text & "} -wraplength" & Positive'Image(Length));
         Tcl.Tk.Ada.Pack.Pack(Label, "-side right");
         Tcl.Tk.Ada.Pack.Pack(SubHelpFrame);
      end AddSubFrame;
   begin
      HelpFrame := Create(".helpdialog.notebook.userinterface");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".adding", "Adding files and folders",
         "plus.gif",
         "You can add files, or individual folders through menu commands (+) or buttons. BUT: you can also do it easily via Drag && Drop, from a Windows Explorer window or the Desktop, onto an AZip archive window. Any mix of dragged folders and files is supported.");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".unpacking", "Unpacking files",
         "drag_unpack.gif",
         "You can extract selected files, the selected folder, or the entire archive via the Extract command (Ctrl+E) or a button. BUT: you can also extract files via Drag && Drop to a Windows Explorer window or to the Desktop.");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {User Interface}");
      HelpFrame := Create(".helpdialog.notebook.installation");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".stealth",
         "Using AZip as a portable software - stealth mode", "no_regedit.gif",
         "For convenience, by default, AZip writes user settings in the registry, as standard Windows software does. If you want the registry NOT being written to, you can add a file, azip.cfg (can be empty), in the same directory as azip*.exe. User settings will be recorded there. If the file is read-only, it simply won't be changed, and settings won't be saved.");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {Installation}");
      HelpFrame := Create(".helpdialog.notebook.commandline");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".parameters", "Command-line parameters",
         "",
         "The activation of AZip with command-line parameters for specific operations is under construction");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".tools",
         "Command-line tools with console output", "za_console.gif",
         "Pure command-line tools corresponding to AZip are located in the Zip-Ada project (zipada, unzipada, rezip, find_zip, comp_zip). Follow hyperlink in the About box for download.");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {Command-line}");
      Tcl.Tk.Ada.Pack.Pack(HelpNoteBook);
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      SetDialog
        (OptionsDialog, "AZip - Quick Help, general tips and hints", 600, 300);
   end ShowHelp;

   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowHelp;
      return TCL_OK;
   end Show_Help_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowHelp", Show_Help_Command'Access);
   end AddCommands;

   procedure CreateHelp is
   begin
      AddCommands;
   end CreateHelp;

end HelpDialog;
