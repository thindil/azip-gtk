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
with Tcl.Ada;
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
with Dialogs; use Dialogs;

package body HelpDialog is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   procedure ShowHelp is
      OptionsDialog: constant Tk_Toplevel :=
        Create(".helpdialog", "-class Dialog");
      CloseButton: constant Ttk_Button :=
        Create
          (".helpdialog.closebutton",
           "-text Close -command {CloseDialog .helpdialog}");
      HelpNoteBook: constant Ttk_Notebook := Create(".helpdialog.notebook");
      HelpFrame: Ttk_Frame;
      procedure AddSubFrame(FrameName, ImageName, Text: String) is
         SubHelpFrame: Ttk_LabelFrame;
         Label: Ttk_Label;
         Image: Tk_Photo;
      begin
         SubHelpFrame := Create(FrameName, "-text {Adding files and folders}");
         Image :=
           Create
             (ImageName & "icon",
              "-file {" & Containing_Directory(Command_Name) &
              Directory_Separator & "plus.gif}");
         Label :=
           Create
             (Widget_Image(SubHelpFrame) & ".image",
              "-image " & Widget_Image(Image));
         Tcl.Tk.Ada.Pack.Pack(Label, "-side left");
         Label :=
           Create
             (Widget_Image(SubHelpFrame) & ".label",
              "-text {" & Text & "} -wraplength 550");
         Tcl.Tk.Ada.Pack.Pack(Label, "-side right");
         Tcl.Tk.Ada.Pack.Pack(SubHelpFrame);
      end AddSubFrame;
   begin
      HelpFrame := Create(".helpdialog.notebook.userinterface");
      AddSubFrame
        (Widget_Image(HelpFrame) & ".adding", "plus.gif",
         "You can add files, or individual folders through menu commands (+) or buttons. BUT: you can also do it easily via Drag && Drop, from a Windows Explorer window or the Desktop, onto an AZip archive window. Any mix of dragged folders and files is supported.");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {User Interface}");
      HelpFrame := Create(".helpdialog.notebook.installation");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {Installation}");
      HelpFrame := Create(".helpdialog.notebook.commandline");
      Add(HelpNoteBook, Widget_Image(HelpFrame), "-text {Command-line}");
      Tcl.Tk.Ada.Pack.Pack(HelpNoteBook);
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      SetDialog
        (OptionsDialog, "AZip - Quick Help, general tips and hints", 600, 400);
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
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise Program_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
   begin
      AddCommand("ShowHelp", Show_Help_Command'Access);
   end AddCommands;

   procedure CreateHelp is
   begin
      AddCommands;
   end CreateHelp;

end HelpDialog;
