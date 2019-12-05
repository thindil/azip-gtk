-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Gtk.Box; use Gtk.Box;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Widget; use Gtk.Widget;
with MainWindow; use MainWindow;

package body HelpDialog is

   procedure ShowHelpDialog is
      Dialog: constant Gtk_Dialog := Gtk_Dialog_New("AZip Quick Help - a couple of tips and hints...", Window, Modal);
      DialogBox: constant Gtk_Box := Get_Content_Area(Dialog);
      Notebook: constant Gtk_Notebook := Gtk_Notebook_New;
      Box: Gtk_Vbox;
      InstallationLabel: constant Gtk_Label := Gtk_Label_New("AZip doesn't require any installation. It can even run from a USB stick for instance.");
      procedure AddFrame(Label, Text: String) is
         Frame: constant Gtk_Frame := Gtk_Frame_New(Label);
         FrameBox: constant Gtk_Hbox := Gtk_Hbox_New;
         FrameLabel: constant Gtk_Label := Gtk_Label_New(Text);
      begin
         Set_Line_Wrap(FrameLabel, True);
         Set_Max_Width_Chars(FrameLabel, 80);
         Pack_Start(FrameBox, FrameLabel);
         Add(Frame, FrameBox);
         Pack_Start(Box, Frame);
      end AddFrame;
   begin
      Box := Gtk_Vbox_New;
      AddFrame("Adding files and folders", "You can add files, or individual folders through menu commands (+) or buttons. BUT: you can also do it easily via Drag && Drop, from a Windows Explorer window or the Desktop, onto an AZip archive window. Any mix of dragged folders and files is supported.");
      AddFrame("Unpacking files", "You can extract selected files, the selected folder, or the entire archive via the Extract command (Ctrl+E) or a button. BUT: you can also extract files via Drag && Drop to a Windows Explorer window or to the Desktop.");
      Append_Page(Notebook, Box, Gtk_Label_New("User Interface"));
      Box := Gtk_VBox_New;
      Set_Line_Wrap(InstallationLabel, True);
      Set_Max_Width_Chars(InstallationLabel, 80);
      Pack_Start(Box, InstallationLabel);
      Append_Page(Notebook, Box, Gtk_Label_New("Installation"));
      Box := Gtk_VBox_New;
      Append_Page(Notebook, Box, Gtk_Label_New("Command-line"));
      Pack_Start(DialogBox, Notebook);
      Show_All(DialogBox);
      -- Add Ok button to dialog
      if Add_Button(Dialog, "Ok", Gtk_Response_OK) = null then
         return;
      end if;
      -- Show dialog to the user
      if Run(Dialog) /= Gtk_Response_Cancel then
         Destroy(Dialog);
      end if;
   end ShowHelpDialog;

end HelpDialog;
