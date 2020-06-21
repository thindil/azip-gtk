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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with ArchivesViews; use ArchivesViews;
with Utils; use Utils;

package body FindDialog is

   -- ****if* FindDialog/Show_Find_Dialog_Command
   -- FUNCTION
   -- Create and show find dialog to the user
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Show_Find_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Find_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FindDialog: Tk_Toplevel := Create(".finddialog", "-class Dialog");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      Label: Ttk_Label;
      TEntry: Ttk_Entry;
      ButtonBox: constant Ttk_Frame := Create(".finddialog.buttonbox");
      Button: Ttk_Button;
      Tokens: Slice_Set;
      FilesView: Ttk_Tree_View;
      IconWidth, Width, Height: Positive := 1;
   begin
      FilesView.Interp := Interp;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      Create(Tokens, Children(FilesView, "{}"), " ");
      -- If there no files in the currently selected archive, quit
      if Slice(Tokens, 1) = "" then
         Destroy(FindDialog);
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      -- Create find dialog UI
      Label := Create(".finddialog.findimage", "-image .toolbar.findicon");
      IconWidth := Positive'Value(Winfo_Get(Label, "reqwidth"));
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 1");
      Label :=
        Create
          (".finddialog.labelname",
           "-text {Entry name ( if empty: all names )}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      if Width < Positive'Value(Winfo_Get(Label, "reqwidth")) then
         Width := Positive'Value(Winfo_Get(Label, "reqwidth"));
      end if;
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      TEntry := Create(".finddialog.entryname");
      Tcl.Tk.Ada.Grid.Grid(TEntry, "-column 1 -row 1 -sticky we");
      Height := Height + Positive'Value(Winfo_Get(TEntry, "reqheight"));
      Label := Create(".finddialog.findimage2", "-image .toolbar.findicon");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 3");
      Label :=
        Create
          (".finddialog.labelcontent",
           "-text {Content ( if empty: all content )}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2");
      if Width < Positive'Value(Winfo_Get(Label, "reqwidth")) then
         Width := Positive'Value(Winfo_Get(Label, "reqwidth"));
      end if;
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      TEntry := Create(".finddialog.entrycontent");
      Tcl.Tk.Ada.Grid.Grid(TEntry, "-column 1 -row 3 -sticky we");
      Height := Height + Positive'Value(Winfo_Get(TEntry, "reqheight"));
      Button :=
        Create(".finddialog.buttonbox.ok", "-text Ok -command FindInArchive");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Height := Height + Positive'Value(Winfo_Get(Button, "reqheight"));
      Button :=
        Create
          (".finddialog.buttonbox.cancel",
           "-text Cancel -command {CloseDialog .finddialog}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Tcl.Tk.Ada.Grid.Grid(ButtonBox, "-column 1 -row 4 -sticky e");
      SetDialog
        (FindDialog, "AZip - find in archive", Width + IconWidth, Height);
      return TCL_OK;
   end Show_Find_Dialog_Command;

   -- ****if* FindDialog/Find_In_Archive_Command
   -- FUNCTION
   -- Search for the selected strings in the currently selected archive files
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Find_In_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Find_In_Archive_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FindDialog: Tk_Toplevel;
      TextEntry: Ttk_Entry;
      Name, Content, Values, FileName: Unbounded_String;
      FilesView: Ttk_Tree_View;
      EntriesFound, Occurences, OverallResult, Result: Natural := 0;
   begin
      FindDialog.Interp := Interp;
      FindDialog.Name := New_String(".finddialog");
      TextEntry.Interp := FindDialog.Interp;
      TextEntry.Name := New_String(".finddialog.entryname");
      Name := To_Unbounded_String(Get(TextEntry));
      TextEntry.Name := New_String(".finddialog.entrycontent");
      Content := To_Unbounded_String(Get(TextEntry));
      FilesView.Interp := Interp;
      FilesView.Name :=
        New_String
          (".mdi.archive" & Trim(Positive'Image(ActiveArchive), Both) &
           ".filesframe.fileslist");
      -- Search for the selected strings in the currently selected archive's
      -- files
      for I in 1 .. CurrentLastIndex loop
         if Exists(FilesView, Positive'Image(I)) = "1" then
            Values :=
              To_Unbounded_String
                (Item(FilesView, Positive'Image(I), "-values"));
            Tcl_Eval(FilesView.Interp, "lindex {" & To_String(Values) & "} 0");
            FileName :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Ada.Text_IO.Put_Line
              ("Looking for name: " & To_String(Name) & " in " &
               To_String(FileName));
            if Name = Null_Unbounded_String then
               Result := 1;
            else
               if Index(FileName, To_String(Name)) > 0 then
                  Result := 1;
               else
                  Result := 0;
               end if;
            end if;
            EntriesFound := EntriesFound + Result;
            OverallResult := Result;
            Ada.Text_IO.Put_Line
              ("Looking for content: " & To_String(Content) & " in " &
               To_String(FileName));
            Result := 0;
            Occurences := Occurences + Result;
            OverallResult := OverallResult + Result;
            Tcl_Eval
              (FilesView.Interp, "lrange {" & To_String(Values) & "} 0 10");
            Values :=
              To_Unbounded_String(Tcl.Ada.Tcl_GetResult(FilesView.Interp));
            Item
              (FilesView, Positive'Image(I),
               "-values [list " & To_String(Values) & "" &
               Natural'Image(OverallResult) & " ]");
         end if;
      end loop;
      Destroy(FindDialog);
      -- Show search result to the user. If user answer yes, show flat view of
      -- the currently selected archive, sorted by the result column
      if MessageBox
          ("-message {Search completed. " & LF & LF & "Occurences found:" &
           Natural'Image(Occurences) & " " & LF & "Total entries:" &
           Natural'Image(EntriesFound) &
           "} -icon question -type yesno -detail {Do you want to see full results (flat view & result sort)?}") =
        "yes" then
         Tcl_SetVar(FilesView.Interp, "viewtype", "flat");
         ToggleView;
         Heading(FilesView, "12", "-image arrow-down");
         SortArchive("Result");
      end if;
      return TCL_OK;
   end Find_In_Archive_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowFindDialog", Show_Find_Dialog_Command'Access);
      AddCommand("FindInArchive", Find_In_Archive_Command'Access);
   end AddCommands;

end FindDialog;
