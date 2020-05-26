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

with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Utils; use Utils;

package body ProgressDialog is

   ProgressDialog: Tk_Toplevel;
   ProgressBar: Ttk_ProgressBar;

   procedure CreateProgressDialog(Title: String) is
   begin
      ProgressDialog := Create(".progressdialog", "-class Dialog");
      ProgressBar :=
        Create
          (".progressdialog.progressbar",
           "-orient horizontal -length 250 -value 0");
      SetDialog(ProgressDialog, "Azip - " & Title, 275, 50);
      Tcl.Tk.Ada.Pack.Pack(ProgressBar, "-expand true");
   end CreateProgressDialog;

   procedure DeleteProgressDialog is
   begin
      Destroy(ProgressDialog);
   end DeleteProgressDialog;

   procedure UpdateProgress is
   begin
      Step(ProgressBar);
   end UpdateProgress;

end ProgressDialog;
