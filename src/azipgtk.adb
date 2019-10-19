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

with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtk.Main; use Gtk.Main;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with MainWindow; use MainWindow;

procedure AZipGtk is
   Builder: Gtkada_Builder;
   Error: aliased GError;
begin
   if not Ada.Environment_Variables.Exists("RUNFROMSCRIPT") and
     Dir_Separator = '/' then
      Put_Line
        ("The program can be run only via 'run.sh' script. Please don't run binary directly.");
      return;
   end if;
   -- Start Gettext internationalization
   Setlocale;
   Bind_Text_Domain("hunter", Value("LOCALESDIR"));
   Text_Domain("hunter");
   -- Start GTK
   Init;
   Gtk_New(Builder);
   if Add_From_File(Builder, "azip.glade", Error'Access) = Guint(0) then
      Put_Line("Error : " & Get_Message(Error));
      return;
   end if;
   CreateMainWindow(Builder);
   Clear("LD_LIBRARY_PATH");
   Clear("GDK_PIXBUF_MODULE_FILE");
   Clear("GDK_PIXBUF_MODULEDIR");
   Clear("FONTCONFIG_FILE");
   Clear("RUNFROMSCRIPT");
   Clear("GSETTINGS_BACKEND");
   Main;
exception
   when An_Exception : others =>
      declare
         ErrorFile: File_Type;
      begin
         if Ada.Directories.Exists("error.log") then
            Open(ErrorFile, Append_File, "error.log");
         else
            Create(ErrorFile, Append_File, "error.log");
         end if;
         Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
         Put_Line(ErrorFile, "0.1");
         Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
         Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Put(ErrorFile, Symbolic_Traceback(An_Exception));
         Put_Line
           (ErrorFile, "-------------------------------------------------");
         Close(ErrorFile);
         Put_Line
           ("Oops, something bad happen and program crashed. File 'error.log' (should be in this same directory) contains all traceback.");
      end;
end AZipGtk;
