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
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;

package ArchivesViews is

   ColumnsNames: constant array(1 .. 12) of Unbounded_String :=
     (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
      To_Unbounded_String("Modified"), To_Unbounded_String("Attributes"),
      To_Unbounded_String("Size"), To_Unbounded_String("Packed"),
      To_Unbounded_String("Ratio"), To_Unbounded_String("Format"),
      To_Unbounded_String("CRC 32"), To_Unbounded_String("Path"),
      To_Unbounded_String("Name encoding"), To_Unbounded_String("Result"));

   procedure CreateMDI;

private

   MDI: Ttk_PanedWindow;

   procedure SetActive(NewActive: Positive; Created: Boolean := False);
   procedure CreateView;
   procedure LoadArchive(FileName: String);
   procedure ExtractArchive(Directory: String);
   procedure AddFiles
     (FileName: String; Encrypted: Boolean; Path: String := "";
      Hide: Boolean := False);
   procedure SaveArchiveAs;
   procedure DeleteItems;
   procedure SortArchive(Column: String);
   procedure TestArchive;
   procedure ShowFindDialog;
   procedure FindInArchive;
   procedure ToggleView;
   procedure AddDirectory(DirectoryName: String; Encrypted: Boolean);
   procedure ShowFiles;
   procedure UpdateArchive;
   procedure RecompressArchive;
   procedure ShowProperties;
   procedure ToggleSelect(SelectAll: Boolean);
   procedure DeleteDirectory;
   procedure ExtractFile(Directory: String);

end ArchivesViews;
