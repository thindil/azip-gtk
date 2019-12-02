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

-- ****h* AZipGtk/ColumnsDialog
-- FUNCTION
-- Provide code for setting visible columns in archives view
-- SOURCE
package ColumnsDialog is
-- ****

   -- ****v* ColumnsDialog/VisibleColumns
   -- FUNCTION
   -- Store visibility of an archive columns list. Columns from 0 to 11: Name,
   -- Type, Modified, Attributes, Size, Packed, Ratio, Format, CRC 32, Path,
   -- Name encoding, Result.
   -- SOURCE
   VisibleColumns: array(0 .. 11) of Boolean :=
     (True, True, True, True, True, True, True, True, True, False, True, True);
   -- ****

   -- ****f* ColumnsDialog/ShowColumnsDialog
   -- FUNCTION
   -- Create and show setting columns visibility dialog to the user
   -- SOURCE
   procedure ShowColumnsDialog;
   -- ****

end ColumnsDialog;
