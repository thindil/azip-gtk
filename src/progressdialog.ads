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

-- ****h* AzipTk/ProgressDialog
-- FUNCTION
-- Provide code to show progress of the selected action
-- SOURCE
package ProgressDialog is
-- ****

   -- ****f* ProgressDialog/CreateProgressDialog
   -- FUNCTION
   -- Create a new progress dialog and show it to the user
   -- PARAMETERS
   -- Title - Title of the progress dialog
   -- SOURCE
   procedure CreateProgressDialog(Title: String);
   -- ****

   -- ****f* ProgressDialog/DeleteProgressDialog
   -- FUNCTION
   -- Remove the current progress dialog
   -- SOURCE
   procedure DeleteProgressDialog;
   -- ****

   -- ****f* ProgressDialog/UpdateProgress
   -- FUNCTION
   -- Update action progress on the current progress dialog
   -- SOURCE
   procedure UpdateProgress;
   -- ****

end ProgressDialog;
