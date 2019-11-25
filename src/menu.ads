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
with Gtk.Enums; use Gtk.Enums;

-- ****h* AZipGtk/Menu
-- FUNCTION
-- Provide code for the program main menu
-- SOURCE
package Menu is
-- ****

   -- ****v* Menu/Orientation
   -- FUNCTION
   -- Direction in which MDI window with archives lists will be split. Default
   -- is vertical.
   -- SOURCE
   Orientation: Gtk_Orientation := Orientation_Vertical;
   -- ****

   -- ****f* Menu/CreateMenu
   -- FUNCTION
   -- Create main menu bar and add it to the window.
   -- PARAMETERS
   -- WindowBox - Gtk_Vbox which will be contains the menu bar
   -- SOURCE
   procedure CreateMenu(WindowBox: Gtk_Vbox);
   -- ****

end Menu;
