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

package body MenuBar is

   procedure CreateMenuBar(MainWindow: Frame) is
      Menubar, FileMenu: Menu;
   begin
      Menubar := Create(".menubar", "-borderwidth 0");
      FileMenu := Create(".menubar.file", "-tearoff false");
      Add(FileMenu, "command", "-label ""New archive""");
      Add(FileMenu, "command", "-label ""Open archive...""");
      Add(FileMenu, "command", "-label ""Save archive as ...""");
      Add(FileMenu, "command", "-label ""Close archive""");
      Add(FileMenu, "separator");
      Add(FileMenu, "command", "-label Properties");
      Add(FileMenu, "separator");
      Add(FileMenu, "command", "-label Recent");
      Add(FileMenu, "separator");
      Add(FileMenu, "command", "-label Quit -command exit");
      Add(Menubar, "cascade", "-menu .menubar.file -label File");
      Add(Menubar, "command", "-label Edit");
      Add(Menubar, "command", "-label Tools");
      Add(Menubar, "command", "-label View");
      Add(Menubar, "command", "-label Options");
      Add(Menubar, "command", "-label Window");
      Add(Menubar, "command", "-label Help");
      configure(MainWindow, "-menu .menubar");
   end;

end MenuBar;
