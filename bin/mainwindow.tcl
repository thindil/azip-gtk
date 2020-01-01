menu .menubar -borderwidth 0
menu .menubar.file -tearoff false
.menubar.file add command -label "New archive"
.menubar.file add command -label "Open archive..."
.menubar.file add command -label "Save archive as..."
.menubar.file add command -label "Close archive"
.menubar.file add separator
.menubar.file add command -label "Properties"
.menubar.file add separator
.menubar.file add command -label "Recent"
.menubar.file add separator
.menubar.file add command -label "Quit" -command {exit}
.menubar add cascade -menu .menubar.file -label File
.menubar add command -label Edit
.menubar add command -label Tools
.menubar add command -label View
.menubar add command -label Options
.menubar add command -label Window
.menubar add command -label Help
. configure -menu .menubar
wm title . "AZip"
frame .toolbar -relief groove -borderwidth 1
button .toolbar.new -text "New"
pack .toolbar.new -side left
button .toolbar.open -text "Open"
pack .toolbar.open -side left
pack .toolbar -fill x
frame .mdi
pack .mdi -fill both -expand true
