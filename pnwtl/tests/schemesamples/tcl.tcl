#--------------------------------------------------------------------------
# Displays a dialog box with the specified title that allows the user to
# specify the parameters for a search. Returns 1 if a valid search has
# been specified, and 0 if the dialog was cancelled out of. The pattern
# and type of search are obtained from the GLOBAL array named $array; this
# array is also used to store widget states (temporarily).
#--------------------------------------------------------------------------
#  search(pattern) - the string or pattern to search for
#  search(type)    - the type of search: exact, nocase or regexp
#  search(sel)     - index of the selected button widget
#--------------------------------------------------------------------------
proc NB_searchDialog {array title} {
   upvar #0 $array search

   toplevel .dialog -bd 10
   wm title .dialog $title
   wm resizable .dialog 0 0

   # create and pack the frames for the pattern entry, options radiobuttons
   # and OK and Cancel buttons frames:
   frame .dialog.pattern
   frame .dialog.options
   frame .dialog.buttons
   pack .dialog.pattern .dialog.options .dialog.buttons -side top

   # create and pack the pattern entry box and label:
   label .dialog.pattern.label -text "Search Pattern:" -underline 0 -padx 0
   set pattern [entry .dialog.pattern.entry -textvariable ${array}(pattern) \
                                            -width 30]
   pack .dialog.pattern.label $pattern -side left

   # select everything in the entry widget (to allow reuse and overriding):
   $pattern select range 0 end
 
   # create and pack the options radiobuttons:
   set exact [radiobutton .dialog.options.exact -text "Exact Match" \
                 -variable ${array}(type) -value {-exact}]
   set nocase [radiobutton .dialog.options.nocase -text "Ignore Case" \
                -variable ${array}(type) -value {-nocase}]
   set regExp [radiobutton .dialog.options.regexp -text "Regular Expression" \
                -variable ${array}(type) -value {-regexp}]
   pack $exact $nocase $regExp -side top -anchor w

   # select the option button last selected, or $exact if there has been
   # no previous selection:
   if { [info exists search(type)] == 0 || \
        [string length $search(type)] == 0} {
      $exact select
      } \
   else {
      .dialog.options.[string tolower [string trim $search(type) -]] select
      }
      
   # create and pack the OK and cancel buttons:
   set ok [button .dialog.buttons.ok -text OK -underline 0 \
                            -command "NB_quitOnValidSearchPattern $array 1"]
   set cancel [button .dialog.buttons.cancel -text Cancel -underline 0 \
                            -command "set ${array}(sel) 0"]
   pack $ok $cancel -side left -padx 10 -pady 5

   # set up bindings for accelerators. Putting the .dialog bindtag first
   # allows the Alt sequences to be handled by the dialog before they're
   # added to the entry widget:
   foreach w [list $pattern $exact $nocase $regExp $ok $cancel] {
      bindtags $w [list .dialog [winfo class $w] $w all]
      }
   bind .dialog <Alt-o>   "focus $ok; break"
   bind .dialog <Alt-c>   "focus $cancel; break"
   bind .dialog <Alt-s>   "focus $pattern; break"
   bind .dialog <Alt-Key> "break"

   # handle pressing the Return key in various widgets:
   bind $cancel  <Return>  "$cancel invoke; break"
   bind $ok      <Return>  "$ok invoke; break"
   bind $pattern <Return>  "$ok invoke; break"
   # pressing Return in other widgets has no effect

   # put up the dialog box and get the search parameters:
   focus $pattern
   grab  .dialog
   tkwait variable ${array}(sel)
   grab release .dialog
   destroy .dialog
   return $search(sel)

   }  ;# proc NB_searchDialog {array title}