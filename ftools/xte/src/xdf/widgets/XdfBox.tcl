#
# XdfBox
# ----------------------------------------------------------------------
# Implements an (optionally editable) selection box widget using
# primitive widgets as the building blocks.  A editiable selection box
# displays a list of items along with an edit button that can be used to
# pop an window for editing the contents of the selection box.  This
# class merely wraps the SelectBox together with a few extra widgets for
# Editing.
#
#   PUBLIC ATTRIBUTES:
#
#     -title ......... title string displayed above selectbox
#     -helptext ...... text for the help widget
#     -dlist ......... default list of items (for reset button)
#
#     -edit .......... make editable?  "true" or "false"
#     -mode .......... single/multi selection
#     -action ........ callback invoked whenever entry is selected/unselected
#
#     -list .......... list of items to be displayed
#     -width ......... width of displayed list in characters or "expand"
#     -height ........ height of displayed list in lines or "expand"
#
#   METHODS:
#
#     config ....... used to change public attributes
#     get .......... returns "all", "selected" or "showing" list
#     select ....... select/unselect entries programmatically
#     help ......... open help widget and fill with helptext
#     reset ........ reset list to default list
#
#   X11 OPTION DATABASE ATTRIBUTES
#
#     selectBackground ... background color for selected entries
#     selectForeground ... foreground color for selected entries
#
#     listBackground ..... background color for entries
#     listForeground ..... foreground color for entries
#
#     ...and the rest of the usual widget attributes
#
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  FilteredBox.tcl,v 1.3 1994/04/08 13:39:16 mmc Exp
# ----------------------------------------------------------------------
#               Copyright (c) 1993  AT&T Bell Laboratories
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of AT&T Bell Laboratories
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# AT&T disclaims all warranties with regard to this software, including
# all implied warranties of merchantability and fitness.  In no event
# shall AT&T be liable for any special, indirect or consequential
# damages or any damages whatsoever resulting from loss of use, data or
# profits, whether in an action of contract, negligence or other
# tortuous action, arising out of or in connection with the use or
# performance of this software.
# ======================================================================

itcl_class XdfBox {
	# ------------------------------------------------------------------
	#  CONSTRUCTOR - create new [editable] selectbox
	# ------------------------------------------------------------------
	constructor {config} {
		#
		#  Create a window with the same name as this object
		#
		set class [$this info class]
		::rename $this $this-tmp-
		::frame $this -class $class -relief flat -borderwidth 8
		::rename $this $this-win-
		::rename $this-tmp- $this

		frame $this.s -relief sunken -borderwidth 1
		frame $this.s.r -relief raised -borderwidth 1
		pack append $this.s $this.s.r {top expand fill}

		SelectBox $this.s.r.list
		$this.s.r.list-win- config -borderwidth 4

		label $this.s.r.title -text "$title" -relief ridge \
			-borderwidth 5

                button $this.help -text "Help" -command \
			"help {$title} $helptext"

		button $this.all -text "All" -command "$this select all"

                button $this.none -text "None" -command "$this select reset"

                button $this.reset -text "Reset" -command \
			"$this config -list \[$this get defaults\]"

                button $this.edit -text "Edit" -command \
			"$this config -list \[edit {$title} \[$this get all\]\]"
		pack append $this.s.r \
			$this.s.r.list {top expand fill}

		pack append $this $this.s {top expand fill} \
			$this.help {left expand fill} \
			$this.all {left expand fill} \
			$this.none {left expand fill} \
			$this.reset {left expand fill}

		if {$edit == "true"} {
		    pack append $this $this.edit {left expand fill}
		}
		#
		#  Explicitly handle config's that may have been ignored earlier
		#
		foreach attr $config {
			config -$attr [set $attr]
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  config - used to change public attributes
	# ------------------------------------------------------------------
	method config {config} {}

	# ------------------------------------------------------------------
	#  DESTRUCTOR - destroy window containing widget
	# ------------------------------------------------------------------
	destructor {
		$this.s.r.list delete
		::rename $this-win- {}
		destroy $this
	}

	# ------------------------------------------------------------------
	#  METHOD:  help - open help widget and fill with helptext
	# ------------------------------------------------------------------
#	method help {window title text} {
#	    global prompt

#            puts stderr {Hello, from help method!}

#	    set f [toplevel .help_$window -borderwidth -10]

#	    wm title $f "Help XDF $title"
#	    wm iconname $f "Help XDF $title"

#	    set t [text $f.t -setgrid true -wrap word \
#		    -width 60 -height 15 \
#		    -yscrollcommand "$f.sy set"]
#	    scrollbar $f.sy -orient vert -command "$f.t yview"
#	    button $f.exit -text Exit -command {set prompt(exit) 1}

#	    $t insert end $text
#	    pack $f.sy -side right -fill y
#	    pack $f.t -side top -fill both -expand true
#	    pack $f.exit -side left -fill x -expand true

#	    focus $f.exit
#	    grab $f
#	    tkwait variable prompt(exit)
#	    grab release $f
#	    destroy $f
#	}

	# ------------------------------------------------------------------
	#  METHOD:  get - returns "all", "selected" or "showing" lists
	# ------------------------------------------------------------------
	method get {{what all}} {
		switch $what {
			all {
				return $list
			}
			selected {
				return [$this.s.r.list get selected]
			}
			showing {
				return [$this.s.r.list get all]
			}
			defaults {
				return $dlist
			}
			default {
				error "invalid arg \"$what\": should be all, selected or showing"
			}
		}
	}

	# ------------------------------------------------------------------
	#  METHOD:  select - public access for highlighting entries
	#   USAGE:  select all
	#           select reset
	#           select entry label state
	# ------------------------------------------------------------------
	method select {args} {
		eval $this.s.r.list select $args
	}

	#
	#  PUBLIC DATA: XdfBox-specific options
	#
	public edit {
		if {[winfo exists $this]} {
			if {"$edit" != ""} {
				$this.s.r.list config -edit $edit
			} else {
				$this.s.r.list config -edit ""
			}
		}
	}
	public title "" {
		if {[winfo exists $this]} {
			if {$title != ""} {
				$this.s.r.title config -text " $title "
			} else {
				$this.s.r.title config -text ""
			}
		}
	}
	public helptext {
		if {[winfo exists $this]} {
			if {"$helptext" != ""} {
				$this.s.r.list config -helptext $helptext
			} else {
				$this.s.r.list config -helptext ""
			}
		}
	}
	public list {} {
		if {[winfo exists $this]} {
#			set list [lsort $list]
			$this.s.r.list config -list $list
		}
	}
	public dlist {} {
		if {[winfo exists $this]} {
		        set dlist $dlist
		}
	}

	#
	#  PUBLIC DATA: SelectBox options
	#
	public mode multi {
		if {[winfo exists $this]} {
			$this.s.r.list config -mode $mode
		}
	}
	public action {} {
		if {[winfo exists $this]} {
			$this.s.r.list config -action $action
		}
	}
	public width 30 {
		if {[winfo exists $this]} {
			$this.s.r.list config -width $width
		}
	}
	public height 10 {
		if {[winfo exists $this]} {
			$this.s.r.list config -height $height
		}
	}
}


