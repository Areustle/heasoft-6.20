########################################################################
#
#   class:  Notifications
#
# purpose:  Provide a message distribution center for special events.
#           Any number of objects/users can ask to be notified when
#           a given event occurs.
#
#   usage:  To create/access the notification center:
#                   set newObj [gNotifications]
#              or
#                   set oldObj [gNotifications default]
#              where the first example creates a new notification center
#              and the later returns pre-existing one of an unknown
#              name (or creates a new one if one doesn't already exist).
#              In most cases, an application should have only one
#              notification object, so either create one and store it in
#              a global variable, or always access it through the "default"
#              method.
#
#           To send a message:
#                   NotificationObj postMessage object message ?args?
#              where "object" is the object to which "message" applies,
#              usually the sending object.  "message" is a string
#              describing the event which has just taken place...
#              eg, "graphHasFinishedDrawing".  "args" contains extra
#              information observers may use.
#
#           To register to receive messages:
#                  NotificationObj addObserver observer cmd object message
#              where "observer" is the object (or function name) to be
#              called when "message" is posted by (or for) "object".  "cmd"
#              is either the observing object's method to be used or a
#              extra/dummy argument passed to an observing function.  An
#              observing object needs to implement the method:
#                   body observer::cmd { object message opts } {...}
#              while an observing function implements the procedure:
#                   proc observer { cmd object message opts } {...}
#              where opts is an optional list of additional information
#              sent by the object.
#
#           To unregister:
#                  NotificationObj removeObserver observer ?object? ?message?
#              where "observer" is the same as before.  An observer can
#              unregister for all messages or just ones from particular
#              objects and messages.
#           
#######################################################################

itcl::class Notifications {

   constructor {} {}
   destructor {}

   public {
      method addObserver    { observer cmd object message }
      method removeObserver { observer {object ""} {message ""} }
      method postMessage    { object message args }

      method registerRemote { rNotes } { set remoteNotes $rNotes }
   }

   private {
      variable lookup
      variable remoteNotes ""

      method locateObserver { observer observerList }
   }
}

#######################################################################
#
#  gNotifications ?default?
#
#  Use this procedure to create/access instances of Notifications in
#  the global namespace
#
#######################################################################

proc gNotifications { args } {
   global notificationList

   switch -exact [lindex $args 0] {
       "default" {
            set args ""
            set gNote [lindex [itcl::find objects ::* -class Notifications] 0]
            if { $gNote != "" } {
               return $gNote
            }
            return [uplevel #0 Notifications #auto $args]
       }
       default {
            set user [lindex $args 0]
            if { [llength $notificationList] == 1 && [lindex [lindex $notificationList 0] 0] == "NoOne" } {
               # case 1: only one notification, place user in this one
               set newList [list $user [lindex [lindex $notificationList 0] 1]]
               set notificationList {}
               lappend notificationList $newList
               return [lindex [lindex $notificationList 0] 1]
            } else {
               set idx [lsearch -glob $notificationList [list $user *]]
               if { $idx >= 0 } {
                  return [lindex [lindex $notificationList $idx] 1]
               } else {
                  return [uplevel #0 Notifications #auto]
               }
            }
       }
   }
}


########################################################################
#
#  addObserver cmd object message
#
#  Use this method to register an object/procedure as an observer for
#  a particular object/message
#
########################################################################

itcl::body Notifications::addObserver { observer cmd object message } {

   set object [string trimleft $object :]
   if { $cmd=="-" } {
      set cmd $message
   }

   if { $remoteNotes != "" \
         && $object != "" && $object != "*" && [itcl::find object *::$object] != "" \
         && [$object isa DistantObject] && $message != "connectionHasClosed" } {

      #  Looking for a message not sent by a DO, so pass registration
      #  to remote Notification center.

      $remoteNotes addObserver $observer $cmd $object $message
      return
   }

   if { [info exists lookup($message,$object)] } {

      set currentObservers $lookup($message,$object)
      if { [locateObserver $observer $currentObservers]==-1 } {
         lappend currentObservers [list $observer $cmd]
      }

   } else {

      set currentObservers [list [list $observer $cmd]]

   }
   set lookup($message,$object) $currentObservers
}

########################################################################
#
#   removeObserver observer ?object? ?message?
#
#   Use this method to remove an object/procedure from receiving
#   certian notifications
#
########################################################################

itcl::body Notifications::removeObserver { observer {object ""} {message ""} } {

   set object [string trimleft $object :]

   if { $remoteNotes != "" \
         && $object != "" && $object != "*" && [itcl::find object *::$object] != "" \
         && [$object isa DistantObject] && $message != "connectionHasClosed" } {

      #  Looking for a message not sent by a DO, so pass registration
      #  to remote Notification center.

      $remoteNotes removeObserver $observer $object $message
      return
   }

   if { $object!="" && $message!="" } {

      if { ![info exists lookup($message,$object)] } return
      set currentObservers $lookup($message,$object)
      set idx [locateObserver $observer $currentObservers]
      if { $idx == -1 } return
      set lookup($message,$object) [lreplace $currentObservers $idx $idx]

   } else {

      foreach key [array names lookup] {
         foreach [list o m] [split $key ","] {}
         if { ($object=="" || $object==$o) && \
               ($message=="" || $message==$m) } {
            
            set currentObservers $lookup($key)
            set idx [locateObserver $observer $currentObservers]
            if { $idx != -1 } {
               set lookup($key) [lreplace $currentObservers $idx $idx]
            }
         }
      }
   }

}

########################################################################
#
#   postMessage object message
#
#   Use this method to send a message to all observers
#
########################################################################

itcl::body Notifications::postMessage { object message args } {

   set object [string trimleft $object :]

   ###########
   #
   #   Build list of ... 
   #
   set allObservers {}

   #
   #  ... observers of fully-resolved object-message, ...
   #

   if { [info exists lookup($message,$object)] } {
      eval lappend allObservers $lookup($message,$object)
   }

   #
   #  ... observers of all messages from this object, ...
   #

   if { [info exists lookup(*,$object)] } {
      eval lappend allObservers $lookup(*,$object)
   }

   #
   #  ... observers of this message from all objects
   #

   if { [info exists lookup($message,*)] } {
      eval lappend allObservers $lookup($message,*)
   }

   #
   #  Now send notifications to each observer
   #

   foreach observer $allObservers {
      foreach [list obs cmd] $observer {}
#puts "... ${cmd}ing \"$obs\" of $message"

         # send synchronously
         if { [catch {$obs $cmd $object $message $args} res] } {
#puts "*** Notify error: $obs $cmd $object $message"
#puts "$res"
         }
   }

   return

}


########################################################################
#
#   Private methods...
#

itcl::body Notifications::locateObserver { observer observerList } {
   set idx 0
   foreach oldObserver $observerList {
      if { $observer == [lindex $oldObserver 0] } {
         return $idx
      }
      incr idx
   }
   return -1
}


