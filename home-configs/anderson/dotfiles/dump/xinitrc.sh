#!/bin/sh

if test -z "$DBUS_SESSION_BUS_ADDRESS"
then eval $(dbus-launch --exit-with-session --sh-syntax)
fi

systemctl --user import-environment DISPLAY XAUTHORITY

if command -v dbus-update-activation-environment > /dev/null 2>&1
then dbus-update-activation-environment DISPLAY XAUTHORITY
fi

source ~/.choose_wm || { echo "I need a ~/.choose_wm to run." ; exit 1; }
