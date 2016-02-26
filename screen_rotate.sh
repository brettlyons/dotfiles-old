# File: /usr/local/bin/screen_rotation.sh

#!/bin/sh

# Find the line in "xrandr -q --verbose" output that contains current screen orientation and "strip" out current orientation.

rotation="$(xrandr -q --verbose | grep 'connected' | egrep -o  '\) (normal|left|inverted|right) \(' | egrep -o '(normal|left|inverted|right)')"

# set the pressure curve to my custom preference
xsetwacom --set "Wacom Serial Penabled 1FG Touchscreen stylus" PressureCurve 0 20 100 100
# Using current screen orientation proceed to rotate screen and input tools.

case "$rotation" in
    normal)
    # rotate to the left
    xrandr -o left
    xsetwacom set "Wacom Serial Penabled 1FG Touchscreen stylus" rotate ccw
    xsetwacom set "Wacom Serial Penabled 1FG Touchscreen eraser" rotate ccw
    ;;
    left)
    # rotate to normal 
    xrandr -o normal
    xsetwacom set "Wacom Serial Penabled 1FG Touchscreen stylus" rotate none
    xsetwacom set "Wacom Serial Penabled 1FG Touchscreen eraser" rotate none 
    ;;
    inverted)
    #rotate to to the left
    xrandr -o right
    xsetwacom set "Serial Wacom Tablet stylus" rotate cw
    xsetwacom set "Serial Wacom Tablet eraser" rotate cw
    ;;
    right)
    #rotate to the left
    xrandr -o normal
    xsetwacom set "Serial Wacom Tablet stylus" rotate none
    xsetwacom set "Serial Wacom Tablet eraser" rotate none
    ;;
esac
