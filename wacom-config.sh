#!/bin/sh

for i in $(seq 10); do
    if xsetwacom list devices | grep -q Wacom; then
        break
    fi
    sleep 1
done

list=$(xsetwacom list devices)
stylus=$(echo "${list}" | awk '/stylus/{print $8}')
# configure the buttons on ${stylus} with your xsetwacom commands...
#xsetwacom set "${stylus}" Button 2 11
xsetwacom --set ${stylus} Button 2 "key +ctrl z -ctrl"
