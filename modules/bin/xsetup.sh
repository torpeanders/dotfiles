#!/bin/bash

NUM_DISPLAYS=$(xrandr -q | grep ' connected' | wc -l)

case "$NUM_DISPLAYS" in
    2)
        REAL_DPI=140
        ;;
    *)
        REAL_DPI=140
        ;;
esac

file=$HOME/.Xresources

if ! grep -q "Xft.antialias" $file; then
    echo "Xft.antialias:  true" >> $file
fi

if ! grep -q "Xft.dpi" $file; then
    echo "Xft.dpi: 96" >> $file
fi

if ! grep -q "Xft.hinting" $file; then
    echo "Xft.hinting: true" >> $file
fi

if ! grep -q "Xft.hintstyle" $file; then
    echo "Xft.hintstyle: true" >> $file
fi

#if ! grep -q "Xft.rgba" $file; then
#    echo "Xft.rgba: rgb" >> $file
#fi

xrandr --dpi $REAL_DPI

ed -s $file <<EOF
g,Xft.dpi,d
a
Xft.dpi: $REAL_DPI
.
w
q
EOF

xrdb -merge $file
