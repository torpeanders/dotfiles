#!/bin/bash

XRANDR_TEMP=$(mktemp -t xrandr.XXXXXX); xrandr > $XRANDR_TEMP

FULL_DPI=($(cat $XRANDR_TEMP | perl -ne 'if (/connected primary (\d+)x(\d+).* (\d+)mm x (\d+)mm/) { $dpi = sqrt($1**2+$2**2)*25.4/sqrt($3**2+$4**2); printf "%d %d", $dpi+.5, ($dpi*1024)+.5 }'))

[ -z "$FULL_DPI" ] && FULL_DPI=($(cat $XRANDR_TEMP | perl -ne 'if (/connected (\d+)x(\d+).* (\d+)mm x (\d+)mm/) { $dpi = sqrt($1**2+$2**2)*25.4/sqrt($3**2+$4**2); printf "%d %d", $dpi+.5, ($dpi*1024)+.5; last }'))

REAL_DPI=${FULL_DPI[0]}
DPI=${FULL_DPI[1]}

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
