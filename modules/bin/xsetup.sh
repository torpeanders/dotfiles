#!/bin/bash

num_displays=$1

case "$num_displays" in
    1)
        REAL_DPI=96
        FULL_DPI=98304
    ;;
    *)
        XRANDR_TEMP=$(mktemp -t xrandr.XXXXXX); xrandr > $XRANDR_TEMP

        FULL_DPI=($(cat $XRANDR_TEMP | perl -ne 'if (/connected primary (\d+)x(\d+).* (\d+)mm x (\d+)mm/) { $dpi = sqrt($1**2+$2**2)*25.4/sqrt($3**2+$4**2); printf "%d %d", $dpi+.5, ($dpi*1024)+.5 }'))

        [ -z "$FULL_DPI" ] && FULL_DPI=($(cat $XRANDR_TEMP | perl -ne 'if (/connected (\d+)x(\d+).* (\d+)mm x (\d+)mm/) { $dpi = sqrt($1**2+$2**2)*25.4/sqrt($3**2+$4**2); printf "%d %d", $dpi+.5, ($dpi*1024)+.5; last }'))

        REAL_DPI=${FULL_DPI[0]}
        DPI=${FULL_DPI[1]}
        ;;
esac

file=$HOME/.xsettingsd


if ! grep -q Xft/DPI $file; then
    echo Xft/DPI $[96*1024] >> $file
fi

if ! grep -q Gdk/UnscaledDPI $file; then
    echo Gdk/UnscaledDPI $[96*1024] >> $file
fi

ed -s $file <<EOF
g,Gdk/UnscaledDPI,d
a
Gdk/UnscaledDPI $DPI
.
g,Xft/DPI,d
a
Xft/DPI $DPI
.
w
q
EOF

file2=$HOME/.Xresources

if ! grep -q "Xft.antialias" $file2; then
    echo "Xft.antialias:  true" >> $file2
fi

if ! grep -q "Xft.dpi" $file2; then
    echo "Xft.dpi: 96" >> $file2
fi

if ! grep -q "Xft.hinting" $file2; then
    echo "Xft.hinting: true" >> $file2
fi

if ! grep -q "Xft.hintstyle" $file2; then
    echo "Xft.hintstyle: true" >> $file2
fi

#if ! grep -q "Xft.rgba" $file2; then
#    echo "Xft.rgba: rgb" >> $file2
#fi

xrandr --dpi $REAL_DPI

ed -s $file2 <<EOF
g,Xft.dpi,d
a
Xft.dpi: $REAL_DPI
.
w
q
EOF

xrdb -merge $file2
xsettingsd &
