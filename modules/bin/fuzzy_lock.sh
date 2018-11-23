#! /bin/bash

# bindsym $mod+Control+l exec --no-startup-id "sensible-locker lock"
# exec --no-startup-id "sensible-locker init"

have() {
    local nwant=$# nhave=0 prog=
    for prog in "$@"; do
	      type -p "$prog" >& /dev/null && nhave=$[nhave+1]
    done
    [ $nwant = $nhave ]
}

have_one () {
    for prog in "$@"; do
	      if type -p "$prog" >& /dev/null; then
	          return 0;
	      fi
    done
    prog=false
    return 1
}

cmd=$1

idle_timeout=600
lock_grace=60

case $cmd in
    init)
	      if have xss-lock i3lock; then
	          # xss-lock first dims then locks after cycle
	          xset s $[idle_timeout-lock_grace] $lock_grace
	          # p
	          xset dpms $[idle_timeout*2]
	          xss-lock -n "$0 dim" -- $0 lock-cmd &
	      fi
	      ;;

    lock)
	      loginctl lock-session
	      ;;

    lock-cmd)
	      if have i3lock; then
            # Use pixelated screenshot as lock background
            convert X:root -scale 10% -scale 1000% /tmp/screen_locked.png
            i3lock -n -i /tmp/screen_locked.png
	      fi
	      ;;

    dim)
	      if have_one ybacklight xbacklight; then
	          trap 'exit 0' TERM INT
	          # v1.2.1 and lower lowers backlight with up to 1% per invocation
	          # due to integer input only
	          trap "$prog -set $($prog -get); kill %% >& /dev/null" EXIT
	          time=$[3*1000]
	          $prog -time $time -steps $[time/10] -set 1
	          while true; do
		            sleep 3600 &
		            wait
	          done
	      fi
	      ;;
esac
