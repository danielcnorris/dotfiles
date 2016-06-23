setxkbmap -option ctrl:nocaps
xbindkeys
# Statusbar loop
while true; do
   xsetroot -name "$( acpi -b | grep -Eo "[0-9]+%") $( date +"%F %R" )"
      sleep 1m    # Update time every minute
      done &
exec dwm
