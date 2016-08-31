setxkbmap -option ctrl:nocaps
xbindkeys
redshift -t 5500:2000 & > /dev/null &
remind -z '-kgxmessage %s &' /home/dcn/dcn/remind &
# Statusbar loop
while true; do
  xsetroot -name "$( acpi -b | grep -Eo "[0-9]+%") $( date +"%F %a %R" )"
  sleep 1m    # Update time every minute
done &
exec dwm
