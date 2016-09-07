export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon --xim &
setxkbmap -option ctrl:nocaps
xbindkeys
redshift -t 5500:2000 & > /dev/null &
remind -z -k'gxmessage %s &' /home/dcn/dcn/remind &
while true; do
  xsetroot -name "$( acpi -b | grep -Eo "[0-9]+%") $( date +"%F %a %R" )"
  sleep 5
done &
exec dwm
