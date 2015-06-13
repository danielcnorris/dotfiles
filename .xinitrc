# Save power
xset +dpms
xset dpms 0 0 150
xbacklight = 20

# Inverted scroll
xmodmap -e "pointer = 1 2 3 5 4"

# Load xterm settings
xrdb -merge ~/.Xresources

# Start window manager
ratpoison
