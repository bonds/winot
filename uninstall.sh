#!/bin/sh

GEUUID=winot@scott.ggr.com

if [[ `id -u` != 0 ]]; then
    echo "error: this script must be run by root"
    exit 1
fi

echo removing winot

rcctl stop winot
rcctl disable winot
rm /usr/local/bin/winot
rm /etc/rc.d/winot
rm -r /usr/local/share/gnome-shell/extensions/$GEUUID

echo done
echo "you must manually delete /etc/winot (the config file) if you want it removed"
