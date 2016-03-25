#!/bin/sh

if [[ `id -u` != 0 ]]; then
    echo "error: this script must be run by root"
    exit 1
fi

echo removing winot

rcctl stop winot
rcctl disable winot
rm /usr/local/bin/winot
rm /etc/rc.d/winot

echo done
echo you must manually delete /etc/winot if you want it removed
