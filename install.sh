#!/bin/sh

EXECPATH=$PWD/$(find .stack-work/install -name "winot-exe" | tail -1)

if [[ `id -u` != 0 ]]; then
    echo "error: this script must be run by root"
    exit 1
fi

if [ -f "$EXECPATH" ]; then
    echo installing winot exec from $EXECPATH
    cp -f "$EXECPATH" /usr/local/bin/winot
    chown root:bin /usr/local/bin/winot
    chmod 555 /usr/local/bin/winot

    if [ ! -f /etc/winot ]; then
        cp winot.etc /etc/winot
    fi
    chown root:wheel /etc/winot
    chmod 660 /etc/winot

    cp winot.rcd /etc/rc.d/winot
    chown root:wheel /etc/rc.d/winot
    chmod 555 /etc/rc.d/winot

    echo done
    echo to start winot: doas rcctl start winot
    echo to start winot at boot: doas rcctl enable winot
else
    echo "couldnt find an executable to install, did you 'stack build' yet?"
fi
