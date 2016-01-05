#!/bin/sh

# functions that do stuff

function first_available_virtual_interface {

    prefix=$1

    ifconfig $prefix | grep flags | sed 's/$prefix\([0-9]\):.*/\1/g' > /tmp/winot-$prefix-used
    jot 10 0 > /tmp/winot-$prefix-numbers
    first_adapter=$(diff /tmp/winot-$prefix-used /tmp/winot-$prefix-numbers | grep '>' | cut -c 3- | sort -n | head -1)
    rm /tmp/winot-$prefix-used /tmp/winot-$prefix-numbers

    echo $prefix$first_adapter

}

function choose_wlan_adapter {

    if [ -n "$wlan_if" ]; then
        echo $wlan_if
    else
        first_wlan_adapter=$(ifconfig wlan 2>/dev/null | grep flags | sed 's/\(^.*\):.*/\1/g' | head -n 1)
        if [ -n "$first_wlan_adapter" ]; then
            echo $first_wlan_adapter
        else
            return
        fi
    fi

}

function choose_wwan_adapter {

    if [ -n "$wwan_if" ]; then
        echo $wwan_if
    else
        first_available=$(first_available_virtual_interface ppp)
        if [ -n "$first_available" ]; then
            echo $first_available
        else
            return
        fi
     fi

}

function choose_vpn_adapter {

    if [ -n "$vpn_if" ]; then
        echo $vpn_if
    else
        first_available=$(first_available_virtual_interface tun)
        if [ -n "$first_available" ]; then
            echo $first_available
        else
            return
        fi
    fi

}


function default_route_ip {

    route -n show -inet | grep default | head -n 1 | awk '{ print $2 }' | tr -d '\n'

}

function wwan_gateway {

    route -n show -inet | grep $wwan_if | grep UHl | head -n 1 | awk '{ print $2 }' | tr -d '\n'

}

function check_vpn {

    echo fn:check_vpn

    if [ "$vpn_enabled" != 'yes' ]; then
        return 1
    fi

    counter=0
    while [ $counter -lt 3 ]; do
        if ping -q -c 1 -w 1 $vpn_server_private_ip > /dev/null 2>&1; then return 0; fi
        counter=`expr $counter + 1`
    done

    echo reconnecting to vpn
    rmdir /tmp/vpn.lock > /dev/null 2>&1
    (connect_to_vpn) &
    return 1

}

function connect_to_vpn {

    if mkdir /tmp/vpn.lock > /dev/null 2>&1; then
        route delete $vpn_server_public_ip
        route add $vpn_server_public_ip $(wlan_gateway)
        ifconfig $vpn_if down
        ifconfig $vpn_if up
        pkill -5 -f "$vpn_command"
        SSH_AUTH_SOCK=$(ssh_auth_sock) $vpn_command &
    fi

}

function ssh_auth_sock {

    grep SSH_AUTH_SOCK $ssh_auth_sock_file | sed 's/.*SSH_AUTH_SOCK \(.*\);/\1/g' | tr -d '\n'

}

function wlan_gateway {

    grep routers /var/db/dhclient.leases.$wlan_if | tail -n 1 | sed 's/.* \(.*\);/\1/g' | tr -d '\n'

}

function good_wwan_connection {

    ifconfig $wwan_if | grep inet > /dev/null 2>&1

}

function good_wwan_process {

    pgrep pppd > /dev/null 2>&1

}

function check_routes {

    echo fn:check_routes

    try_wwan=no

    if check_wlan; then
        if check_vpn; then
            echo good vpn
            if [[ $(default_route_ip) != $vpn_server_private_ip ]]; then
                echo updating default route to use vpn
                for i in $(route -n show -inet | grep -o default); do route delete default; done
                route add default $vpn_server_private_ip
            fi
        elif [ "$enable_wlan_without_vpn" = 'yes' ]; then
            if [[ $(default_route_ip) != $(wlan_gateway) ]]; then
                echo updating default route to use wlan without vpn protection \(insecure\)
                for i in $(route -n show -inet | grep -o default); do route delete default; done
                route add default $wlan_gateway
            fi
        else
            try_wwan=yes
        fi
    else
        try_wwan=yes
    fi
    if [ "$try_wwan" = 'yes' ]; then
        if good_wwan_process && good_wwan_connection; then
            echo good wwan
            if [[ $(default_route_ip) != $(wwan_gateway) ]]; then
                echo updating default route to use wwan
                route -qn flush
                for i in $(route -n show -inet | grep -o default); do route delete default; done
                route add default $(wwan_gateway)
            fi
        else
            echo no internet connection found, local network access only
            return 1
        fi
    fi
}

function check_wwan {

    echo fn:check_wwan

    if [ "$wwan_enabled" != 'yes' ]; then
        return 1
    fi

    if ! pgrep pppd > /dev/null 2>&1; then
        echo restarting pppd
        /usr/sbin/pppd call $wwan_peer
    fi

}

function check_wlan {

    echo fn:check_wlan

    if [ "$wlan_enabled" != 'yes' ]; then
        return 1
    fi

    if ifconfig $wlan_if | grep "status: active" > /dev/null 2>&1; then
        if ifconfig $wlan_if | grep "inet" > /dev/null 2>&1; then
            if ping -q -c 1 -w 1 $(wlan_gateway) > /dev/null 2>&1; then
                check_wlan_signal
                return 0
            fi
        else
            if mkdir /tmp/dhclient.lock > /dev/null 2>&1; then
                echo getting IP for wireless network
                (dhclient $wlan_if > /dev/null 2>&1; rmdir /tmp/dhclient.lock) &
            fi
            return 1
        fi
    else
        if mkdir /tmp/wiconfig.lock > /dev/null 2>&1; then
            echo connecting to wireless network
            (/usr/local/bin/wiconfig -qs $wlan_if > /dev/null 2>&1; rmdir /tmp/wiconfig.lock) &
        fi
        return 1
    fi

}

function check_wlan_signal {
    # it appears that scanning leads OpenBSD to switch to the higher powered
    # BSSID if one is available with the same SSID, but the scanning process
    # interrupts and then renegotiates the current connection, regardless
    # whether a new BSSID with a stronger signal was found or whether we kept
    # the same BSSID, so only scan when the signal is consistently weak and the
    # connection is relatively idle

    echo fn:check_wlan_signal

    signal_strength=$(cat /tmp/$wlan_if-signal.log | sort -rn | head -1 | tr -d '\n')
    bandwidth=$(cat /tmp/$wlan_if-bandwidth.log | sort -rn | head -1 | tr -d '\n')
    signal_strength_count=$(wc -l /tmp/$wlan_if-signal.log | awk '{print $1}' | tr -d '\n')
    bandwidth_count=$(wc -l /tmp/$wlan_if-bandwidth.log | awk '{print $1}' | tr -d '\n')
    lock=/tmp/signal.lock

    if [ $signal_strength -lt $weak_signal_means_less_than ] &&
       [ $bandwidth -lt $idle_means_less_than_x_bytes ] &&
       [ $signal_strength_count -eq $weak_signal_intervals_before_weak ] &&
       [ $bandwidth_count -eq $idle_intervals_before_idle ]; then
        if mkdir $lock > /dev/null 2>&1; then
            echo looking for a stronger wireless signal
            (ifconfig $wlan_if scan > /dev/null 2>&1; sleep $minimum_seconds_between_scans; rmdir $lock) &
        fi
    fi

}

function log_wlan_stats {

    while true
    do
        systat -w 100 -B ifstat $idle_interval_in_seconds | grep $wlan_if | awk '{print $7}' >> /tmp/$wlan_if-bandwidth.log
        ifconfig $wlan_if | grep bssid | sed -E "s/.*bssid.* (.*)%.*/\1/g" >> /tmp/$wlan_if-signal.log
        tail -n $idle_intervals_before_idle /tmp/$wlan_if-bandwidth.log > /tmp/$wlan_if-bandwidth.log.new
        tail -n $weak_signal_intervals_before_weak /tmp/$wlan_if-signal.log > /tmp/$wlan_if-signal.log.new
        mv /tmp/$wlan_if-bandwidth.log.new /tmp/$wlan_if-bandwidth.log
        mv /tmp/$wlan_if-signal.log.new /tmp/$wlan_if-signal.log
    done

}

function cleanup {

    echo fn:cleanup

    # kill related processes

    pkill -5 -f "$vpn_command"
    pkill -f pppd

    # clear out the interface config

    sleep 1 # give pppd a chance to exit and free up the wwan_if
    ifconfig $vpn_if destroy
    ifconfig $wwan_if destroy
    ifconfig $wlan_if -nwid -wpakey -inet down

    # delete directories used as locks

    rmdir /tmp/dhclient.lock > /dev/null 2>&1
    rmdir /tmp/wiconfig.lock > /dev/null 2>&1
    rmdir /tmp/vpn.lock > /dev/null 2>&1
    rmdir /tmp/signal.lock > /dev/null 2>&1

    # delete the logs used for roaming

    rm /tmp/$wlan_if-signal.log
    rm /tmp/$wlan_if-signal.log.new
    rm /tmp/$wlan_if-bandwidth.log
    rm /tmp/$wlan_if-bandwidth.log.new

}

# main

if [[ `id -u` != 0 ]]; then
    echo "error: this script must be run by root"
    exit 1
fi

trap "cleanup; exit" INT TERM QUIT HUP

echo starting up

. /etc/winot # load config
wlan_if=$(choose_wlan_adapter)
wwan_if=$(choose_wwan_adapter)
vpn_if=$(choose_vpn_adapter)
vpn_command="ssh -N -w $(echo -n $vpn_if | tail -c 1):any $vpn_server_public_ip"

idle_means_less_than_x_bytes=1000
idle_interval_in_seconds=1
idle_intervals_before_idle=30
weak_signal_means_less_than=20
weak_signal_intervals_before_weak=30
minimum_seconds_between_scans=60

cleanup

if [ -n "$wlan_if" ]; then
    echo wlan enabled
    wlan_enabled='yes'
    log_wlan_stats &
else
    echo wlan disabled
fi
if [ -n "$wwan_if" ] &&
   [ -n "$wwan_peer" ] ; then
    echo wwan enabled
    wwan_enabled='yes'
    ifconfig $wwan_if create up
else
    echo wwan disabled
fi
if [ -n "$vpn_if" ] &&
   [ -n "$vpn_server_public_ip" ] &&
   [ -n "$vpn_client_private_ip" ] &&
   [ -n "$vpn_server_private_ip" ] &&
   [ -n "$vpn_private_netmask" ] &&
   [ -n "$ssh_auth_sock_file" ]; then
    echo vpn enabled
    vpn_enabled='yes'
    ifconfig $vpn_if create $vpn_client_private_ip $vpn_server_private_ip netmask $vpn_private_netmask up
else
    echo vpn disabled
fi

#echo first ppp: $(first_available_virtual_interface ppp)
#echo first tun: $(first_available_virtual_interface tun)
#echo vpn_if: $vpn_if
#echo wwan_if: $wwan_if

echo monitoring network connections

while true
do
    echo start loop
    check_wwan
    check_routes
    sleep 1
done
