#!/bin/sh

# functions that do stuff

function log {
    #echo $(date "+%Y%m%d%H%M%S") $@
    echo $(date) $@
}

function get_lock {

    if mkdir /tmp/winot-$1-lock > /dev/null 2>&1; then
        return 0
    else
        return 1
    fi

}

function release_lock {

    rmdir /tmp/winot-$1-lock > /dev/null 2>&1

}

function set_status_ok {

    touch /tmp/winot-$1-ok

}

function set_status_bad {

    rm /tmp/winot-$1-ok

}

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

function vpn_gateway {

    route -n show -inet | grep $vpn_server_public_ip | head -n 1 | awk '{ print $2 }' | tr -d '\n'

}

function wwan_gateway {

    route -n show -inet | grep $wwan_if | grep UHl | head -n 1 | awk '{ print $2 }' | tr -d '\n'

}

function check_vpn {

    (
    log fn:check_vpn
    name=vpn

    if get_lock $name; then
        if [ "$vpn_enabled" != 'yes' ]; then
            set_status_bad $name
        else
            counter=0
            ping_ok='no'
            while [ $counter -lt 3 ]; do
                if ping -q -c 1 -w 1 $vpn_server_private_ip > /dev/null 2>&1; then
                    ping_ok='yes'
                    counter=99
                fi
                counter=`expr $counter + 1`
            done

            if [ "$ping_ok" = 'yes' ]; then
                set_status_ok $name
            else
                set_status_bad $name
                log reconnecting to vpn
                ifconfig $vpn_if down
                ifconfig $vpn_if up
                pkill -5 -f "$vpn_command"
                SSH_AUTH_SOCK=$(ssh_auth_sock) $vpn_command &
                sleep $WAIT_X_SECONDS_FOR_VPN_TO_CONNECT
            fi
        fi
        release_lock $name
    fi
    ) &

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

    (
    log fn:check_routes
    name=routes
    try_wwan=no

    if get_lock $name; then
        if [ -f /tmp/winot-wlan-ok ]; then
            log "vpngw: $(vpn_gateway)"
            log "vpnip: $vpn_server_public_ip"
            log "wlangw: $(wlan_gateway)"
            if [[ $(vpn_gateway) != $(wlan_gateway) ]]; then
                route delete $vpn_server_public_ip $(wlan_gateway)
                route add $vpn_server_public_ip $(wlan_gateway)
            fi
            if [ -f /tmp/winot-vpn-ok ]; then
                if [[ $(default_route_ip) != $vpn_server_private_ip ]]; then
                    log updating default route to use vpn
                    for i in $(route -n show -inet | grep -o default); do route delete default; done
                    route add default $vpn_server_private_ip
                fi
            elif [ "$enable_wlan_without_vpn" = 'yes' ]; then
                if [[ $(default_route_ip) != $(wlan_gateway) ]]; then
                    log updating default route to use wlan without vpn protection \(insecure\)
                    for i in $(route -n show -inet | grep -o default); do route delete default; done
                    route add default $(wlan_gateway)
                fi
            else
                try_wwan=yes
            fi
        else
            route delete $vpn_server_public_ip
            try_wwan=yes
        fi

        if [ "$try_wwan" = 'yes' ]; then
            if [ -f /tmp/winot-wwan-ok ]; then
                if [[ $(default_route_ip) != $(wwan_gateway) ]]; then
                    log updating default route to use wwan
                    route -qn flush
                    for i in $(route -n show -inet | grep -o default); do route delete default; done
                    route add default $(wwan_gateway)
                fi
            else
                log no internet connection found, local network access only
            fi
        fi
        release_lock $name
    fi
    ) &
}

function check_wwan {

    (
    log fn:check_wwan
    name=wwan

    if get_lock $name; then
        if [ "$wwan_enabled" != 'yes' ]; then
            set_status_bad $name
        elif good_wwan_process && good_wwan_connection; then
            set_status_ok $name
        else
            set_status_bad $name
            /usr/sbin/pppd call $wwan_peer
            sleep $WAIT_X_SECONDS_FOR_WWAN_TO_CONNECT
        fi
        release_lock $name
    fi
    ) &
}


function check_wlan {

    (
    log fn:check_wlan
    name=wlan

    if get_lock $name; then
        if [ "$wlan_enabled" != 'yes' ]; then
            set_status_bad $name
        elif ifconfig $wlan_if | grep "status: active" > /dev/null 2>&1; then
            if ifconfig $wlan_if | grep "inet" > /dev/null 2>&1; then
                if ping -q -c 1 -w 1 $(wlan_gateway) > /dev/null 2>&1; then
                    set_status_ok $name
                    if ! check_wlan_signal; then
                        seconds_since_last_scan=$(dc -e "$(date +%s) $last_scan - n")
                        if [ $seconds_since_last_scan -gt $MINIMUM_SECONDS_BETWEEN_SCANS ]; then
                            log looking for a stronger wireless signal
                            last_scan=$(date +%s)
                            set_status_bad $name
                            ifconfig $wlan_if scan > /dev/null 2>&1
                        fi
                    fi
                fi
            else
                set_status_bad $name
                log 'getting IP for wireless network'
                dhclient $wlan_if > /dev/null 2>&1
            fi
        else
            set_status_bad $name
            log connecting to wireless network
            last_scan=$(date +%s) # treat new connection as a signal scan too
            /usr/local/bin/wiconfig -qs $wlan_if > /dev/null 2>&1
            sleep $WAIT_X_SECONDS_FOR_WLAN_TO_CONNECT
        fi
        release_lock $name
    fi
    ) &

}

function check_wlan_signal {
    # it appears that scanning leads OpenBSD to switch to the higher powered
    # BSSID if one is available with the same SSID, but the scanning process
    # interrupts and then renegotiates the current connection, regardless
    # whether a new BSSID with a stronger signal was found or whether we kept
    # the same BSSID, so only scan when the signal is consistently weak and the
    # connection is relatively idle

    log fn:check_wlan_signal

    signal_strength=$(cat /tmp/$wlan_if-signal.log | sort -rn | head -1 | tr -d '\n')
    bandwidth=$(cat /tmp/$wlan_if-bandwidth.log | sort -rn | head -1 | tr -d '\n')
    signal_strength_count=$(wc -l /tmp/$wlan_if-signal.log | awk '{print $1}' | tr -d '\n')
    bandwidth_count=$(wc -l /tmp/$wlan_if-bandwidth.log | awk '{print $1}' | tr -d '\n')

    #echo "ss: $signal_strength"
    #echo "bw: $bandwidth"
    #echo "ssc: $signal_strength_count"
    #echo "bc: $bandwidth_count"

    #echo "wsml: $WEAK_SIGNAL_MEANS_LESS_THAN"
    #echo "imlt: $IDLE_MEANS_LESS_THAN_X_BYTES"
    #echo "wsibw: $WEAK_SIGNAL_INTERVALS_BEFORE_WEAK"
    #echo "itbi: $IDLE_INTERVALS_BEFORE_IDLE"

    if [ -n "$signal_strength" ] &&
       [ -n "$bandwidth" ] &&
       [ -n "$signal_strength_count" ] &&
       [ -n "$bandwidth_count" ] &&
       [ $signal_strength -lt $WEAK_SIGNAL_MEANS_LESS_THAN ] &&
       [ $bandwidth -lt $IDLE_MEANS_LESS_THAN_X_BYTES ] &&
       [ $signal_strength_count -eq $WEAK_SIGNAL_INTERVALS_BEFORE_WEAK ] &&
       [ $bandwidth_count -eq $IDLE_INTERVALS_BEFORE_IDLE ]; then
        return 1 # the signal is weak
    else
        return 0 # the signal is strong or we don't have enough info
    fi

}

function log_wlan_stats {

    while true
    do
        systat -w 100 -B ifstat $IDLE_INTERVAL_IN_SECONDS | grep $wlan_if | awk '{print $7}' >> /tmp/$wlan_if-bandwidth.log
        ifconfig $wlan_if | grep bssid | sed -E "s/.*bssid.* (.*)%.*/\1/g" >> /tmp/$wlan_if-signal.log
        tail -n $IDLE_INTERVALS_BEFORE_IDLE /tmp/$wlan_if-bandwidth.log > /tmp/$wlan_if-bandwidth.log.new
        tail -n $WEAK_SIGNAL_INTERVALS_BEFORE_WEAK /tmp/$wlan_if-signal.log > /tmp/$wlan_if-signal.log.new
        mv /tmp/$wlan_if-bandwidth.log.new /tmp/$wlan_if-bandwidth.log
        mv /tmp/$wlan_if-signal.log.new /tmp/$wlan_if-signal.log
    done

}

function cleanup_stale_locks {
    # delete locks that are still there after we resume from sleep

    if [ -n "$last_loop" ]; then
        seconds_since_last_loop=$(dc -e "$(date +%s) $last_loop - n")
        if [ $seconds_since_last_loop -gt 30 ]; then
            log releasing stale locks
            release_lock wwan
            release_lock wlan
            release_lock vpn
            release_lock routes
        fi
    fi

}

function cleanup {

    log fn:cleanup

    # kill related processes

    pkill -5 -f "$vpn_command" > /dev/null 2>&1
    pkill -f pppd > /dev/null 2>&1

    # clear out the interface config

    sleep 1 # give pppd a chance to exit and free up the wwan_if
    ifconfig $vpn_if destroy > /dev/null 2>&1
    ifconfig $wwan_if destroy > /dev/null 2>&1
    ifconfig $wlan_if -nwid -wpakey -inet down > /dev/null 2>&1

    # reset statuses

    set_status_bad wwan
    set_status_bad wlan
    set_status_bad vpn

    # release locks

    release_lock wwan
    release_lock wlan
    release_lock vpn
    release_lock routes

    # delete the logs used for roaming

    rm /tmp/$wlan_if-signal.log > /dev/null 2>&1
    rm /tmp/$wlan_if-signal.log.new > /dev/null 2>&1
    rm /tmp/$wlan_if-bandwidth.log > /dev/null 2>&1
    rm /tmp/$wlan_if-bandwidth.log.new > /dev/null 2>&1

}

# main

if [[ `id -u` != 0 ]]; then
    echo "error: this script must be run by root"
    exit 1
fi

trap "cleanup; exit" INT TERM QUIT HUP

echo starting up

renice -n 20 $$ # run at low priority
. /etc/winot # load config
exec > /var/log/winot 2>&1 # log output to file

IDLE_MEANS_LESS_THAN_X_BYTES=1000
IDLE_INTERVAL_IN_SECONDS=1
IDLE_INTERVALS_BEFORE_IDLE=30
WEAK_SIGNAL_MEANS_LESS_THAN=20
WEAK_SIGNAL_INTERVALS_BEFORE_WEAK=30
MINIMUM_SECONDS_BETWEEN_SCANS=60
WAIT_X_SECONDS_FOR_WLAN_TO_CONNECT=10
WAIT_X_SECONDS_FOR_WWAN_TO_CONNECT=10
WAIT_X_SECONDS_FOR_VPN_TO_CONNECT=10

wlan_if=$(choose_wlan_adapter)
wwan_if=$(choose_wwan_adapter)
vpn_if=$(choose_vpn_adapter)
vpn_command="ssh -N -w $(echo -n $vpn_if | tail -c 1):any $vpn_server_public_ip"

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
    log start loop
    cleanup_stale_locks
    check_wwan
    check_wlan
    check_vpn
    check_routes
    last_loop=$this_loop
    this_loop=$(date +%s)
    sleep 1
done
