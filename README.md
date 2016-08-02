[![Build Status](https://travis-ci.org/bonds/winot.svg?branch=master)](https://travis-ci.org/bonds/winot)
[![Coverage Status](https://coveralls.io/repos/github/bonds/winot/badge.svg?branch=master)](https://coveralls.io/github/bonds/winot?branch=master)

This is a service for OpenBSD that keeps you connected to your wireless
connections at all times.

# Features

* connects to wwan, wlan (aka wifi), and vpn and reconnects as needed
* routes traffic via vpn when its available, otherwise it routes traffic via the
  wwan
* does NOT route traffic across the wifi except to connect to vpn, in other
  words, assumes your wifi network is untrusted, i.e. you work at the airport or
  at cafes on occasion
* you can turn off the vpn requirement for wlan connections if you're feeling
  brave and/or safe and/or you can live without it
* notices when your wifi signal is weak, scans for a stronger BSSID, and
  connects to it
* status bar icon that shows your connection status (requires Gnome)

![Imgur](http://i.imgur.com/4RdqQ00.gif)

# Installation

## Requirements

* OpenBSD 5.8 or later
* [Stack][3]
* icu4c package
* a wwan adapter with a properly configured pppd peer configured, or a wifi
  adapter and a wifi network to connect to
* your wifi intreface is configured through DHCP, i.e. the IP and other info is
  not statically configured in a hostname.if file

## Optional

* an SSH server with tunneling enabled
* an SSH agent running with the credentials for accessing your SSH server loaded
* Gnome, to get the status bar icon

## Let's Install It Already

1. ````git clone https://github.com/bonds/winot /tmp/winot````
1. ````cd /tmp/winot````
1. ````stack build````
1. ````doas sh /tmp/winot/install.sh````
1. edit /etc/winot and add your settings
1. ````doas rcctl start winot````

# Further Reading

* [how to setup a layer 3 VPN over SSH][2]

## Alternatives

* [wiconfig][1]
* [wifind][8]
* [wireless][9]
* [ifstated][4], a service for monitoring the network and running scripts when something changes
* manually configure wwan using [pppd][5], wlan using [ifconfig][6], vpn [using ssh][2] or some other tool of your choice, and [route][7] for changing your default route

  [1]: https://github.com/devious/wiconfig
  [2]: http://www.kernel-panic.it/openbsd/vpn/vpn5.html
  [3]: http://docs.haskellstack.org/en/stable/README/
  [4]: http://man.openbsd.org/OpenBSD-current/man8/ifstated.8
  [5]: http://man.openbsd.org/OpenBSD-current/man8/pppd.8
  [6]: http://man.openbsd.org/OpenBSD-current/man8/ifconfig.8
  [7]: http://man.openbsd.org/OpenBSD-current/man8/route.8
  [8]: http://marc.info/?l=openbsd-misc&m=146488514620893&w=2
  [9]: https://github.com/farhaven/wireless
