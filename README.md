This is a script for OpenBSD that keeps you connected to your wireless
connections at all times.

# Features

* connects to wwan and reconnects as needed
* connects to wifi and reconnects as needed
* connects to vpn and reconnects as needed
* routes traffic according to vpn when its available, otherwise it routes
  traffic to the wwan
* does not route traffic across the wifi except to connect to vpn, in other
  words, assumes your wifi network is untrusted, i.e. you work at the airport or
  at cafes on occasion
* notices when your wifi signal is weak, scans for a stronger BSSID, and
  connects to it

# Installation

## Requirements

* OpenBSD 5.8 or later
* a wwan adapter
* a wifi adapter
* [wiconfig][1] is installed and configured with your wifi passwords
* an SSH server with tunneling enabled
* an SSH agent running with the credentials for accessing your SSH server loaded
* tun0 is setup to connect to your SSH server using tunnelling
* ppp0 is setup to connect to your wwan server

## Steps

1. git clone this repo
1. ````doas mv winot /usr/local/bin/````
1. ````doas mv winot.etc /etc/winot````
1. ````doas mv winot.rcd /etc/rc.d/winot````
1. ````doas chown root:wheel /etc/winot````
1. ````doas chmod 600 /etc/winot````
1. update your info in /etc/winot
1. ````doas winot````
1. ````doas rcctl enable winot````
1. ````doas rcctl start winot````

  [1]: https://github.com/devious/wiconfig
