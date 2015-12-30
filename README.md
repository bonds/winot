This is a script for OpenBSD that keeps you connected to your wireless
connections at all times.

# Features

* connects to wwan, wlan (aka wifi), and vpn and reconnects as needed
* routes traffic via vpn when its available, otherwise it routes traffic via the
  wwan
* does NOT route traffic across the wifi except to connect to vpn, in other
  words, assumes your wifi network is untrusted, i.e. you work at the airport or
  at cafes on occasion
* notices when your wifi signal is weak, scans for a stronger BSSID, and
  connects to it

# Installation

## Requirements

* OpenBSD 5.8 or later
* a wwan adapter with a properly configured pppd peer configured, or a wifi
  adapter plus [wiconfig][1] is installed and configured with your wifi passwords
* an SSH server with tunneling enabled
* an SSH agent running with the credentials for accessing your SSH server loaded

## Steps

1. ````git clone https://github.com/bonds/winot /tmp/winot````
1. ````doas sh /tmp/winot/install.sh````
1. edit /etc/winot and add your settings
1. ````doas rcctl start winot````

## Further Reading

* [how to setup a layer 3 VPN over SSH][2]

  [1]: https://github.com/devious/wiconfig
  [2]: http://www.kernel-panic.it/openbsd/vpn/vpn5.html

