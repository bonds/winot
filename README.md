# Intro

This is a script for OpenBSD that I use to stay connected to my wireless
networks at all times. It may be used interactively for adding new wifi
networks when a recognized network is not available, or it can be run as a
daemon. Winot was written in Ruby and it was tested on OpenBSD 5.7-stable.

# Installation

1. install Ruby
1. install these gems: logging, pry
1. git clone this repo
1. ````sudo ./winot````
1. customize winot-rcd to reflect your Ruby setup and your winot location...I
   use rbenv and store winot under $HOME/src/winot/
1. copy winot-rcd to /etc/rc.d/winot
1. ````sudo rcctl enable winot````
1. ````sudo rcctl start winot````

# Features

* browse wifi networks and enter password
* specify a hidden network to connect to
* run as a daemon
