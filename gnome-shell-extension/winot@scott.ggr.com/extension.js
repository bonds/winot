// -*- mode: js2; indent-tabs-mode: nil; js2-basic-offset: 4 -*-

const Gio = imports.gi.Gio;
const GLib = imports.gi.GLib;
const Meta = imports.gi.Meta;
const Clutter = imports.gi.Clutter;
const St = imports.gi.St;
const Lang = imports.lang;
const Mainloop = imports.mainloop;
const PanelMenu = imports.ui.panelMenu;
const PopupMenu = imports.ui.popupMenu;
const Panel = imports.ui.panel;

const Gettext = imports.gettext.domain('gnome-shell-extensions');
const _ = Gettext.gettext;

const Main = imports.ui.main;

const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();



let text, _indicator, event, status;
let lastStrengthBoundary = 100;
let lastUsing = 'None';
let lastStatus = 0;
let secondsBetweenSignalChecks = 2;

function _hideHello() {
    Main.uiGroup.remove_actor(text);
    text = null;
}

function _showHello() {
    if (!text) {
        text = new St.Label({ style_class: 'helloworld-label', text: 'hello, world' });
        Main.uiGroup.add_actor(text);
    }

    text.opacity = 255;

    let monitor = Main.layoutManager.primaryMonitor;

    text.set_position(monitor.x + Math.floor(monitor.width / 2 - text.width / 2),
                      monitor.y + Math.floor(monitor.height / 2 - text.height / 2));

    Tweener.addTween(text,
                     { opacity: 0,
                       time: 2,
                       transition: 'easeOutQuad',
                       onComplete: _hideHello });
}

function _choose_wlan_icon(strength) {
    let iconSuffix;

    // avoid flipping back and forth when on a strength boundary
    // e.g. 59, 60, 58, 61 will stay with the 'mid' icon the whole time
    let strengthDiff = Math.abs(strength - lastStrengthBoundary);
    //log("strength: " + strength);
    if (strengthDiff > 5) {
        if (strength < 30) {
            iconSuffix = 'low';
            lastStrengthBoundary = 30;
        } else if (strength < 60) {
            iconSuffix = 'mid';
            lastStrengthBoundary = 60;
        } else {
            iconSuffix = 'full';
            lastStrengthBoundary = 100;
        }
    } else {
        switch (lastStrengthBoundary) {
            case 30:
                iconSuffix = 'low';
                break;
            case 60:
                iconSuffix = 'mid';
                break;
            case 100:
                iconSuffix = 'full';
                break;
        }
    }
    return 'wifi-' + iconSuffix;
}

const NetworkIndicator = new Lang.Class({
    Name: 'NetworkIndicator.NetworkIndicator',
    Extends: PanelMenu.Button,

    _init: function(){
        this.parent(0.0, _("Network Indicator"));
        _indicator = this;

        let gicon = Gio.icon_new_for_string(Me.path + "/icons/32/spam.png");
        this.icon = new St.Icon({ gicon: gicon, icon_size: '32' });
        this.actor.add_child(this.icon);

        this.workspacesItems = [];
        this._workspaceSection = new PopupMenu.PopupMenuSection();
        this.menu.addMenuItem(this._workspaceSection);
        this._updateStatus();
        this._createWorkspacesSection();
        this._updateIcon();

    },

    destroy: function() {
        for (let i = 0; i < this._screenSignals.length; i++)
            global.screen.disconnect(this._screenSignals[i]);

        if (this._settingsChangedId) {
            this._settings.disconnect(this._settingsChangedId);
            this._settingsChangedId = 0;
        }

        this.parent();
    },

    _updateIndicator: function() {
        log('here2');
    },

    _createWorkspacesSection : function() {
        _indicator._workspaceSection.removeAll();
        log('here1' + _indicator.status);
        if (_indicator.status != null) {
            log('here4' + _indicator.status);
            let networks = [];
            _indicator.workspacesItems = [];
            _indicator._currentWorkspace = global.screen.get_active_workspace().index();

            let i,j,k;
            for(i=0; i < _indicator.status.csNetworks.length; i++) {
                let ssid = _indicator.status.csNetworks[i].csSsid 
                if (ssid == '') {
                    continue;
                }
                let bssids = _indicator.status.csNetworks[i].csBssids
                let strength = 0;
                for(j=0; j < bssids.length; j++) {
                    let bssid = bssids[j];
                    if (bssid.csStrength > strength) {
                        strength = bssid.csStrength;
                    }
                }
                networks.push({'ssid': ssid, 'strength': strength});
            }
            networks = networks.sort(function(a,b) {
              return parseFloat(b.strength) - parseFloat(a.strength);
            });
            for(i=0; i < networks.length; i++) {
                let network = networks[i];
                _indicator.workspacesItems[i] = new PopupMenu.PopupMenuItem(network.strength + ' ' + network.ssid);
                _indicator._workspaceSection.addMenuItem(_indicator.workspacesItems[i]);
                _indicator.workspacesItems[i].workspaceId = i;
                let self = _indicator;
                _indicator.workspacesItems[i].connect('activate', Lang.bind(_indicator, function(actor, event) {
                    _indicator._activate(actor.workspaceId);
                }));
            }
            let now = Math.floor(GLib.get_real_time()/1000000);
            let since = now - _indicator.status.csScanned; 

            _indicator.workspacesItems[networks.length] = new PopupMenu.PopupMenuItem('');
            _indicator._workspaceSection.addMenuItem(_indicator.workspacesItems[networks.length]);
            _indicator.workspacesItems[networks.length+1] = new PopupMenu.PopupMenuItem('updated ' + since + ' seconds ago');
            _indicator._workspaceSection.addMenuItem(_indicator.workspacesItems[networks.length+1]);
        }
    },

    _activate : function (index) {
        if(index >= 0 && index <  global.screen.n_workspaces) {
            let metaWorkspace = global.screen.get_workspace_by_index(index);
            metaWorkspace.activate(global.get_current_time());
        }
    },

    _updateStatus : function (retry) {
        // not reliable, there's a race condition with writing and reading file
        // that's why we're using the try-catch block and the retries
        // TODO: use a more reliable inter-process messaging bus
        let ok = false;
        let retryAfterXMilliseconds = 250;

        if (retry == null) {
            retry == 4
        }
        try {
            let [res1, timestamp,,] = GLib.spawn_command_line_sync('stat -f "%a" /var/winot/status');
            let [res2, statusRaw] = GLib.file_get_contents('/var/winot/status');
            if (res1 && res2) {
                // timestamp isn't a string until I concat it to a string
                // which I do so I can use trim()
                _indicator.lastStatus = (timestamp + "").trim();
                _indicator.status = JSON.parse(statusRaw);
                ok = true;
            }
        }
        catch(err) {}

        if (ok) {
            log('status updated');
            _indicator._updateIcon();
            _indicator._createWorkspacesSection();
        } else {
            if (retry > 0) {
                setTimeOut(_updateStatus(retry-1, retryAfterXMilliseconds))
            } else {
                this.status = null;
            }
        }
        return true;
    },

    _updateIcon : function() {
        let gicon, icon, iconName, message;
        let now = Math.floor(GLib.get_real_time()/1000000);
        if ((now - _indicator.lastStatus) > 5) {
            // if the status file is stale, assume the worst
            iconName = 'spam';
        } else if (_indicator.status != null) {
            switch (_indicator.status.csUsing) {
                case 'None':
                    iconName = 'spam';
                    break;
                case 'WWAN':
                    iconName = 'transfer';
                    break;
                case 'WLAN':
                    iconName = _choose_wlan_icon(_indicator.status.csWlanStrength);
                    break;
                case 'VPN':
                    iconName = _choose_wlan_icon(_indicator.status.csWlanStrength);
                    break;
            }
            if (_indicator.status.csUsing != lastUsing) {
                message = "connected via " + _indicator.status.csUsing;
                GLib.spawn_command_line_async("notify-send --app-name=winot --icon=" + Me.path + "/icons/32/wifi-full.png '" + message + "'");
                lastUsing = _indicator.status.csUsing;
            }
        } else {
            iconName = 'spam';
        }
        gicon = Gio.icon_new_for_string(Me.path + "/icons/32/" + iconName + ".png");
        icon = new St.Icon({ gicon: gicon, icon_size: '32'});
        _indicator.actor.remove_all_children();
        _indicator.actor.add_child(icon);
    },

});

function enable() {
    _indicator = new NetworkIndicator;
    Main.panel.addToStatusArea('network-indicator', _indicator);
    event = GLib.timeout_add_seconds(0, secondsBetweenSignalChecks, _indicator._updateStatus);
}

function disable() {
    _indicator.destroy();
    Mainloop.source_remove(event);
}

