
const St = imports.gi.St;
const Main = imports.ui.main;
const Mainloop = imports.mainloop;
const Tweener = imports.ui.tweener;
const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;
const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();

let text, button, event;
let nic = 'iwm0';

function _hideHello() {
    Main.uiGroup.remove_actor(text);
    text = null;
}

function _showHello() {
    if (!text) {
        text = new St.Label({ style_class: 'helloworld-label', text: _getSignalStrength()});
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

function _getSignalStrength() {
    let [res, out, err, status] = GLib.spawn_sync(null, ['/sbin/ifconfig', nic], null, GLib.SpawnFlags.DEFAULT, null);
    if (status == 0) {
        let outStr = String.fromCharCode.apply(String, out).trim();
        let match = /(\d{1,2})%/.exec(outStr)
        if (match != null) {
            return match[1]
        } else {
            log("could not parse ifconfig output")
            return 0
        }
    } else {
        log("trouble getting signal strength")
        return 0
    }
}

function _updateStrengthIcon() {
    let strength = _getSignalStrength();
    if (strength < 30) {
        let gicon = Gio.icon_new_for_string(Me.path + "/icons/32/wifi-low.png");
        let icon = new St.Icon({ gicon: gicon, icon_size: '32'});
        button.set_child(icon);
    } else if (strength < 60) {
        let gicon = Gio.icon_new_for_string(Me.path + "/icons/32/wifi-mid.png");
        let icon = new St.Icon({ gicon: gicon, icon_size: '32'});
        button.set_child(icon);
    } else {
        let gicon = Gio.icon_new_for_string(Me.path + "/icons/32/wifi-full.png");
        let icon = new St.Icon({ gicon: gicon, icon_size: '32'});
        button.set_child(icon);
    }
    return true;
}

function init() {
    button = new St.Bin({ style_class: 'panel-button',
                          reactive: true,
                          can_focus: true,
                          x_fill: true,
                          y_fill: false,
                          track_hover: true });
        let gicon = Gio.icon_new_for_string(Me.path + "/icons/32/wifi-full.png");
        let icon = new St.Icon({ gicon: gicon, icon_size: '32'});
        //let icon = new St.Icon({ icon_name: 'system-run-symbolic',
                             //style_class: 'system-status-icon' });

    button.set_child(icon);
    button.connect('button-press-event', _showHello);
}

function enable() {
    Main.panel._rightBox.insert_child_at_index(button, 0);
    event = GLib.timeout_add_seconds(0, 1, _updateStrengthIcon);
}

function disable() {
    Main.panel._rightBox.remove_child(button);
    Mainloop.source_remove(event);
}
