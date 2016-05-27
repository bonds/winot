
const St = imports.gi.St;
const Main = imports.ui.main;
const Mainloop = imports.mainloop;
const Tweener = imports.ui.tweener;
const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;
const ExtensionUtils = imports.misc.extensionUtils;
const Me = ExtensionUtils.getCurrentExtension();

let text, button, event, status;
let nic = 'iwm0';
let lastStrengthBoundary = 100;
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

function _updateStatus() {
    // not reliable, there's a race condition with writing and reading file
    // that's why we're using the try-catch block
    // TODO: use a more reliable inter-process messaging bus
    try {
        let [res, out] = GLib.file_get_contents('/var/winot/status');
        if (res) {
            status = JSON.parse(out);
        } else {
            throw 'could not read status file';
        }
    }
    catch(err) {
        status = null;
    }
    _updateIcon();
    return true;
}

// TODO: if data is more than 5 seconds stale, use 'None' icon
function _updateIcon() {
    let gicon, icon, iconName;
    if (status != null) {
        switch (status.csUsing) {
            case 'None':
                iconName = 'spam';
                break;
            case 'WWAN':
                iconName = 'transfer';
                break;
            case 'WLAN':
                iconName = _choose_wlan_icon(status.csWlanStrength);
                break;
            case 'VPN':
                iconName = _choose_wlan_icon(status.csWlanStrength);
                break;
        }
    } else {
        iconName = 'spam';
    }
    gicon = Gio.icon_new_for_string(Me.path + "/icons/32/" + iconName + ".png");
    icon = new St.Icon({ gicon: gicon, icon_size: '32'});
    button.set_child(icon);
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
    event = GLib.timeout_add_seconds(0, secondsBetweenSignalChecks, _updateStatus);
}

function disable() {
    Main.panel._rightBox.remove_child(button);
    Mainloop.source_remove(event);
}
