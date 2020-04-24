#          __  _ __                         _____      
#   ____ _/ /_(_) /__     _________  ____  / __(_)___ _
#  / __ `/ __/ / / _ \   / ___/ __ \/ __ \/ /_/ / __ `/
# / /_/ / /_/ / /  __/  / /__/ /_/ / / / / __/ / /_/ / 
# \__, /\__/_/_/\___/   \___/\____/_/ /_/_/ /_/\__, /     github@t0maslb
#   /_/                                       /____/   

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget
from typing import List  # noqa: F401

#### VARIABLES ####
mod = "mod4"

## APP VARIABLES ##
term = "kitty"
browser = "chromium"
app_launcher = "dmenu_run -p '[Run]'"
menu = "dmenu"
gui_editor = "code"
term_editor = "nvim"
# gui_calendar =
tui_calendar = "calcurses"

## LAYOUT VARIABLES ##
bwidth=3
bcnormal='#282a36'
bcfocused='#ff5c57'
margin=25
mono_font='Roboto Mono for Powerline'

## BAR COLORS ##
bar_colors = [
    "#282a36",      # black
    "#ff5c57",      # red
    "#5af78e",      # green
    "#f3f99d",      # yellow
    "#57c7ff",      # blue
    "#ff6ac1",      # magenta
    "#9aedfe",      # cyan
    "#f1f1f0"       # white
]

## BAR DEFAULTS ##
widget_defaults = dict(
    font='Roboto Mono for Powerline',
    # font='droid sans',
    # font='ubuntu',
    fontsize=13,
    padding=3,
)

keys = [
    #### ORGANISING_WINDOWS ####
    # Switch between windows in current stack pane
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    # Move windows up or down in current stack or reorganize them in general
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    # Resize windows
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    # MONAD
    Key([mod, "shift"], "i", lazy.layout.maximize()),
    Key([mod, "shift"], "m", lazy.layout.normalize()),
    # BSP
    Key([mod], "s", lazy.layout.toggle_split()),

    Key([mod], "q", lazy.window.kill()),
    Key([mod], "r", lazy.layout.next()),


    #### LAYOUTS ####
    # Swap panes of split stack
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "space", lazy.layout.rotate()),
    Key([mod, "shift"], "Return", lazy.layout.toggle_split()),

    #### LAUNCHING ####
    # APPS
    # gui
    Key([mod], "Return", lazy.spawn(term)),
    Key([mod], "w", lazy.spawn(browser)),
    Key([mod], "e", lazy.spawn(gui_editor)),
    Key([mod], "space", lazy.spawn(app_launcher)),
    # tui
    Key([mod], "c", lazy.spawn("kitty -e calcurse")),
    Key([mod], "f", lazy.spawn("kitty -e /home/tomas/bin/tui-utils/lf")),
    

    #### SOUND AND SCREEN CONTROL ####
    # sound
    Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -c 0 sset Master 2- unmute")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -c 0 sset Master 2+ unmute")),


    #### REDD BUTTONS ####
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown())
]

# groups = [Group(i) for i in "asdfuiop"]
# groups = [Group(i) for i in "123456789"]
# for i in groups:
#     keys.extend([
#         # mod1 + letter of group = switch to group
#         Key([mod], i.name, lazy.group[i+1].toscreen()),

#         # mod1 + shift + letter of group = switch to & move focused window to group
#         Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
#     ])


def init_group_names():
    return [
        ("DEV", {'layout': 'xmonadtall'}), 
        ("WEB", {'layout': 'xmonadtall'}),
        ("SYS", {'layout': 'treetab'}),
        ("DOC", {'layout': 'max'}),
        ("TERM", {'layout': 'bsp'}),
        ("CHAT", {'layout': 'bsp'}),
        ("GFX", {'layout': 'max'}),
        ("VID", {'layout': 'xmonadtall'}),
        ("MUS", {'layout': 'floating'})
    ]

def init_groups():
    return [Group(name, **kwargs) for name, kwargs in group_names]

if __name__ in ["config", "__main__"]:
    group_names = init_group_names()
    groups = init_groups()

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to anoter group

layouts = [
    layout.Max(),
    layout.MonadTall(
        name='xmonadtall',
        border_width=bwidth,
        border_normal=bcnormal,
        border_focus=bcfocused,
        ratio=0.5,
        margin=margin
    ),
    layout.MonadWide(
        name='xmonadwide',
        border_width=bwidth,
        border_normal=bcnormal,
        border_focus=bcfocused,
        ratio=0.5,
        margin=margin
    ),
    layout.Bsp(
        name='bsp',
        border_width=bwidth,
        border_normal=bcnormal, 
        border_focus=bcfocused, 
        fair=False,
        margin=margin
    ),
    layout.Floating(
        name='floating',
        border_width=bwidth,
        border_normal=bcnormal, 
        border_focus=bcfocused,
        max_border_width=bwidth
    ),
    layout.Zoomy(

        margin=17
    ),
    #TODO Customise this as much as posible, for it might end up being a very useful layout
    layout.TreeTab(
        name='treetab',
        sections=["First", "Second", "Third"],
        border_normal=bcnormal,
        border_focus=bcfocused,
        margin_y=margin,
        margin_left=margin,
        padding_x=0,
        padding_y=10,
        panel_width=160,
        font=mono_font,
        fontsize=10
    )
    # layout.Stack(num_stacks=2)
]

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                ## GROUPS ##
                widget.GroupBox(
                    font="San Francisco",
                    foreground=bar_colors[0],
                    background=bar_colors[4],
                    highlight_method="line",
                    highlight_color=bar_colors[7],
                    active=bar_colors[0],
                    this_current_screen_border=bar_colors[1],
                    this_screen_border=bar_colors[3],
                    other_current_screen_border=bar_colors[5],
                    other_screen_border=bar_colors[2],
                    padding=1,
                    rounded=False
                ),
                # widget.Prompt(),
                widget.Sep(
                    linewidth=10,
                    foreground=bar_colors[0],
                    background=bar_colors[0],
                    size_percent=100
                ),
                ## WINDOW NAME ##
                widget.WindowName(
                    foreground=bar_colors[1],
                    background=bar_colors[0]
                ),
                widget.TextBox(
                    text='',
                    background = bar_colors[0],
                    foreground = bar_colors[4],
                    font="Hack",
                    padding=-15,
                    fontsize=75
                ),
                widget.CurrentLayout(
                    foreground=bar_colors[0],
                    background=bar_colors[4]
                ),
                widget.Sep(
                    linewidth=4,
                    foreground=bar_colors[4],
                    background=bar_colors[4],
                    size_percent=100
                ),
                widget.TextBox(
                        text='',
                        background = bar_colors[4],
                        foreground = bar_colors[2],
                        font="Hack",
                        padding=-15,
                        fontsize=75
                ),
                widget.Backlight(
                    foreground=bar_colors[0],
                    background=bar_colors[2],
                    backlight_name='intel_backlight'
                ),
                widget.Sep(
                    linewidth=4,
                    foreground=bar_colors[2],
                    background=bar_colors[2],
                    size_percent=100
                ),
                widget.TextBox(
                        text='',
                        background = bar_colors[2],
                        foreground = bar_colors[3],
                        font="Hack",
                        padding=-15,
                        fontsize=75
                ),
                widget.Volume(
                    # fmt="🔊 V{}",
                    emoji=True,
                    foreground=bar_colors[0],
                    background=bar_colors[3]
                ),
                widget.Volume(
                    # fmt="🔊 V{}",
                    # emoji=True,
                    foreground=bar_colors[0],
                    background=bar_colors[3]
                ),
                widget.Sep(
                    linewidth=4,
                    foreground=bar_colors[3],
                    background=bar_colors[3],
                    size_percent=100
                ),
                widget.TextBox(
                        text='',
                        background = bar_colors[3],
                        foreground = bar_colors[5],
                        font="Hack",
                        padding=-15,
                        fontsize=75
                ),
                widget.Wlan(
                    interface='wlp58s0',
                    format='{essid}: [{quality}/70]',
                    foreground=bar_colors[0],
                    background=bar_colors[5]
                ),
                widget.Sep(
                    linewidth=4,
                    foreground=bar_colors[5],
                    background=bar_colors[5],
                    size_percent=100
                ),
                widget.TextBox(
                        text='',
                        background = bar_colors[5],
                        foreground = bar_colors[1],
                        font="Hack",
                        padding=-15,
                        fontsize=75
                ),
                widget.Clock(
                    foreground=bar_colors[0],
                    background=bar_colors[1],
                    format='%d/%m/%y [%H:%M]'
                    ),
                widget.Sep(
                    linewidth=4,
                    foreground=bar_colors[1],
                    background=bar_colors[1],
                    size_percent=100
                ),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
