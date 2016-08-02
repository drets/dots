---------------------------
-- Default awesome theme --
---------------------------

theme = {}

theme.font          = "Ubuntu Mono 14"

theme.bg_normal     = "#222222"
theme.bg_focus      = "#535d6c"
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#aaaaaa"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.border_width  = 1
theme.border_normal = "#000000"
theme.border_focus  = "#535d6c"
theme.border_marked = "#91231c"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Display the taglist squares
theme.taglist_squares_sel   = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/taglist/squarefw.png"
theme.taglist_squares_unsel = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/taglist/squarew.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/titlebar/maximized_focus_active.png"

theme.wallpaper = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/fairhw.png"
theme.layout_fairv = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/fairvw.png"
theme.layout_floating  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/floatingw.png"
theme.layout_magnifier = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/magnifierw.png"
theme.layout_max = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/maxw.png"
theme.layout_fullscreen = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/fullscreenw.png"
theme.layout_tilebottom = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/tilebottomw.png"
theme.layout_tileleft   = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/tileleftw.png"
theme.layout_tile = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/tilew.png"
theme.layout_tiletop = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/tiletopw.png"
theme.layout_spiral  = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/spiralw.png"
theme.layout_dwindle = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/themes/default/layouts/dwindlew.png"

theme.awesome_icon = "/nix/store/9y9cdc17laim1vmak3wwnsd13g3j8a11-awesome-3.5.9/share/awesome/icons/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
