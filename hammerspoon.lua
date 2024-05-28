local hyper         = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1 }
local screen_state  = screen_states.FULLSCREEN
local emacs_app     = 'Emacs.app'
local kitty_app     = 'Kitty.app'
local chrome_app    = 'Google Chrome.app'

hs.window.animationDuration = 0

local function call_app_fullscreen (key, app)
    hs.hotkey.bind(hyper, key, function()
        hs.application.open(app)
        if screen_state == screen_states.FULLSCREEN then
           hs.window.focusedWindow():maximize()
        end
    end)
end

local function switch_layouts (key, screen_state_layout, app1, app2)
    hs.hotkey.bind(hyper, key, function()
        if screen_state_layout == screen_states.TWOPANE then
           screen_state = screen_state_layout
           -- Right side of screen
           hs.application.open(app2)
           hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1})
           -- Left side of screen
           hs.application.open(app1)
           hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1})
        elseif screen_state_layout == screen_states.FULLSCREEN then
           screen_state = screen_state_layout
           screen_state = screen_states.FULLSCREEN
           hs.window.focusedWindow():moveToUnit({0, 0, 1, 1})
        end
    end)
end

hs.hotkey.bind(hyper, "r", hs.reload)
hs.hotkey.bind(hyper, "c", hs.toggleConsole)

call_app_fullscreen("j", emacs_app)
call_app_fullscreen("k", kitty_app)
call_app_fullscreen("l", chrome_app)

switch_layouts("0", screen_states.FULLSCREEN)
switch_layouts("1", screen_states.TWOPANE, emacs_app, kitty_app)
switch_layouts("2", screen_states.TWOPANE, emacs_app, chrome_app)
switch_layouts("3", screen_states.TWOPANE, kitty_app, chrome_app)
