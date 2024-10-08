local apps = {
    emacs = 'Emacs.app',
    kitty = 'Kitty.app',
    chrome = 'Google Chrome.app',
    messages = 'Messages.app'
}
local hyper         = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1, CENTRED = 2 }
local screen_state  = screen_states.FULLSCREEN

hs.window.animationDuration = 0

local function call_app_fullscreen (key, app)
    hs.hotkey.bind(hyper, key, function()
        hs.application.open(app)
        if screen_state == screen_states.FULLSCREEN then
           hs.window.focusedWindow():maximize()
        end
    end)
end

-- Function to switch between layouts
local function switch_layouts(key, target_state, app1, app2)
    hs.hotkey.bind(hyper, key, function()
        screen_state = target_state
        if target_state == screen_states.TWOPANE then
            hs.application.open(app2)
            hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1}) -- Right side
            hs.application.open(app1)
            hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1}) -- Left side
        elseif target_state == screen_states.CENTRED then
            hs.window.focusedWindow():moveToUnit({0.1, 0.1, 0.8, 0.8}) -- Centered (80% width and height)
        else
            hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) -- Fullscreen
        end
    end)
end

-- Reload Hammerspoon configuration
hs.hotkey.bind(hyper, "r", hs.reload)
-- hs.hotkey.bind(hyper, "c", hs.toggleConsole)

-- Bind applications to hotkeys
call_app_fullscreen("j", apps.emacs)
call_app_fullscreen("k", apps.kitty)
call_app_fullscreen("l", apps.chrome)
call_app_fullscreen("m", apps.messages)

-- Bind layout switching hotkeys
switch_layouts("f", screen_states.FULLSCREEN)
switch_layouts("c", screen_states.CENTRED)
switch_layouts("1", screen_states.TWOPANE, apps.emacs, apps.kitty)
switch_layouts("2", screen_states.TWOPANE, apps.emacs, apps.chrome)
switch_layouts("3", screen_states.TWOPANE, apps.kitty, apps.chrome)
