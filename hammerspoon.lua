local apps = {
    chrome = 'Google Chrome.app',
    dictionary = 'Dictionary.app',
    emacs = 'Emacs.app',
    kitty = 'Kitty.app',
    messages = 'Messages.app',
}
local hyper         = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1, CENTRED = 2 }
local screen_state  = screen_states.FULLSCREEN

hs.window.animationDuration = 0

local function call_app (key, app)
    hs.hotkey.bind(hyper, key, function()
        hs.application.open(app)
        if app == apps.messages or app == apps.dictionary then
           hs.window.focusedWindow():moveToUnit({0.17, 0.17, 0.67, 0.67}) -- Centered (70% width and height)
        elseif screen_state == screen_states.FULLSCREEN then
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
call_app("j", apps.emacs)
call_app("k", apps.kitty)
call_app("l", apps.chrome)
call_app("m", apps.messages)
call_app("d", apps.dictionary)

-- Bind layout switching hotkeys
switch_layouts("f", screen_states.FULLSCREEN)
switch_layouts("c", screen_states.CENTRED)
switch_layouts("1", screen_states.TWOPANE, apps.emacs, apps.kitty)
switch_layouts("2", screen_states.TWOPANE, apps.emacs, apps.chrome)
switch_layouts("3", screen_states.TWOPANE, apps.kitty, apps.chrome)
