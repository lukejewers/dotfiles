local apps = {
    chrome = 'Google Chrome',
    emacs = 'Emacs',
    finder = 'Finder',
    ghostty = 'Ghostty',
    kindle = 'Kindle',
    messages = 'Messages',
}
local hyper         = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1, CENTRED = 2 }
local screen_state  = screen_states.FULLSCREEN

hs.window.animationDuration = 0

local function open_and_activate(app)
    hs.application.open(app)
    hs.application.get(app):activate()
end

local function call_app(key, app)
    hs.hotkey.bind(hyper, key, function()
        local focusedApp = hs.application.frontmostApplication()
        if focusedApp and (focusedApp:title() == apps.messages or focusedApp:title() == apps.finder) then
            focusedApp:hide()
        else
            open_and_activate(app)
            local window = hs.window.focusedWindow()
            if app == apps.messages or app == apps.finder then
                window:moveToUnit({0.17, 0.17, 0.67, 0.67})
            elseif screen_state == screen_states.FULLSCREEN then
                window:maximize()
            end
        end
    end)
end

-- Function to switch between layouts
local function switch_layouts(key, target_state, app1, app2)
    hs.hotkey.bind(hyper, key, function()
        screen_state = target_state
        if target_state == screen_states.TWOPANE then
            open_and_activate(app2)
            hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1}) -- Right side
            open_and_activate(app1)
            hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1}) -- Left side
        elseif target_state == screen_states.CENTRED then
            hs.window.focusedWindow():moveToUnit({0.1, 0.1, 0.8, 0.8}) -- Centered (80% width and height)
        else
            hs.window.focusedWindow():moveToUnit({0, 0, 1, 1}) -- Fullscreen
        end
    end)
end

-- Debug info
-- hs.hotkey.bind(hyper, "r", hs.reload)
-- hs.hotkey.bind(hyper, "o", hs.toggleConsole)

-- Bind applications to hotkeys
call_app("f", apps.finder)
call_app("i", apps.kindle)
call_app("j", apps.emacs)
call_app("k", apps.ghostty)
call_app("l", apps.chrome)
call_app("m", apps.messages)

-- Bind layout switching hotkeys
switch_layouts("9", screen_states.CENTRED)
switch_layouts("0", screen_states.FULLSCREEN)
switch_layouts("8", screen_states.TWOPANE, apps.emacs, apps.chrome)
