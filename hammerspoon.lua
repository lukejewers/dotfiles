local apps = {
    emacs = 'Emacs',
    finder = 'Finder',
    firefox = 'Firefox',
    ghostty = 'Ghostty',
    messages = 'Messages',
    settings = 'System Settings',
}

local popup_apps = {
    [apps.messages] = true,
    [apps.finder]   = true,
    [apps.settings] = true,
    [apps.ghostty]  = true,
}

local hyper         = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1, CENTRED = 2 }
local screen_state  = screen_states.FULLSCREEN

hs.window.animationDuration = 0

local function open_and_activate(app)
    hs.application.open(app)
    hs.application.get(app):activate()
end

local function is_popup_app(app)
    return popup_apps[app] or false
end

local function call_app(key, app)
    hs.hotkey.bind(hyper, key, function()
        local focusedApp = hs.application.frontmostApplication()
        if focusedApp and is_popup_app(focusedApp:title()) then
            focusedApp:hide()
        else
            open_and_activate(app)
            local window = hs.window.focusedWindow()
            if is_popup_app(app) then
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
call_app("j", apps.emacs)
call_app("l", apps.firefox)
call_app("m", apps.messages)
call_app("s", apps.settings)
call_app("t", apps.ghostty)

-- Bind layout switching hotkeys
switch_layouts("9", screen_states.CENTRED)
switch_layouts("0", screen_states.FULLSCREEN)
switch_layouts("8", screen_states.TWOPANE, apps.emacs, apps.firefox)
