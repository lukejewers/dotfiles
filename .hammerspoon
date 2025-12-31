-------------------------------------------------------------------
--  APPLICATION & LAYOUT DEFINITIONS
-------------------------------------------------------------------

-- Modifier key
local hyper = { "ctrl", "cmd" }

-- Apps
local apps = {
    emacs = 'Emacs',
    finder = 'Finder',
    firefox = 'Firefox',
    messages = 'Messages',
    settings = 'System Settings',
}

-- Popup apps
local popup_apps = {
    [apps.messages] = true,
    [apps.finder]   = true,
    [apps.settings] = true,
}

-- Layout state management
local screen_states = { FULLSCREEN = 0, TWOPANE = 1 }
local screen_state  = screen_states.FULLSCREEN

-------------------------------------------------------------------
--  APP LAUNCHER
-------------------------------------------------------------------

-- Opens and activates an application
local function open_and_activate(app)
    hs.application.open(app)
    hs.application.get(app):activate()
end

-- Checks if an app is designated as a "popup" app
local function is_popup_app(app)
    return popup_apps[app] or false
end

-- Binds a key to launch or switch to an application
local function call_app(key, app)
    hs.hotkey.bind(hyper, key, function()
        local focusedApp = hs.application.frontmostApplication()
        if focusedApp and is_popup_app(focusedApp:title()) then
            focusedApp:hide()
        else
            open_and_activate(app)
            local window = hs.window.focusedWindow()
            if window then
                if is_popup_app(app) then
                    window:moveToUnit({0.17, 0.17, 0.67, 0.67}) -- centered
                elseif screen_state == screen_states.FULLSCREEN then
                    window:maximize()
                end
            end
        end
    end)
end

-- Binds a key to switch between pre-defined window layouts
local function switch_layouts(key, target_state, app1, app2)
    hs.hotkey.bind(hyper, key, function()
        screen_state = target_state
        local focused_window = hs.window.focusedWindow()
        if target_state == screen_states.TWOPANE then
            open_and_activate(app2)
            hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1}) -- Right side
            open_and_activate(app1)
            hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1})   -- Left side
        elseif focused_window then
            focused_window:maximize()
        end
    end)
end

-------------------------------------------------------------------
--  WINDOW MANAGEMENT
-------------------------------------------------------------------

-- Left Half
hs.hotkey.bind(hyper, "Left", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0, 0, 0.5, 1})
end)

-- Right Half
hs.hotkey.bind(hyper, "Right", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0.5, 0, 0.5, 1})
end)

-- Fullscreen
hs.hotkey.bind(hyper, "Up", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:maximize()
end)

-- Center
hs.hotkey.bind(hyper, "Down", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0.15, 0.15, 0.7, 0.7})
end)

-------------------------------------------------------------------
-- BIND HOTKEYS
-------------------------------------------------------------------

-- Applications key bindings
call_app("f", apps.finder)
call_app("j", apps.emacs)
call_app("k", apps.firefox)
call_app("m", apps.messages)
call_app("s", apps.settings)

-- Layout key bindings
switch_layouts("t", screen_states.FULLSCREEN)
switch_layouts("e", screen_states.TWOPANE, apps.emacs, apps.firefox)

-- Debug info (uncomment to use)
-- hs.hotkey.bind(hyper, "r", hs.reload)
-- hs.hotkey.bind(hyper, "c", hs.toggleConsole)

hs.alert.show("Hammerspoon config loaded")
