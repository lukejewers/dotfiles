 -------------------------------------------------------------------
--  CONFIGURATION
-------------------------------------------------------------------

-- Animation duration for window movements.
-- A very small value is used to ensure mouse dragging works correctly.
hs.window.animationDuration = 0.01

-- Modifier key
local hyper     = { "ctrl", "cmd" }
local alt_hyper = { "ctrl", "cmd", "alt" }

-- Set to true to snap windows by dragging them to the edge of your screen
local enable_window_snapping_with_mouse = true

-- Set to true to snap windows using keyboard shortcuts (eg. Ctrl + Option + Right Arrow)
local enable_window_snapping_with_keyboard = true

-- The height of the window's title area (in pixels). Used for mouse-drag detection.
local window_titlebar_height = 0

-- The sensitivity (in pixels) for snapping when the mouse reaches the screen edge.
local monitor_edge_sensitivity = 0

-------------------------------------------------------------------
--  APPLICATION & LAYOUT DEFINITIONS
-------------------------------------------------------------------

-- Apps
local apps = {
    emacs = 'Emacs',
    finder = 'Finder',
    firefox = 'Firefox',
    kindle = 'Kindle',
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
--  WINDOW SNAPPING
--  From https://gist.githubusercontent.com/spartanatreyu/850788a0441e1c5565668a35ed9a1dfc/raw/a6ac8731014f99e0adf46878a0a03f04badb7656/init.lua
-------------------------------------------------------------------

function round(num)
    return math.floor(num + 0.5)
end

function get_window_under_mouse()
    local my_pos = hs.geometry.new(hs.mouse.getAbsolutePosition())
    local my_screen = hs.mouse.getCurrentScreen()

    return hs.fnutils.find(hs.window.orderedWindows(), function(w)
        return my_screen == w:screen() and my_pos:inside(w:frame())
    end)
end


local dragging = 0 -- 0: no drag, 1: dragging a window, -1: dragging but not a window
local dragging_window = nil

local click_event = hs.eventtap.new({hs.eventtap.event.types.leftMouseDragged}, function(e)
    if dragging == 0 then
        dragging_window = get_window_under_mouse()
        if dragging_window ~= nil then
            local m = hs.mouse.getAbsolutePosition()
            local f = dragging_window:frame()
            -- Check if mouse is inside the titlebar area
            if m.x > f.x and m.x < (f.x + f.w) and m.y > f.y and m.y < (f.y + window_titlebar_height) then
                dragging = 1
            else
                dragging = -1
                dragging_window = nil
            end
        end
    end
end)

local unclick_event = hs.eventtap.new({hs.eventtap.event.types.leftMouseUp}, function(e)
    if dragging == 1 and dragging_window ~= nil then
        local win = dragging_window
        local m = hs.mouse.getAbsolutePosition()
        local screen = win:screen()
        local max = screen:frame()
        local f = {} -- New frame

        -- Top Left
        if m.x <= max.x + monitor_edge_sensitivity and m.y <= max.y + monitor_edge_sensitivity then
            f = {x = max.x, y = max.y, w = max.w / 2, h = max.h / 2}
        -- Maximize
        elseif m.y <= max.y + monitor_edge_sensitivity then
            win:maximize()
            f = nil
        -- Top Right
        elseif m.x >= max.x + max.w - monitor_edge_sensitivity and m.y <= max.y + monitor_edge_sensitivity then
            f = {x = max.x + max.w / 2, y = max.y, w = max.w / 2, h = max.h / 2}
        -- Left Half
        elseif m.x <= max.x + monitor_edge_sensitivity then
            f = {x = max.x, y = max.y, w = max.w / 2, h = max.h}
        -- Right Half
        elseif m.x >= max.x + max.w - monitor_edge_sensitivity then
            f = {x = max.x + max.w / 2, y = max.y, w = max.w / 2, h = max.h}
        -- Bottom Left
        elseif m.x <= max.x + monitor_edge_sensitivity and m.y >= max.y + max.h - monitor_edge_sensitivity then
            f = {x = max.x, y = max.y + max.h / 2, w = max.w / 2, h = max.h / 2}
        -- Bottom Right
        elseif m.x >= max.x + max.w - monitor_edge_sensitivity and m.y >= max.y + max.h - monitor_edge_sensitivity then
            f = {x = max.x + max.w / 2, y = max.y + max.h / 2, w = max.w / 2, h = max.h / 2}
        else
            f = nil
        end

        if f then win:setFrame(f) end
    end

    dragging = 0
    dragging_window = nil
end)

click_event:start()
unclick_event:start()

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

-- Top Left Corner
hs.hotkey.bind(alt_hyper, "Up", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0, 0, 0.5, 0.5})
end)

-- Top Right Corner
hs.hotkey.bind(alt_hyper, "Right", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0.5, 0, 0.5, 0.5})
end)

-- Bottom Right Corner
hs.hotkey.bind(alt_hyper, "Down", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0.5, 0.5, 0.5, 0.5})
end)

-- Bottom Left Corner
hs.hotkey.bind(alt_hyper, "Left", function()
    local win = hs.window.focusedWindow()
    if not win then return end
    win:moveToUnit({0, 0.5, 0.5, 0.5})
end)

-------------------------------------------------------------------
-- BIND HOTKEYS
-------------------------------------------------------------------

-- Applications key bindings
call_app("f", apps.finder)
call_app("j", apps.emacs)
call_app("b", apps.kindle)
call_app("l", apps.firefox)
call_app("m", apps.messages)
call_app("s", apps.settings)

-- Layout key bindings
switch_layouts("0", screen_states.FULLSCREEN)
switch_layouts("9", screen_states.TWOPANE, apps.emacs, apps.firefox)

-- Debug info (uncomment to use)
-- hs.hotkey.bind(hyper, "r", hs.reload)
-- hs.hotkey.bind(hyper, "c", hs.toggleConsole)

hs.alert.show("Hammerspoon config loaded")
