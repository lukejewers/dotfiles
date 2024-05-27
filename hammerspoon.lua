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

hs.hotkey.bind(hyper, "r", hs.reload)
hs.hotkey.bind(hyper, "c", hs.toggleConsole)

call_app_fullscreen("j", emacs_app)
call_app_fullscreen("k", kitty_app)
call_app_fullscreen("l", chrome_app)

hs.hotkey.bind(hyper, "z", function()
    if screen_state == screen_states.FULLSCREEN then
       screen_state = screen_states.TWOPANE
       -- Right side of screen
       hs.application.open(chrome_app)
       hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1})
       -- Left side of screen
       hs.application.open(emacs_app)
       hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1})
    elseif screen_state == screen_states.TWOPANE then
       screen_state = screen_states.FULLSCREEN
       hs.window.focusedWindow():moveToUnit({0, 0, 1, 1})
    end
end)
