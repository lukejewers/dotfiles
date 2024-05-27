local hyper = { "ctrl", "cmd" }
local screen_states = { FULLSCREEN = 0, TWOPANE = 1}
local screen_state = screen_states.FULLSCREEN

hs.hotkey.bind(hyper, "r", hs.reload)
hs.hotkey.bind(hyper, "c", hs.toggleConsole)

hs.window.animationDuration = 0

hs.hotkey.bind(hyper, "z", function()
    if screen_state == screen_states.FULLSCREEN then
       screen_state = screen_states.TWOPANE
       -- Right side of screen
       hs.application.open('Google Chrome.app')
       hs.window.focusedWindow():moveToUnit({0.5, 0, 0.5, 1})
       -- Left side of screen
       hs.application.open('Emacs.app')
       hs.window.focusedWindow():moveToUnit({0, 0, 0.5, 1})
       --
    else
       screen_state = screen_states.FULLSCREEN
       hs.window.focusedWindow():moveToUnit({0, 0, 1, 1})
    end
end)

hs.hotkey.bind(hyper, "j", function()
    hs.application.open('Emacs.app')
    if screen_state == screen_state.FULLSCREEN then
       hs.window.focusedWindow():maximize()
    end
end)

hs.hotkey.bind(hyper, "k", function()
    hs.application.open('Kitty.app')
    if screen_state == screen_state.FULLSCREEN then
       hs.window.focusedWindow():maximize()
    end
end)

hs.hotkey.bind(hyper, "l", function()
    hs.application.open('Google Chrome.app')
    if screen_state == screen_state.FULLSCREEN then
       hs.window.focusedWindow():maximize()
    end
end)
