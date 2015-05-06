local hotkey = require "mjolnir.hotkey"
local window = require "mjolnir.window"
local alert = require "mjolnir.alert"
local screen = require "mjolnir.screen"
local mash = {"cmd", "alt"}

mjolnir.openconsole()

hotkey.bind(mash, "DOWN", function ()
    local win = window.focusedwindow()
    if not win then return end
    local f = win:frame()
    local s = win:screen():frame()
    if f.w == 1280 and f.h == 720 and f.x == s.w / 2 - 640 and f.y == s.h / 2 - 360 then
        f.w = 1920
        f.h = 1080
        f.x = (s.w / 2) - 960
        f.y = (s.h / 2) - 540
        alert.show('ScreenCast 1080p')
    else
        f.w = 1280
        f.h = 720
        f.x = (s.w / 2) - 640
        f.y = (s.h / 2) - 360
        alert.show('ScreenCast 720p')
    end
    win:setframe(f)
end)

hotkey.bind(mash, "UP", function ()
    local win = window.focusedwindow()
    if not win then return end
    local f = win:frame()
    local s = win:screen():frame()
    if f.w == s.w and f.h == s.h / 2 and f.y ~= s.h / 2 then
        f.w = s.w
        f.h = s.h / 2
        f.x = 0
        f.y = s.h / 2
        alert.show('Bottom 1/2')
    elseif f.w == s.w and f.h == s.h then
        f.w = s.w
        f.h = s.h / 2
        f.x = 0
        f.y = 0
        alert.show('Top 1/2')
    else
        f.w = s.w
        f.h = s.h
        f.x = 0
        f.y = 0
        alert.show('FullScreen')
    end
    win:setframe(f)
end)

hotkey.bind(mash, "LEFT", function ()
    local win = window.focusedwindow()
    if not win then return end
    local f = win:frame()
    local s = win:screen():frame()
    if f.w == s.w / 3 and f.x == 0 then
        f.w = s.w / 1.5
        alert.show('Left 2/3', 1)
    elseif f.w == s.w / 2 and f.x == 0 then
        f.w = s.w / 3
        alert.show('Left 1/3', 1)
    else
        f.w = s.w / 2
        alert.show('Left 1/2', 1)
    end
    f.h = s.h
    f.x = 0
    f.y = 0
    win:setframe(f)
end)


hotkey.bind(mash, "RIGHT", function ()
    local win = window.focusedwindow()
    if not win then return end
    local f = win:frame()
    local s = win:screen():frame()
    if f.w == s.w / 3 and f.x == s.w / 1.5 then
        f.w = s.w / 1.5
        alert.show('Right 2/3', 1)
    elseif f.w == s.w / 2 and f.x == s.w / 2 then
        f.w = s.w / 3
        alert.show('Right 1/3', 1)
    else
        f.w = s.w / 2
        alert.show('Right 1/2', 1)
    end
    f.h = s.h
    f.x = s.w - f.w
    f.y = 0
    win:setframe(f)
end)
