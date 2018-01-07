-- Load Extensions
local application = require "hs.application"
local window = require "hs.window"
local hotkey = require "hs.hotkey"
local keycodes = require "hs.keycodes"
local fnutils = require "hs.fnutils"
local alert = require "hs.alert"
local screen = require "hs.screen"
local grid = require "hs.grid"
local osascript = require "hs.osascript"
local hints = require "hs.hints"
local appfinder = require "hs.appfinder"

-- Watch for Minimizing Windows and/or Applications

local appWatcherStarted = false
local appWatchers = {}

-- Toggle an application between being the frontmost app, and being hidden
function toggleApplication(appName)
    local app = hs.appfinder.appFromName(appName)
    if not app then
        return
    end

    local mainWin = app:mainWindow()
    if mainWin then
        if mainWin == hs.window.focusedWindow() then
            mainWin:application():hide()
        else
            mainWin:application():activate(true)
            mainWin:application():unhide()
            mainWin:focus()
        end
    end
end

local function watchApp(app)
  if appWatchers[app:pid()] then return end

  local watcher = app:newWatcher(function (el,ev,wat,ud)
    if ev == hs.uielement.watcher.windowMinimized.windowCreated then
        -- watchWindow(el)
    elseif ev == hs.uielement.watcher.windowMinimized then
      _animationDuration = hs.window.animationDuration
      hs.window.animationDuration = 0
      el:unminimize()
      launchAppleScript(el:application():name())
      hs.window.animationDuration = _animationDuration
    end
  end)

  appWatchers[app:pid()] = {watcher = watcher}
  watcher:start({hs.uielement.watcher.windowMinimized})
end

function enableForApp(app)
  local appObject = hs.application.get(app)

  -- might already be running
  if appObject then
    watchApp(appObject)
  end

  -- set up a watcher to catch any watched app launching or terminating
  if appWatcherStarted then return end
  appWatcherStarted = true

  local appWatcher = hs.application.watcher.new(function(appName, eventType, appObject)
    if (eventType == hs.application.watcher.activated) then
      -- Bring all Finder windows forward when one gets activated
      if (appName == 'Finder') then
        appObject:selectMenuItem({'Window', 'Bring All to Front'})
      elseif (appName == 'Fantastical') then
        appObject:selectMenuItem({'Window', 'Full Calendar Window'})
      else
        -- appObject:selectMenuItem({'Window', 'Bring All to Front'})
      end
    elseif (eventType == hs.application.watcher.launched) then
      watchApp(appObject)
    elseif (eventType == hs.application.watcher.terminated) then
      -- Clean up
      local appWatch = appWatchers[appObject:pid()]
      if appWatch then
        appWatch.watcher:stop()
        appWatchers[appObject:pid()] = nil
      end
    end
  end)

  appWatcher:start()
end

-- TODO Update keyboard settings from below:
-- https://github.com/jasonrudolph/keyboard

appWorkflow = [[
  set appName to "%s"
  set startIt to false
  tell application "System Events"
  	if not (exists process appName) then
  		set startIt to true
  	else if frontmost of process appName then
  		set visible of process appName to false
  	else
  		set frontmost of process appName to true
  	end if
  end tell
  if startIt then
  	tell application appName to activate
  end if
]]

launchAppleScript = function(appName)
  str = string.format(appWorkflow, appName)
  osascript.applescript(str)
end

launchSingle = function(appname)
  hs.application.launchOrFocus(appname)
end

oascripts = {
  -- {'z', ''}, was saved for menu pop
  {'x', 'Safari'},
  {'c', 'Google Chrome Canary'},
  {'v', 'MacVim'},
  {'b', 'Google Chrome'},
  {'n', 'NotePlan'},
  {'m', 'MailMate'},
  {'a', 'BusyContacts'},
  {'s', 'Sonos'},
  -- {'d', ''}, saved for dash
  {'f', 'Finder'},
  {'g', 'Tower'},
  -- {'h', ''}, saved for launchbar
  {'j', 'Jump Desktop'},
  -- {'k', 'Fantastical 2'},
  -- {'l', ''}, no action
  -- {';', ''}, daylite
  {'q', 'Quiver'},
  -- {'w', ''}, saved for moom
  {'e', 'Emacs'},
  -- {'r', ''}, saved for fantastical 2
  -- {'t', 'iTerm'},
  -- {'y', ''}, no action
  {'u', 'Calcbot'},
  {'i', 'Messages'},
  {'o', 'OmniFocus'},
  -- {'p', ''}, saved for snippets lab
}

for i, app in ipairs(oascripts) do
  hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, app[1], function()
      launchAppleScript(app[2])
        end, nil, function()
      launchAppleScript(app[2])
  end)
  enableForApp(app[2])
end

-- double identity apps

doubleapps = {
  {'t', 'iTerm2', 'iTerm'},
  {'k', 'Fantastical', 'Fantastical 2'},
}

launchDouble = function(appName1, appName2)
  local app = hs.application.find(appName1)
  if (app and hs.application.isRunning(app)) then
    -- alert(string.format('found running app: %s', appName1))
    launchAppleScript(appName1)
  else
    -- alert(string.format('launching app: %s', appName2))
    hs.application.launchOrFocus(appName2)
  end
end

for i, app in ipairs(doubleapps) do
  hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, app[1], function()
      launchDouble(app[2], app[3])
                     end, nil, function ()
      launchDouble(app[2], app[3])
  end)
  enableForApp(app[2])
end


-- Cursor locator

local mouseCircle = nil
local mouseCircleTimer = nil

function mouseHighlight()
  size = 150
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        mouseCircle2:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.getAbsolutePosition()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-(size/2), mousepoint.y-(size/2), size, size))
    mouseCircle2 = hs.drawing.circle(hs.geometry.rect(mousepoint.x-(size/4), mousepoint.y-(size/4), size/2, size/2))
    mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle2:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle:setFill(false)
    mouseCircle2:setFill(false)
    mouseCircle:setStrokeWidth(3)
    mouseCircle2:setStrokeWidth(5)
    mouseCircle:show()
    mouseCircle2:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(1, function() mouseCircle:delete() mouseCircle2:delete() end)
end
-- hotkey.bind({"cmd","alt","shift"}, "D", mouseHighlight)

-- USB events

local usbWatcher = nil

function usbDeviceCallback(data)
  if (data["productName"] == "ScanSnap iX500") then
    if (data["eventType"] == "added") then
      hs.application.launchOrFocus("ScanSnap Manager")
    elseif (data["eventType"] == "removed") then
      app = hs.appfinder.appFromName("ScanSnap Manager")
      app:kill()
    end
  end
end

function createUsbWatcher()
  usbWatcher = hs.usb.watcher.new(usbDeviceCallback)
  usbWatcher:start()
end

-- tabs

local tabs = require "tabs"

local definitions = nil
local hyper = nil
local hyper2 = nil

local gridset = function(frame)
	return function()
		local win = window.focusedWindow()
		if win then
			grid.set(win, frame, win:screen())
		else
			alert.show("No focused window.")
		end
	end
end

auxWin = nil
function saveFocus()
  auxWin = window.focusedWindow()
  alert.show("Window '" .. auxWin:title() .. "' saved.")
end
function focusSaved()
  if auxWin then
    auxWin:focus()
  end
end

local hotkeys = {}

function createHotkeys()
  for key, fun in pairs(definitions) do
    local mod = hyper
    if string.len(key) == 2 and string.sub(key,2,2) == "c" then
      mod = {"cmd"}
    elseif string.len(key) == 2 and string.sub(key,2,2) == "l" then
      mod = {"ctrl"}
    end

    local hk = hotkey.new(mod, string.sub(key,1,1), fun)
    table.insert(hotkeys, hk)
    hk:enable()
  end
end

function rebindHotkeys()
  for i, hk in ipairs(hotkeys) do
    hk:disable()
  end
  hotkeys = {}
  createHotkeys()
  alert.show("Rebound Hotkeys")
end

function applyPlace(win, place)
  local scrs = screen:allScreens()
  local scr = scrs[place[1]]
  grid.set(win, place[2], scr)
end

function applyLayout(layout)
  return function()
    for appName, place in pairs(layout) do
      local app = appfinder.appFromName(appName)
      if app then
        for i, win in ipairs(app:allWindows()) do
          applyPlace(win, place)
        end
      end
    end
  end
end

function init()
  createUsbWatcher()
  -- createHotkeys()
  -- keycodes.inputSourceChanged(rebindHotkeys)

  alert.show("Hammerspoon, at your service.")
end

-- Actual config =================================

hyper = {"cmd", "alt", "ctrl","shift"}
hyper2 = {"ctrl"}
hs.window.animationDuration = 0;
-- hints.style = "vimperator"
-- Set grid size.
grid.GRIDWIDTH  = 6
grid.GRIDHEIGHT = 8
grid.MARGINX = 0
grid.MARGINY = 0
local gw = grid.GRIDWIDTH
local gh = grid.GRIDHEIGHT

local gomiddle = {x = 1, y = 1, w = 4, h = 6}
local goleft = {x = 0, y = 0, w = gw/2, h = gh}
local goright = {x = gw/2, y = 0, w = gw/2, h = gh}
local gobig = {x = 0, y = 0, w = gw, h = gh}

local fullApps = {
  "Safari","Aurora","Nightly","Xcode","Qt Creator","Google Chrome",
  "Google Chrome Canary", "Eclipse", "Coda 2", "iTunes", "Emacs", "Firefox"
}
local layout2 = {
  Airmail = {1, gomiddle},
  Spotify = {1, gomiddle},
  Calendar = {1, gomiddle},
  Dash = {1, gomiddle},
  iTerm = {2, goright},
  MacRanger = {2, goleft},
}
fnutils.each(fullApps, function(app) layout2[app] = {1, gobig} end)

definitions = {
  [";"] = saveFocus,
  a = focusSaved,

  h = gridset(gomiddle),
  t = gridset(goleft),
  n = grid.maximizeWindow,
  s = gridset(goright),

  g = applyLayout(layout2),

  d = grid.pushWindowNextScreen,
  r = hs.reload,
  q = function() appfinder.appFromName("Hammerspoon"):kill() end,

  k = function() hints.windowHints(appfinder.appFromName("Emacs"):allWindows()) end,
  j = function() hints.windowHints(window.focusedWindow():application():allWindows()) end,
  ll = function() hyper, hyper2 = hyper2,hyper; rebindHotkeys() end,
  ec = function() hints.windowHints(nil) end
}

-- launch and focus applications
fnutils.each({
  { key = "o", app = "MacRanger" },
  { key = "e", app = "Google Chrome" },
  { key = "u", app = "Emacs" },
  { key = "i", app = "iTerm2" },
  { key = "m", app = "Airmail" }
}, function(object)
    definitions[object.key] = function()
      local app = appfinder.appFromName(object.app)
      if app then app:activate() end
    end
end)

for i=1,6 do
  definitions[tostring(i)] = function()
    local app = application.frontmostApplication()
    tabs.focusTab(app,i)
  end
end

init()
