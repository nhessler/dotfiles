-- Enable IPC for CLI access (hs command)
require("hs.ipc")

-- Wave Link mic mute toggle
local wavelink = require("wavelink")
wavelink.connect()
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "m", function()
  wavelink.toggleMute()
end)

-- Config reload
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
  hs.reload()
  notification = hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded", autoWithdraw=true})
  notification:withdrawAfter(5)
  notification:send()
end)

-- Open daily apps
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "o", function()

-- Applications/
  hs.application.open("/Applications/1Password 7.app")
  hs.application.open("/Applications/Authy Desktop.app")
  hs.application.open("/Applications/Brave Browser.app")
  hs.application.open("/Applications/Dash.app")
  hs.application.open("/Applications/Emacs.app")
  hs.application.open("/Applications/Kiwi for Gmail.app")
  hs.application.open("Message")
  hs.application.open("/Applications/Miro.app")
  hs.application.open("/Applications/Notion.app")
  hs.application.open("/Applications/Safari.app")
  hs.application.open("/Applications/Slack.app")
  --hs.application.open("/Applications/Stocks.app")
  hs.application.open("/Applications/zoom.us.app")

-- Applications/Utilities
  hs.application.open("/Applications/Utilities/Terminal.app")

end)

-- Studio lights toggle
local studioLights = require("studio-lights")
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "l", function()
  studioLights.toggle()
end)

-- Window management
local wm = require("window-manager")

local windowManager = hs.hotkey.modal.new({"ctrl", "alt", "cmd"}, "w", "Window Manager")

-- Exits
for _i, key in pairs({"escape", "delete", "forwarddelete", "return"}) do
  windowManager:bind('', key, function()
    hs.alert("Windows Managed")
    windowManager:exit()
  end)
end

-- Halves
windowManager:bind('', "Right", "Right Half", function() wm.sizeAndMove({1/2,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Left",  "Left Half",  function() wm.sizeAndMove({  0,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Up",    "Upper Half", function() wm.sizeAndMove({  0,   0,   1, 1/2}, 5) end)
windowManager:bind('', "Down",  "Lower Half", function() wm.sizeAndMove({  0, 1/2,   1, 1/2}, 5) end)

-- Quarters
windowManager:bind("shift", "Up"   , "Upper Right", function() wm.sizeAndMove({1/2,   0, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Right", "Lower Right", function() wm.sizeAndMove({1/2, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Down",  "Lower Left",  function() wm.sizeAndMove({  0, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Left",  "Upper Left",  function() wm.sizeAndMove({  0,   0, 1/2, 1/2}, 5) end)

-- Thirds
windowManager:bind({"ctrl", "alt"}, "Left",  "Left Third",      function() wm.sizeAndMove({  0,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Up",    "Middle Third",    function() wm.sizeAndMove({1/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Right", "Right Third",     function() wm.sizeAndMove({2/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Down",  "Left Two Thirds", function() wm.sizeAndMove({  0,   0, 2/3, 1}, 5) end)

-- Sixths
windowManager:bind({"shift", "ctrl", "alt"}, "[",     "Upper 1st Third", function() wm.sizeAndMove({  0,   0, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Left",  "Lower 1st Third", function() wm.sizeAndMove({  0, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Up",    "Upper 2nd Third", function() wm.sizeAndMove({1/3,   0, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Down",  "Lower 2nd Third", function() wm.sizeAndMove({1/3, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Right", "Lower 3rd Third", function() wm.sizeAndMove({2/3, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "]",     "Upper 3rd Third", function() wm.sizeAndMove({2/3,   0, 1/3, 1/2}, 5) end)

-- Centers
windowManager:bind({"ctrl", "alt", "cmd"}, "Up",    "Max",      function() wm.sizeAndMove({0, 0, 1, 1}, 0) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Left",  "Center S", function() wm.sizeAndMove({1/2, 1/2, 1/2, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Down",  "Center M", function() wm.sizeAndMove({1/2, 1/2, 2/3, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Right", "Center L", function() wm.sizeAndMove({1/2, 1/2, 3/4, 3/4}, 0, true) end)
