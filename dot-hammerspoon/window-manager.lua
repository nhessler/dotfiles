-- Window management logic
-- Provides tiling functions for use with hotkey bindings

local M = {}

M.appExceptions = {"Authy Desktop", "System Preferences", "System Settings", "FaceTime", "OpenVPN Connect", "Unite", "Calculator"}

local function isAppException()
  local appName = hs.window.focusedWindow():application():name()

  for _, name in ipairs(M.appExceptions) do
    if appName == name then
      return true
    end
  end

  return false
end

function M.sizeAndMove(windowTile, padding, centerOffset)
  -- windowTile is (x, y, w, h) as a percent of screen
  local window       = hs.window.focusedWindow()
  local windowFrame  = window:frame()
  local screenFrame  = window:screen():frame()
  local expectedSize = {w=0, h=0}

  local xOffset = (centerOffset and (screenFrame.w * windowTile[3] / 2) or 0)
  local yOffset = (centerOffset and (screenFrame.h * windowTile[4] / 2) or 0)

  expectedSize.w = screenFrame.w * windowTile[3] - padding * 2
  expectedSize.h = screenFrame.h * windowTile[4] - padding * 2

  if isAppException() then
    local wOffset = (expectedSize.w - windowFrame.w) / 2
    local hOffset = (expectedSize.h - windowFrame.h) / 2

    windowFrame.x = screenFrame.x + screenFrame.w * windowTile[1] - xOffset + wOffset
    windowFrame.y = screenFrame.y + screenFrame.h * windowTile[2] - yOffset + hOffset
  else
    windowFrame.x = screenFrame.x + screenFrame.w * windowTile[1] - xOffset + padding
    windowFrame.y = screenFrame.y + screenFrame.h * windowTile[2] - yOffset + padding
    windowFrame.w = expectedSize.w
    windowFrame.h = expectedSize.h
  end

  window:setFrame(windowFrame)
end

return M
