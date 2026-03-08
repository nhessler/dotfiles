-- Studio lights control
-- Toggles Nanoleaf lights via Apple Shortcuts

local M = {}

M.isOn = false

local function log(...)
  print("[StudioLights]", ...)
end

local function getState()
  local output = hs.execute('shortcuts run "Studio Lights State"')
  if output then
    local state = output:match("^%s*(.-)%s*$") -- trim whitespace
    M.isOn = (state == "Yes")
    log("State:", state, "isOn:", tostring(M.isOn))
  end
end

function M.on()
  hs.shortcuts.run("Studio Lights On")
  M.isOn = true
  log("Lights ON")
end

function M.off()
  hs.shortcuts.run("Studio Lights Off")
  M.isOn = false
  log("Lights OFF")
end

function M.toggle()
  getState()
  if M.isOn then
    M.off()
    hs.alert("💡 Studio Lights Off")
  else
    M.on()
    hs.alert("💡 Studio Lights On")
  end
end

return M
