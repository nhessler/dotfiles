-- Wave Link WebSocket API integration
-- Controls mute state via Elgato Wave Link's local JSON-RPC API

local M = {}

M.port = 1824
M.ws = nil
M.msgId = 0
M.micIdentifier = nil
M.isMuted = false
M.pendingCallbacks = {}
M.reconnectTimer = nil

local function log(...)
  print("[WaveLink]", ...)
end

local function nextId()
  M.msgId = M.msgId + 1
  return M.msgId
end

local function send(method, params, callback)
  if not M.ws then
    hs.alert("Wave Link not connected")
    return
  end

  local id = nextId()
  if callback then
    M.pendingCallbacks[id] = callback
  end

  local msg = hs.json.encode({
    id = id,
    jsonrpc = "2.0",
    method = method,
    params = params or {}
  })

  M.ws:send(msg, false)
end

local function handleMessage(msg)
  local data = hs.json.decode(msg)
  if not data then return end

  -- Handle responses to our requests
  if data.id and M.pendingCallbacks[data.id] then
    if data.error then
      log("RPC ERROR:", hs.json.encode(data.error))
    else
      M.pendingCallbacks[data.id](data.result)
    end
    M.pendingCallbacks[data.id] = nil
    return
  end

  -- Track mic mute state from push events
  if data.method == "microphoneConfigChanged" and data.params then
    local p = data.params
    if p.property == "Microphone Mute" and p.identifier == M.micIdentifier then
      M.isMuted = p.value
    end
  end
end

local function fetchState()
  send("getMicrophoneConfig", {}, function(result)
    if not result then return end

    for _, mic in ipairs(result) do
      M.micIdentifier = mic.identifier
      M.isMuted = mic.isMicMuted
      log("Found mic:", mic.name, "muted:", tostring(M.isMuted))
      return
    end

    hs.alert("Wave XLR mic not found in Wave Link")
  end)
end

function M.connect()
  if M.ws then
    M.ws:close()
    M.ws = nil
  end

  local url = "ws://127.0.0.1:" .. M.port

  M.ws = hs.websocket.new(url, function(eventType, message)
    if eventType == "open" then
      log("Connected")
      if M.reconnectTimer then
        M.reconnectTimer:stop()
        M.reconnectTimer = nil
      end
      fetchState()
    elseif eventType == "received" then
      handleMessage(message)
    elseif eventType == "closed" or eventType == "fail" then
      M.ws = nil
      if not M.reconnectTimer then
        M.reconnectTimer = hs.timer.doEvery(10, function()
          M.connect()
        end)
      end
    end
  end)
end

function M.toggleMute()
  if not M.micIdentifier then
    hs.alert("Mic not found — is Wave Link running?")
    return
  end

  local newMuteState = not M.isMuted

  send("setMicrophoneConfig", {
    identifier = M.micIdentifier,
    property = "Microphone Mute",
    value = newMuteState
  })

  M.isMuted = newMuteState
  if newMuteState then
    hs.alert("🔇 Mic Muted")
  else
    hs.alert("🎙️ Mic Live")
  end
end

return M
