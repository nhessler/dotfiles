-- Wave Link WebSocket API integration
-- Controls mute state via Elgato Wave Link's local JSON-RPC API

local M = {}

M.port = nil
M.ws = nil
M.msgId = 0
M.micIdentifier = nil
M.isMuted = false
M.pendingCallbacks = {}

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

M._onReady = nil

local function fetchState()
  send("getMicrophoneConfig", {}, function(result)
    if not result then return end

    for _, mic in ipairs(result) do
      M.micIdentifier = mic.identifier
      M.isMuted = mic.isMicMuted
      log("Found mic:", mic.name, "muted:", tostring(M.isMuted))
      if M._onReady then
        M._onReady()
        M._onReady = nil
      end
      return
    end

    hs.alert("Wave XLR mic not found in Wave Link")
  end)
end

local function discoverPort()
  local output = hs.execute("lsof -i -P -n 2>/dev/null | grep WaveLink | grep LISTEN | grep '\\*:'")
  if output then
    local port = output:match("%*:(%d+)")
    if port then
      M.port = tonumber(port)
      log("Discovered port:", M.port)
      return true
    end
  end
  log("Could not discover Wave Link port")
  return false
end

function M.connect()
  if M.ws then
    M.ws:close()
    M.ws = nil
  end

  if not M.port then
    if not discoverPort() then return false end
  end

  local url = "ws://127.0.0.1:" .. M.port

  M.ws = hs.websocket.new(url, function(eventType, message)
    if eventType == "open" then
      log("Connected")
      fetchState()
    elseif eventType == "received" then
      handleMessage(message)
    elseif eventType == "closed" or eventType == "fail" then
      M.ws = nil
      M.port = nil
      M.micIdentifier = nil
    end
  end)

  return true
end

local function setMute(muted)
  send("setMicrophoneConfig", {
    identifier = M.micIdentifier,
    property = "Microphone Mute",
    value = muted
  })
  M.isMuted = muted
end

local function withMic(fn)
  if M.ws and M.micIdentifier then
    fn()
    return
  end

  if not M.ws then
    if not M.connect() then
      log("Wave Link not running")
      return
    end
  end

  M._onReady = fn
end

function M.mute()
  withMic(function()
    setMute(true)
    log("Muted")
  end)
end

function M.unmute()
  withMic(function()
    setMute(false)
    log("Unmuted")
  end)
end

function M.toggleMute()
  withMic(function()
    setMute(not M.isMuted)
    if M.isMuted then
      hs.alert("🔇 Mic Muted")
    else
      hs.alert("🎙️ Mic Live")
    end
  end)
end

return M
