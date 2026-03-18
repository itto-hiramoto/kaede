const RETRY_DELAYS_MS = [1000, 2000, 5000];

function websocketUrl(path) {
  const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
  return `${protocol}//${window.location.host}${path}`;
}

async function readError(response) {
  const text = await response.text();
  if (text.trim().length > 0) {
    return text.trim();
  }
  return `${response.status} ${response.statusText}`;
}

export function parseNdjson(text) {
  return text
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line) => JSON.parse(line));
}

export async function fetchCommentsSince(since = 0) {
  const response = await fetch(`/comments?since=${encodeURIComponent(String(since))}`, {
    headers: { Accept: "text/plain" },
  });

  if (!response.ok) {
    throw new Error(await readError(response));
  }

  const text = await response.text();
  return parseNdjson(text);
}

export async function postComment(author, message) {
  const authorParam = encodeURIComponent(author);
  const response = await fetch(`/comments?author=${authorParam}`, {
    method: "POST",
    headers: { "Content-Type": "text/plain; charset=utf-8" },
    body: message,
  });

  if (!response.ok) {
    throw new Error(await readError(response));
  }

  return response.text();
}

export function createCommentStream({ since = 0, onMessage, onStateChange }) {
  let closed = false;
  let lastSeen = since;
  let reconnectAttempt = 0;
  let reconnectTimer = 0;
  let socket = null;

  const emitState = (state) => {
    if (typeof onStateChange === "function") {
      onStateChange(state);
    }
  };

  const scheduleReconnect = () => {
    if (closed) {
      return;
    }

    const delay = RETRY_DELAYS_MS[Math.min(reconnectAttempt, RETRY_DELAYS_MS.length - 1)];
    reconnectAttempt += 1;
    reconnectTimer = window.setTimeout(connect, delay);
  };

  const connect = () => {
    if (closed) {
      return;
    }

    const phase = reconnectAttempt === 0 ? "connecting" : "reconnecting";
    emitState(phase);

    socket = new WebSocket(
      websocketUrl(`/ws/comments?since=${encodeURIComponent(String(lastSeen))}`),
    );

    socket.addEventListener("open", () => {
      reconnectAttempt = 0;
      emitState("open");
    });

    socket.addEventListener("message", (event) => {
      try {
        const comment = JSON.parse(event.data);
        if (comment && typeof comment.id === "number" && comment.id > lastSeen) {
          lastSeen = comment.id;
        }
        if (typeof onMessage === "function") {
          onMessage(comment);
        }
      } catch (error) {
        console.error("failed to parse comment frame", error);
        emitState("error");
      }
    });

    socket.addEventListener("error", () => {
      emitState("error");
    });

    socket.addEventListener("close", () => {
      if (closed) {
        emitState("closed");
        return;
      }

      emitState("closed");
      scheduleReconnect();
    });
  };

  connect();

  return {
    close() {
      closed = true;
      if (reconnectTimer !== 0) {
        window.clearTimeout(reconnectTimer);
      }
      if (socket && socket.readyState < WebSocket.CLOSING) {
        socket.close();
      }
    },
    getLastSeen() {
      return lastSeen;
    },
    setLastSeen(nextSince) {
      if (Number.isFinite(nextSince) && nextSince > lastSeen) {
        lastSeen = nextSince;
      }
    },
  };
}
