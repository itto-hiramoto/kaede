import { createCommentStream, fetchCommentsSince } from "/assets/comments-client.js";

const SPEED_PX_PER_SEC = 160;
const LANE_HEIGHT = 56;
const MIN_LANES = 4;
const MAX_QUEUE_LENGTH = 50;

const stage = document.getElementById("screen-stage");
const status = document.getElementById("screen-status");
const measureBox = document.getElementById("measure-box");

const queue = [];
const activeLanes = new Map();
let laneCount = MIN_LANES;
let stream = null;

function setConnectionState(state) {
  const labels = {
    connecting: "接続中",
    reconnecting: "再接続中",
    open: "LIVE",
    closed: "切断",
    error: "エラー",
  };

  status.dataset.state = state;
  status.textContent = labels[state] ?? state;
}

function updateLaneCount() {
  const usableHeight = Math.max(window.innerHeight - 96, LANE_HEIGHT * MIN_LANES);
  laneCount = Math.max(MIN_LANES, Math.floor(usableHeight / LANE_HEIGHT));
}

function nextFreeLane() {
  for (let lane = 0; lane < laneCount; lane += 1) {
    if (!activeLanes.has(lane)) {
      return lane;
    }
  }
  return null;
}

function measureComment(node) {
  const clone = node.cloneNode(true);
  measureBox.replaceChildren(clone);
  return Math.ceil(clone.getBoundingClientRect().width);
}

function createFloatingComment(comment) {
  const item = document.createElement("article");
  item.className = "floating-comment";

  const author = document.createElement("span");
  author.className = "floating-comment__author";
  author.textContent = comment.author || "anonymous";

  const body = document.createElement("span");
  body.className = "floating-comment__body";
  body.textContent = comment.message.replace(/\s+/g, " ").trim();

  item.append(author, body);
  return item;
}

function launchComment(comment, lane) {
  const item = createFloatingComment(comment);
  const width = measureComment(item);
  const travel = width + window.innerWidth + 192;
  const duration = travel / SPEED_PX_PER_SEC;
  const hue = (comment.id * 37) % 360;

  item.style.top = `${32 + lane * LANE_HEIGHT}px`;
  item.style.left = `${window.innerWidth + 48}px`;
  item.style.setProperty("--travel", `${travel}px`);
  item.style.setProperty("--accent", `hsl(${hue} 90% 68%)`);
  item.style.animationDuration = `${duration}s`;

  activeLanes.set(lane, item);
  stage.append(item);

  item.addEventListener(
    "animationend",
    () => {
      activeLanes.delete(lane);
      item.remove();
      drainQueue();
    },
    { once: true },
  );
}

function drainQueue() {
  while (queue.length > 0) {
    const lane = nextFreeLane();
    if (lane === null) {
      return;
    }

    const comment = queue.shift();
    launchComment(comment, lane);
  }
}

function enqueueComment(comment) {
  if (!comment || typeof comment.id !== "number") {
    return;
  }

  queue.push(comment);
  if (queue.length > MAX_QUEUE_LENGTH) {
    queue.splice(0, queue.length - MAX_QUEUE_LENGTH);
  }
  drainQueue();
}

async function boot() {
  updateLaneCount();
  setConnectionState("connecting");

  let since = 0;
  try {
    const history = await fetchCommentsSince(0);
    if (history.length > 0) {
      since = history[history.length - 1].id ?? 0;
    }
  } catch (error) {
    console.error("failed to prime latest comment id", error);
  }

  stream = createCommentStream({
    since,
    onMessage: enqueueComment,
    onStateChange: setConnectionState,
  });
}

window.addEventListener("resize", () => {
  updateLaneCount();
  drainQueue();
});

window.addEventListener("beforeunload", () => stream?.close());

boot();
