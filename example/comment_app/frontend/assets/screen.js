import { createCommentStream, fetchCommentsSince } from "/assets/comments-client.js";

const DEFAULT_SLIDES_PATH = "/slides/slide.html";
const SPEED_PX_PER_SEC = 160;
const LANE_HEIGHT = 56;
const MIN_LANES = 4;
const MAX_QUEUE_LENGTH = 50;
const SLIDE_NAV_KEYS = new Set([
  "ArrowLeft",
  "ArrowRight",
  "ArrowUp",
  "ArrowDown",
  "PageUp",
  "PageDown",
  "Home",
  "End",
  " ",
]);

const slideRoot = document.getElementById("screen-slides");
const slideFrame = document.getElementById("screen-slides-frame");
const stage = document.getElementById("screen-stage");
const status = document.getElementById("screen-status");
const measureBox = document.getElementById("measure-box");

const queue = [];
const activeLanes = new Map();
let laneCount = MIN_LANES;
let stream = null;

function slideSourceUrl() {
  const params = new URLSearchParams(window.location.search);
  const value = params.get("slides");
  if (value === "off") {
    return null;
  }
  return value || DEFAULT_SLIDES_PATH;
}

function hideSlideChrome() {
  try {
    const doc = slideFrame.contentDocument;
    if (!doc) {
      return;
    }

    if (doc.getElementById("screen-slide-style")) {
      return;
    }

    const style = doc.createElement("style");
    style.id = "screen-slide-style";
    style.textContent = `
      .bespoke-marp-osc {
        display: none !important;
      }

      body {
        cursor: default !important;
      }

      [data-bespoke-view=""] {
        overflow: hidden !important;
      }
    `;
    doc.head.append(style);
  } catch (error) {
    console.warn("failed to hide slide chrome", error);
  }
}

function forwardSlideKey(event) {
  if (slideRoot.hidden || !slideFrame.contentWindow || !SLIDE_NAV_KEYS.has(event.key)) {
    return;
  }

  try {
    const targetWindow = slideFrame.contentWindow;
    const init = {
      key: event.key,
      code: event.code,
      altKey: event.altKey,
      ctrlKey: event.ctrlKey,
      metaKey: event.metaKey,
      shiftKey: event.shiftKey,
      repeat: event.repeat,
      bubbles: true,
      cancelable: true,
    };
    const forwarded = new targetWindow.KeyboardEvent("keydown", init);
    targetWindow.dispatchEvent(forwarded);
    targetWindow.document.dispatchEvent(new targetWindow.KeyboardEvent("keydown", init));
    targetWindow.document.body?.dispatchEvent(new targetWindow.KeyboardEvent("keydown", init));
    targetWindow.focus();
    event.preventDefault();
  } catch (error) {
    console.warn("failed to forward slide key", error);
  }
}

function handleScreenKeydown(event) {
  forwardSlideKey(event);
}

function mountSlideFrame(url) {
  slideRoot.hidden = false;
  document.body.classList.add("screen-page--with-slides");
  slideFrame.src = url;
  slideFrame.addEventListener(
    "load",
    () => {
      hideSlideChrome();
    },
    { once: true },
  );
}

function showSlideError(error) {
  slideRoot.hidden = false;
  slideRoot.innerHTML = `
    <div class="screen-slides__error">
      <div>
        <p>スライドの読み込みに失敗しました。</p>
        <p>${String(error)}</p>
      </div>
    </div>
  `;
}

function bootSlides() {
  const slidesUrl = slideSourceUrl();
  if (!slidesUrl) {
    slideRoot.hidden = true;
    return;
  }

  try {
    mountSlideFrame(slidesUrl);
    window.addEventListener("keydown", handleScreenKeydown);
  } catch (error) {
    console.error("failed to initialize slides", error);
    showSlideError(error);
  }
}

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
  bootSlides();
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
