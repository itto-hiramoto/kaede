import { createCommentStream, fetchCommentsSince, postComment } from "/assets/comments-client.js";

const MAX_AUTHOR_LENGTH = 32;
const MAX_MESSAGE_LENGTH = 280;
const MAX_VISIBLE_COMMENTS = 80;
const AUTHOR_STORAGE_KEY = "comment-server-author";

const elements = {
  form: document.getElementById("comment-form"),
  author: document.getElementById("author"),
  message: document.getElementById("message"),
  submitButton: document.getElementById("submit-button"),
  charCount: document.getElementById("char-count"),
  error: document.getElementById("form-error"),
  status: document.getElementById("connection-state"),
  list: document.getElementById("comment-list"),
  emptyState: document.getElementById("empty-state"),
};

const renderedIds = new Set();
let stream = null;

function setConnectionState(state) {
  const labels = {
    connecting: "接続中",
    reconnecting: "再接続中",
    open: "接続済み",
    closed: "切断",
    error: "エラー",
  };

  elements.status.dataset.state = state;
  elements.status.textContent = labels[state] ?? state;
}

function setError(message = "") {
  elements.error.textContent = message;
}

function updateCharCount() {
  elements.charCount.textContent = `${elements.message.value.length} / ${MAX_MESSAGE_LENGTH}`;
}

function toggleEmptyState() {
  const hasComments = elements.list.children.length > 0;
  elements.emptyState.hidden = hasComments;
}

function createCommentItem(comment) {
  const item = document.createElement("li");
  item.className = "comment-item";
  item.dataset.commentId = String(comment.id);

  const meta = document.createElement("div");
  meta.className = "comment-meta";

  const author = document.createElement("strong");
  author.className = "comment-author";
  author.textContent = comment.author || "匿名";

  const id = document.createElement("span");
  id.className = "comment-id";
  id.textContent = `#${comment.id}`;

  meta.append(author, id);

  const body = document.createElement("p");
  body.className = "comment-body";
  body.textContent = comment.message;

  item.append(meta, body);
  return item;
}

function appendComment(comment) {
  if (!comment || typeof comment.id !== "number" || renderedIds.has(comment.id)) {
    return;
  }

  renderedIds.add(comment.id);
  elements.list.append(createCommentItem(comment));

  while (elements.list.children.length > MAX_VISIBLE_COMMENTS) {
    const first = elements.list.firstElementChild;
    if (!first) {
      break;
    }

    const numericId = Number(first.dataset.commentId);
    if (Number.isFinite(numericId)) {
      renderedIds.delete(numericId);
    }
    first.remove();
  }

  toggleEmptyState();
  elements.list.lastElementChild?.scrollIntoView({ block: "nearest" });
}

function validateForm() {
  const author = elements.author.value.trim();
  const message = elements.message.value.trim();

  if (author.length > MAX_AUTHOR_LENGTH) {
    throw new Error("author too long");
  }

  if (message.length === 0) {
    throw new Error("message must not be empty");
  }

  if (message.length > MAX_MESSAGE_LENGTH) {
    throw new Error("message too long");
  }

  return {
    author,
    message,
  };
}

async function hydrateHistory() {
  const comments = await fetchCommentsSince(0);
  comments.forEach(appendComment);
  toggleEmptyState();
  if (comments.length === 0) {
    return 0;
  }
  return comments[comments.length - 1].id ?? 0;
}

async function handleSubmit(event) {
  event.preventDefault();
  setError("");

  let payload;
  try {
    payload = validateForm();
  } catch (error) {
    setError(error.message);
    return;
  }

  elements.submitButton.disabled = true;
  elements.submitButton.textContent = "送信中...";

  try {
    window.localStorage.setItem(AUTHOR_STORAGE_KEY, payload.author);
    await postComment(payload.author, payload.message);
    elements.message.value = "";
    updateCharCount();
    elements.message.focus();
  } catch (error) {
    setError(error.message);
  } finally {
    elements.submitButton.disabled = false;
    elements.submitButton.textContent = "送信する";
  }
}

async function boot() {
  setConnectionState("connecting");
  updateCharCount();
  toggleEmptyState();

  const savedAuthor = window.localStorage.getItem(AUTHOR_STORAGE_KEY);
  if (savedAuthor) {
    elements.author.value = savedAuthor.slice(0, MAX_AUTHOR_LENGTH);
  }

  let since = 0;
  try {
    since = await hydrateHistory();
  } catch (error) {
    setError(error.message);
  }

  stream = createCommentStream({
    since,
    onMessage: appendComment,
    onStateChange: setConnectionState,
  });
}

elements.form.addEventListener("submit", handleSubmit);
elements.message.addEventListener("input", updateCharCount);
window.addEventListener("beforeunload", () => stream?.close());

boot();
