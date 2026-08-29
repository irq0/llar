import { normalizeCaptureBase, validCaptureToken } from "./settings.mjs";

const form = document.getElementById("settings");
const captureBaseInput = document.getElementById("capture-base");
const tokenInput = document.getElementById("token");
const status = document.getElementById("status");

async function restore() {
  const { captureBase = "", token = "" } = await chrome.storage.local.get([
    "captureBase",
    "token",
  ]);
  captureBaseInput.value = captureBase;
  tokenInput.value = token;
}

form.addEventListener("submit", async (event) => {
  event.preventDefault();
  status.textContent = "";

  try {
    const captureBase = normalizeCaptureBase(captureBaseInput.value);
    const token = tokenInput.value.trim();
    if (!validCaptureToken(token)) {
      throw new Error(
        "Token must contain at least 32 letters, digits, underscores, or hyphens.",
      );
    }

    await chrome.storage.local.set({ captureBase, token });
    captureBaseInput.value = captureBase;
    status.textContent = "Settings saved.";
  } catch (error) {
    status.textContent = error.message;
  }
});

void restore();
