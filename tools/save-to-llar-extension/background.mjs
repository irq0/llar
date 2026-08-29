import { makeFeedbackUrl } from "./settings.mjs";

void chrome.storage.local.setAccessLevel({ accessLevel: "TRUSTED_CONTEXTS" });

async function showError(tabId, error) {
  console.error("Save to LLAR failed", error);
  await chrome.action.setBadgeBackgroundColor({ tabId, color: "#b42318" });
  await chrome.action.setBadgeText({ tabId, text: "!" });
  await chrome.action.setTitle({
    tabId,
    title: "🖖 Save to LLAR: " + error.message,
  });
}

chrome.action.onClicked.addListener(async (tab) => {
  try {
    const { captureBase, token } = await chrome.storage.local.get([
      "captureBase",
      "token",
    ]);

    if (!captureBase || !token) {
      await chrome.runtime.openOptionsPage();
      return;
    }

    const feedbackUrl = makeFeedbackUrl(captureBase, token, tab.url || "");
    await chrome.action.setBadgeText({ tabId: tab.id, text: "" });
    await chrome.action.setTitle({ tabId: tab.id, title: "🖖 Save to LLAR" });
    await chrome.tabs.update(tab.id, { url: feedbackUrl });
  } catch (error) {
    await showError(tab.id, error);
  }
});
