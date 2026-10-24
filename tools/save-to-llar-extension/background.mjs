import {
  getExtensionApi,
  restrictStorageToExtension,
} from "./extension-api.mjs";
import { makeFeedbackUrl } from "./settings.mjs";

const extensionApi = getExtensionApi();

void restrictStorageToExtension(extensionApi).catch((error) => {
  console.warn("Could not restrict extension storage access", error);
});

async function showError(tabId, error) {
  console.error("Save to LLAR failed", error);
  await extensionApi.action.setBadgeBackgroundColor({
    tabId,
    color: "#b42318",
  });
  await extensionApi.action.setBadgeText({ tabId, text: "!" });
  await extensionApi.action.setTitle({
    tabId,
    title: "🖖 Save to LLAR: " + error.message,
  });
}

extensionApi.action.onClicked.addListener(async (tab) => {
  try {
    const { captureBase, token } = await extensionApi.storage.local.get([
      "captureBase",
      "token",
    ]);

    if (!captureBase || !token) {
      await extensionApi.runtime.openOptionsPage();
      return;
    }

    const feedbackUrl = makeFeedbackUrl(captureBase, token, tab.url || "");
    await extensionApi.action.setBadgeText({ tabId: tab.id, text: "" });
    await extensionApi.action.setTitle({
      tabId: tab.id,
      title: "🖖 Save to LLAR",
    });
    await extensionApi.tabs.update(tab.id, { url: feedbackUrl });
  } catch (error) {
    await showError(tab.id, error);
  }
});
