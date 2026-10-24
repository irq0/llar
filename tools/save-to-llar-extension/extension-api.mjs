export function getExtensionApi(scope = globalThis) {
  const extensionApi = scope.browser ?? scope.chrome;
  if (!extensionApi) {
    throw new Error("The WebExtensions API is unavailable.");
  }
  return extensionApi;
}

export async function restrictStorageToExtension(extensionApi) {
  const setAccessLevel = extensionApi.storage?.local?.setAccessLevel;
  if (typeof setAccessLevel === "function") {
    await setAccessLevel.call(extensionApi.storage.local, {
      accessLevel: "TRUSTED_CONTEXTS",
    });
  }
}
