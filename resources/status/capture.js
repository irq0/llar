(function () {
  "use strict";

  const status = document.getElementById("capture-status");
  const detail = document.getElementById("capture-detail");
  const closeButton = document.getElementById("capture-close");
  const rawInput = window.location.hash.slice(1);
  const compactDelimiter = rawInput.indexOf(":http");
  const compactInput = compactDelimiter > 0;
  const input = compactInput ? null : new URLSearchParams(rawInput);
  const url = compactInput
    ? rawInput.slice(compactDelimiter + 1)
    : input.get("url");
  const title = compactInput ? null : input.get("title");
  const token = compactInput
    ? rawInput.slice(0, compactDelimiter)
    : input.get("token");

  history.replaceState(
    null,
    "",
    window.location.pathname + window.location.search,
  );

  function finish(ok, message, extra) {
    document.body.classList.add(ok ? "success" : "failure");
    status.textContent = message;
    detail.textContent = extra || "";
    closeButton.hidden = false;
    if (ok && window.opener) {
      window.setTimeout(function () {
        window.close();
      }, 1200);
    }
  }

  closeButton.addEventListener("click", function () {
    if (window.opener) {
      window.close();
    } else {
      history.back();
    }
  });

  if (!url || !token) {
    finish(false, "Nothing was saved.", "The capture URL or token is missing.");
    return;
  }

  fetch("api/v1/captures", {
    method: "POST",
    credentials: "omit",
    cache: "no-store",
    headers: {
      Authorization: "Bearer " + token,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ url: url, title: title || null }),
  })
    .then(async function (response) {
      const body = await response.json().catch(function () {
        return {};
      });
      if (!response.ok) {
        throw new Error(
          body.message || "Capture failed (HTTP " + response.status + ")",
        );
      }
      finish(
        true,
        body.message || "Saved to Llar.",
        "You can close this window.",
      );
    })
    .catch(function (error) {
      finish(
        false,
        "Llar did not confirm the save.",
        error.message + " Try again later.",
      );
    });
})();
