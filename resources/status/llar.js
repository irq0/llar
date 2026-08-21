//
// utilities
//

// Images come from feeds and remote metadata, so a broken URL should remove
// only its preview rather than leave a broken-image control in the interface.
document.addEventListener(
  "error",
  function (event) {
    var image = event.target;
    if (
      !(image instanceof HTMLImageElement) ||
      !image.classList.contains("reader-defensive-image")
    ) {
      return;
    }

    var revealSelector = image.getAttribute("data-error-reveal");
    var revealContainer = revealSelector
      ? image.closest(".reader-gallery-media")
      : null;
    var revealTarget = revealContainer
      ? revealContainer.querySelector(revealSelector)
      : null;
    if (revealTarget) revealTarget.removeAttribute("hidden");

    var selector = image.getAttribute("data-error-remove");
    var target = selector ? image.closest(selector) : image;
    if (target) target.remove();
  },
  true,
);

function enhanceReaderImages() {
  var container = document.getElementById("item-content-body");
  if (!container) return;

  container.querySelectorAll("img").forEach(function (image) {
    if (image.closest("a") || image.hasAttribute("usemap")) return;

    var source = image.currentSrc || image.getAttribute("src");
    if (!source) return;

    var target = image.closest("picture") || image;
    var link = document.createElement("a");
    var description = image.getAttribute("alt");
    link.className = "reader-image-enlarge";
    link.href = source;
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.setAttribute(
      "aria-label",
      description
        ? "Enlarge image: " + description
        : "Enlarge image in new tab",
    );
    target.parentNode.insertBefore(link, target);
    link.appendChild(target);
  });
}

$(enhanceReaderImages);

function getReadingBlocks(container) {
  if (!container) return [];
  var candidates = Array.from(
    container.querySelectorAll(
      "h1, h2, h3, h4, h5, h6, p, pre, li, blockquote, img, iframe, div",
    ),
  ).filter(function (element) {
    return (
      element.getClientRects().length > 0 &&
      (element.tagName !== "DIV" || element.childElementCount === 0)
    );
  });

  // Prefer the most specific readable block. For example, a paragraph inside
  // a list item is one landing target rather than two nested targets.
  return candidates.filter(function (element) {
    return !candidates.some(function (other) {
      return other !== element && element.contains(other);
    });
  });
}

// Browsers normally restore scroll position when navigating back, but a
// Reader list can change while an item is open (most notably when opening it
// removes it from an unread view). Keep a few semantic anchors so we can
// repair an apparent reset without replacing native history restoration.
var readerListPositionStoragePrefix = "llar-reader-list-position:";

function readerListPositionStorageKey() {
  return readerListPositionStoragePrefix + window.location.href;
}

function readerListItems() {
  return Array.from(
    document.querySelectorAll(
      'body.reader-mode-list-items [id^="item-"][data-id]',
    ),
  );
}

function saveReaderListPosition() {
  var items = readerListItems();
  var firstVisibleIndex = items.findIndex(function (item) {
    return item.getBoundingClientRect().bottom > 0;
  });
  if (firstVisibleIndex < 0) return;

  var anchors = items
    .slice(firstVisibleIndex, firstVisibleIndex + 4)
    .map(function (item) {
      return { id: item.id, top: item.getBoundingClientRect().top };
    });

  try {
    window.sessionStorage.setItem(
      readerListPositionStorageKey(),
      JSON.stringify({ anchors: anchors, scrollY: window.scrollY }),
    );
  } catch (_error) {
    // Storage can be disabled without making Reader navigation unusable.
  }
}

function restoreReaderListPosition() {
  // A non-zero offset means the browser already restored this history entry.
  if (window.scrollY >= 10) return;

  var saved;
  try {
    saved = JSON.parse(
      window.sessionStorage.getItem(readerListPositionStorageKey()),
    );
  } catch (_error) {
    return;
  }
  if (!saved) return;

  var anchor = (saved.anchors || []).find(function (candidate) {
    return document.getElementById(candidate.id);
  });
  if (anchor) {
    var item = document.getElementById(anchor.id);
    window.scrollBy(0, item.getBoundingClientRect().top - anchor.top);
  } else if (Number.isFinite(saved.scrollY)) {
    window.scrollTo(0, saved.scrollY);
  }
}

$(function () {
  if (!document.body.classList.contains("reader-mode-list-items")) return;

  window.addEventListener("pagehide", saveReaderListPosition);
  window.requestAnimationFrame(restoreReaderListPosition);
});

window.addEventListener("pageshow", function () {
  if (!document.body.classList.contains("reader-mode-list-items")) return;
  window.requestAnimationFrame(restoreReaderListPosition);
});

function updateStateButton(button, isSet) {
  var icon = button.find("i");
  var visibleLabel = button.find(".reader-action-label");
  var label = isSet ? button.data("label-set") : button.data("label-unset");
  button.data("is-set", isSet);
  button.attr("data-is-set", String(isSet));
  button.attr("title", label);
  button.attr("aria-label", label);
  icon.attr(
    "class",
    isSet ? button.data("icon-set") : button.data("icon-unset"),
  );
  visibleLabel.text(label);
}

function addItemTagRow(id, tag) {
  var list = $("#add-custom-tag-" + id + " .item-tag-list");
  if (!list.length) return;
  var exists = list.find(".item-tag-row").filter(function () {
    return String($(this).data("tag")) === String(tag);
  }).length;
  if (exists) return;

  var label = "Toggle tag " + tag;
  var button = $("<a>")
    .addClass("btn reader-icon-button item-tag-toggle")
    .attr({
      href: "#",
      title: label,
      "aria-label": label,
      "data-id": id,
      "data-icon-set": "fas fa-check-circle icon-is-set",
      "data-icon-unset": "far fa-circle",
      "data-tag": tag,
      "data-label-set": label,
      "data-label-unset": label,
      "data-is-set": "true",
    })
    .data("is-set", true)
    .append($("<i>").addClass("fas fa-check-circle icon-is-set"));
  $("<li>")
    .addClass("list-group-item item-tag-row")
    .attr({ "data-id": id, "data-tag": tag })
    .data({ id: id, tag: tag })
    .append(button, document.createTextNode("\u00a0" + tag))
    .appendTo(list);
}

function applyItemState(state) {
  var id = state.id;
  $(".state-toggle").each(function () {
    var button = $(this);
    if (String(button.data("id")) === String(id)) {
      updateStateButton(button, !!state[String(button.data("state"))]);
    }
  });

  var itemTags = state["item-tags"] || [];
  $(".item-tag-toggle").each(function () {
    var button = $(this);
    if (String(button.data("id")) === String(id)) {
      updateStateButton(button, itemTags.includes(String(button.data("tag"))));
    }
  });
  $(".item-tags-summary").each(function () {
    if (String($(this).data("id")) === String(id)) {
      $(this).text(itemTags.join(", "));
    }
  });
  $(".item-tag-row").each(function () {
    var row = $(this);
    if (
      String(row.data("id")) === String(id) &&
      !itemTags.includes(String(row.data("tag")))
    ) {
      row.remove();
    }
  });
  itemTags.forEach(function (tag) {
    addItemTagRow(id, tag);
  });
  $("#item-" + id)
    .data("unread", !!state.unread)
    .attr("data-unread", state.unread ? "true" : "false")
    .data("item-tags", itemTags)
    .attr("data-item-tags", JSON.stringify(itemTags));
  updateCheckpointControls(state);
}

function updateContinueReadingAfterState(state) {
  var item = $("#item-" + state.id + ".reader-continue-item");
  if (!item.length || state.checkpoint) return;
  item.remove();

  var count = $(".reader-continue-item").length;
  $(".reader-continue-status").text(
    count + " saved place" + (count === 1 ? "." : "s."),
  );
  $(".reader-continue-empty").prop("hidden", count !== 0);
}

function requestItemState(id, action, data) {
  return $.post(
    "/reader/item/by-id/" + id + "/state",
    Object.assign({ action: action }, data || {}),
  ).done(function (state) {
    applyItemState(state);
    updateContinueReadingAfterState(state);
  });
}

var readerStatusReturnFocus = null;

function clearReaderStatus() {
  $("#reader-global-status")
    .prop("hidden", true)
    .removeAttr("data-state")
    .find(".reader-global-status-message")
    .empty();
  readerStatusReturnFocus = null;
}

function dismissReaderStatus() {
  var returnFocus = readerStatusReturnFocus;
  clearReaderStatus();
  if (returnFocus && returnFocus.isConnected) returnFocus.focus();
}

function showReaderError(message, context, xhr) {
  var status = xhr && xhr.status;
  console.error("[" + context + "] request failed:", status || "unknown");
  $("#reader-global-status")
    .attr("data-state", "error")
    .prop("hidden", false)
    .find(".reader-global-status-message")
    .text(message);
}

function setStateControlPending(control, pending) {
  if (pending && control.length) readerStatusReturnFocus = control[0];
  control
    .data("state-request-pending", pending)
    .toggleClass("disabled", pending)
    .attr("aria-disabled", pending ? "true" : null)
    .attr("aria-busy", pending ? "true" : null);
  if (control.is("button, input")) control.prop("disabled", pending);
}

function makeAnimatedDots(extraClass) {
  var dots = $("<span>")
    .addClass("reader-lifecycle-indicator reader-lifecycle-dots")
    .attr("aria-hidden", "true");
  if (extraClass) dots.addClass(extraClass);
  for (var i = 0; i < 3; i += 1) {
    $("<span>").addClass("reader-lifecycle-dot").appendTo(dots);
  }
  return dots;
}

function addListLifecycleIndicator(root, indicator) {
  if (indicator === "dots") {
    makeAnimatedDots().appendTo(root);
  } else if (indicator === "sync") {
    $("<i>")
      .addClass(
        "fas fa-sync-alt reader-lifecycle-indicator reader-lifecycle-sync",
      )
      .attr("aria-hidden", "true")
      .appendTo(root);
  }
}

function showListLifecycleStatus(message, state, action, indicator) {
  var root = $("#reader-list-lifecycle-status");
  if (!root.length) return;

  root
    .empty()
    .attr("data-state", state || "info")
    .prop("hidden", false);
  addListLifecycleIndicator(root, indicator);
  $("<span>").text(message).appendTo(root);
  if (!action) return;

  var control = action.href
    ? $("<a>").attr("href", action.href)
    : $("<button>").attr("type", "button");
  control.text(action.label).appendTo(root);
  if (action.className) control.addClass(action.className);
  if (action.onClick) control.on("click", action.onClick);
}

function setSourceUpdatePending(pending) {
  var buttons = $(".btn-update-sources-in-view");
  buttons
    .data("source-update-pending", pending)
    .removeClass("active")
    .toggleClass("disabled", pending)
    .toggleClass("source-update-pending", pending)
    .attr("aria-disabled", pending ? "true" : null)
    .each(function () {
      var button = $(this);
      var title = pending
        ? button.data("pending-title")
        : button.data("idle-title");
      button.attr("title", title).attr("aria-label", title);
      button.find(".reader-source-update-dots").remove();
      button.find("i").prop("hidden", pending);
      if (pending) {
        makeAnimatedDots("reader-source-update-dots").appendTo(button);
        if (document.activeElement === this) this.blur();
      }
    });
}

function setSourceUpdateReady(itemsUrl) {
  $(".btn-update-sources-in-view")
    .data("source-update-ready", true)
    .addClass("source-update-ready")
    .attr("href", itemsUrl)
    .attr("title", "Open updated snapshot")
    .attr("aria-label", "Open updated snapshot")
    .find("i")
    .prop("hidden", false)
    .removeClass("fa-download")
    .addClass("fa-sync-alt icon-is-set");
}

function sourceUpdateFailures(result) {
  var failureStatuses = ["temp-fail", "perm-fail", "bug"];
  return (Array.isArray(result) ? result : []).filter(function (outcome) {
    return (
      outcome.outcome === "error" || failureStatuses.includes(outcome.status)
    );
  }).length;
}

function sourceUpdateCompletions(result) {
  return (Array.isArray(result) ? result : []).filter(function (outcome) {
    return outcome.outcome === "completed" && outcome.status === "ok";
  }).length;
}

function update_sources_update_state(target, attempts) {
  attempts = attempts || 0;
  $.getJSON(target)
    .done(function (result) {
      if (!result || !result.done) {
        if (attempts >= 150) {
          setSourceUpdatePending(false);
          showListLifecycleStatus(
            "The source check is taking longer than expected. This snapshot is unchanged.",
            "error",
          );
          return;
        }
        window.setTimeout(
          update_sources_update_state,
          2000,
          target,
          attempts + 1,
        );
        return;
      }

      setSourceUpdatePending(false);
      var outcomes = Array.isArray(result.result) ? result.result : [];
      var failures = sourceUpdateFailures(outcomes);
      var completions = sourceUpdateCompletions(outcomes);
      var itemsUrl = $(".btn-update-sources-in-view").first().data("items");
      var message;
      if (!outcomes.length) {
        message = "No fetchable sources are present in this view.";
      } else if (failures) {
        message =
          "Source check finished with " +
          failures +
          (failures === 1 ? " failure." : " failures.") +
          (completions ? "" : " No updated snapshot is available.");
      } else if (!completions) {
        message = "Source check finished; no sources were updated.";
      } else {
        message = "Source check complete.";
      }
      if (completions) setSourceUpdateReady(itemsUrl);
      showListLifecycleStatus(
        message,
        failures ? "error" : "success",
        null,
        "sync",
      );
    })
    .fail(function () {
      setSourceUpdatePending(false);
      showListLifecycleStatus(
        "The source-check status could not be retrieved. This snapshot is unchanged.",
        "error",
      );
    });
}

//
// tagging
//

function bookmark_capture_error_message(request) {
  var response = request && request.responseJSON;
  if (response && typeof response.error === "string" && response.error.trim()) {
    return response.error.trim();
  }
  if (
    response &&
    typeof response.message === "string" &&
    response.message.trim()
  ) {
    return response.message.trim();
  }

  var responseText =
    request && typeof request.responseText === "string"
      ? request.responseText.trim()
      : "";
  if (responseText) {
    try {
      var parsed = JSON.parse(responseText);
      if (typeof parsed.error === "string" && parsed.error.trim()) {
        return parsed.error.trim();
      }
    } catch (_error) {
      // A proxy may return an HTML error page; do not expose it in the popover.
    }
  }

  if (request && request.status === 0) {
    return "Could not reach Llar. Check the connection and try again.";
  }
  return "Llar could not save this URL. Please try again.";
}

var bookmarkActivityRefreshTimer = null;

function bookmarkActivityCount(activity, key) {
  var count = Number(activity && activity[key]);
  return Number.isFinite(count) && count > 0 ? count : 0;
}

function bookmarkActivityQueueLink() {
  return $("<a>")
    .attr("href", "/reader/tools/saved-overview")
    .text("Reading Queue");
}

function renderBookmarkActivity(activity) {
  var root = $("#reader-bookmark-activity");
  if (!root.length) return;

  var activeCount = bookmarkActivityCount(activity, "active-count");
  var readyItems = Array.isArray(activity && activity["recent-ready"])
    ? activity["recent-ready"].slice(0, 3)
    : [];
  var overflowCount = bookmarkActivityCount(activity, "ready-overflow-count");
  var failedCount = bookmarkActivityCount(activity, "recent-failed-count");

  root.empty().attr("data-active-count", activeCount);

  if (activeCount) {
    var pendingLabel =
      activeCount +
      (activeCount === 1 ? " bookmark is" : " bookmarks are") +
      " being prepared; usually ready within a few minutes";
    $("<span>")
      .addClass("reader-bookmark-activity-pending")
      .attr({ title: pendingLabel, "aria-label": pendingLabel })
      .append(
        makeAnimatedDots(),
        $("<span>").text(activeCount + " preparing for "),
        bookmarkActivityQueueLink(),
      )
      .appendTo(root);
  }

  if (readyItems.length) {
    var ready = $("<span>")
      .addClass("reader-bookmark-activity-ready")
      .attr("aria-label", "Bookmarks prepared in the last hour")
      .append(
        $("<span>")
          .addClass("visually-hidden")
          .text("Recently prepared bookmarks: "),
      );
    readyItems.forEach(function (item) {
      var label = item.label || "Saved bookmark";
      $("<a>")
        .addClass("reader-bookmark-ready-link")
        .attr({
          href: item.href || "/reader/item/by-id/" + item["item-id"],
          title: "Ready: " + label,
          "aria-label": "Open ready bookmark: " + label,
          "data-item-id": item["item-id"],
        })
        .append(
          $("<i>").addClass("fas fa-newspaper").attr("aria-hidden", "true"),
        )
        .appendTo(ready);
    });
    if (overflowCount) {
      $("<a>")
        .addClass("reader-bookmark-ready-overflow")
        .attr({
          href: "/reader/tools/saved-overview",
          title:
            overflowCount + " more bookmarks were prepared in the last hour",
          "aria-label":
            "Open Reading Queue for " +
            overflowCount +
            " more prepared bookmarks",
        })
        .text("+" + overflowCount)
        .appendTo(ready);
    }
    ready.appendTo(root);
  }

  if (failedCount) {
    var failureLabel =
      failedCount +
      (failedCount === 1
        ? " bookmark could not be prepared in the last hour"
        : " bookmarks could not be prepared in the last hour");
    $("<span>")
      .addClass("reader-bookmark-activity-failed")
      .attr({ role: "img", title: failureLabel, "aria-label": failureLabel })
      .append(
        $("<i>")
          .addClass("fas fa-exclamation-triangle")
          .attr("aria-hidden", "true"),
      )
      .appendTo(root);
  }

  root.prop("hidden", !activeCount && !readyItems.length && !failedCount);
}

function showBookmarkActivityError(message) {
  var root = $("#reader-bookmark-activity");
  if (!root.length) return;
  root.find(".reader-bookmark-submission-error").remove();
  $("<span>")
    .addClass("reader-bookmark-submission-error")
    .attr({ role: "alert", title: message })
    .append(
      $("<i>")
        .addClass("fas fa-exclamation-triangle")
        .attr("aria-hidden", "true"),
      $("<span>").text(message),
    )
    .appendTo(root);
  root.prop("hidden", false);
}

function bookmarkActivityIsActive() {
  return Number($("#reader-bookmark-activity").attr("data-active-count")) > 0;
}

function scheduleBookmarkActivityRefresh() {
  clearTimeout(bookmarkActivityRefreshTimer);
  bookmarkActivityRefreshTimer = null;
  if (!bookmarkActivityIsActive() || document.visibilityState === "hidden") {
    return;
  }
  bookmarkActivityRefreshTimer = setTimeout(refreshBookmarkActivity, 30000);
}

function refreshBookmarkActivity() {
  clearTimeout(bookmarkActivityRefreshTimer);
  bookmarkActivityRefreshTimer = null;
  $.getJSON("/reader/bookmark/activity")
    .done(function (activity) {
      renderBookmarkActivity(activity);
      scheduleBookmarkActivityRefresh();
    })
    .fail(function () {
      scheduleBookmarkActivityRefresh();
    });
}

function setBookmarkSubmitPending(button, pending) {
  button
    .prop("disabled", pending)
    .attr("aria-busy", pending ? "true" : null)
    .toggleClass("btn-primary", pending)
    .toggleClass("btn-secondary", !pending)
    .removeClass("btn-danger");
}

// Document-structure-aware reading navigation. Space and touch gestures share
// this model, while saving a cross-device checkpoint remains explicit.
var readingNavigation = {
  container: null,
  blocks: [],
  landmarks: [],
  target: null,
  frame: null,
  landingBlock: null,
  landingTimer: null,
  resizeObserver: null,
  mutationObserver: null,
};

function clampReadingValue(value, minimum, maximum) {
  return Math.min(maximum, Math.max(minimum, value));
}

function readingPrefersReducedMotion() {
  return (
    window.matchMedia &&
    window.matchMedia("(prefers-reduced-motion: reduce)").matches
  );
}

function readingScrollBehavior() {
  return readingPrefersReducedMotion() ? "auto" : "smooth";
}

function readingViewport(container) {
  var topNav = document.getElementById("top-nav");
  var containerRect = container.getBoundingClientRect();
  var top = Math.max(
    0,
    containerRect.top,
    topNav ? topNav.getBoundingClientRect().bottom : 0,
  );
  return {
    top: top,
    bottom: Math.min(window.innerHeight, Math.max(top, containerRect.bottom)),
  };
}

function readingUsesHorizontalColumns(container) {
  var style = window.getComputedStyle(container);
  return (
    style.columnWidth !== "auto" &&
    container.scrollWidth > container.clientWidth + 1
  );
}

function readingAxisMetrics(container) {
  if (readingUsesHorizontalColumns(container)) {
    var horizontalTotal = Math.max(1, container.scrollWidth);
    var horizontalExtent = clampReadingValue(
      container.clientWidth,
      0,
      horizontalTotal,
    );
    var horizontalStart = clampReadingValue(
      container.scrollLeft,
      0,
      Math.max(0, horizontalTotal - horizontalExtent),
    );
    return {
      axis: "horizontal",
      total: horizontalTotal,
      start: horizontalStart,
      extent: horizontalExtent,
    };
  }

  var viewport = readingViewport(container);
  var containerRect = container.getBoundingClientRect();
  var verticalTotal = Math.max(1, container.scrollHeight, containerRect.height);
  var verticalStart = clampReadingValue(
    viewport.top - containerRect.top,
    0,
    verticalTotal,
  );
  var verticalEnd = clampReadingValue(
    viewport.bottom - containerRect.top,
    verticalStart,
    verticalTotal,
  );
  var verticalExtent = verticalEnd - verticalStart;
  return {
    axis: "vertical",
    total: verticalTotal,
    start: verticalStart,
    extent: verticalExtent,
  };
}

function readingElementProgress(container, element, metrics) {
  var containerRect = container.getBoundingClientRect();
  var rects = readableClientRects(element);
  var rect = rects.length ? rects[0] : element.getBoundingClientRect();
  var position =
    metrics.axis === "horizontal"
      ? container.scrollLeft + rect.left - containerRect.left
      : rect.top - containerRect.top;
  return clampReadingValue(position / metrics.total, 0, 1);
}

function readableClientRects(block) {
  return Array.from(block.getClientRects()).filter(function (rect) {
    return rect.width > 0 && rect.height > 0;
  });
}

function readingStructureElements(container) {
  return Array.from(container.querySelectorAll("h1, h2, h3, h4, hr")).filter(
    function (element) {
      return (
        element.getClientRects().length > 0 &&
        (element.matches("hr") || element.textContent.trim().length > 0)
      );
    },
  );
}

function readingStructurePriority(element) {
  if (element.matches("h1")) return 0;
  if (element.matches("h2")) return 1;
  if (element.matches("hr")) return 2;
  if (element.matches("h3")) return 3;
  return 4;
}

function selectReadingStructureCandidates(elements) {
  var segmentCount = 24;
  if (elements.length <= segmentCount) return elements;

  // Keep the rail quiet on documents with generated or very granular
  // headings. Pick one landmark from each document segment so the full
  // article remains represented, preferring high-level structure locally.
  var segmentSize = elements.length / segmentCount;
  return Array.from({ length: segmentCount }, function (_unused, segmentIndex) {
    var segmentStart = Math.floor(segmentIndex * segmentSize);
    var segmentEnd =
      segmentIndex === segmentCount - 1
        ? elements.length
        : Math.floor((segmentIndex + 1) * segmentSize);
    var midpoint = (segmentStart + segmentEnd - 1) / 2;
    var segment = elements.slice(segmentStart, segmentEnd);
    var choice = segment.reduce(
      function (best, element, localIndex) {
        var priority = readingStructurePriority(element);
        var distance = Math.abs(segmentStart + localIndex - midpoint);
        if (
          priority < best.priority ||
          (priority === best.priority && distance < best.distance)
        ) {
          return { element: element, priority: priority, distance: distance };
        }
        return best;
      },
      {
        element: segment[0],
        priority: readingStructurePriority(segment[0]),
        distance: Math.abs(segmentStart - midpoint),
      },
    );
    return choice.element;
  });
}

function readingLandmarkLabel(element, candidates, index) {
  if (!element.matches("hr")) {
    return element.textContent.replace(/\s+/g, " ").trim().slice(0, 120);
  }
  var followingHeading = candidates.slice(index + 1).find(function (candidate) {
    return !candidate.matches("hr");
  });
  return followingHeading
    ? "Section break before " +
        followingHeading.textContent.replace(/\s+/g, " ").trim().slice(0, 96)
    : "Section break";
}

function scrollToReadingElement(element) {
  var container = readingNavigation.container;
  if (!container || !element) return;
  var rect = element.getBoundingClientRect();
  if (readingUsesHorizontalColumns(container)) {
    var containerRect = container.getBoundingClientRect();
    container.scrollTo({
      left: Math.max(0, container.scrollLeft + rect.left - containerRect.left),
      behavior: readingScrollBehavior(),
    });
  } else {
    window.scrollTo({
      top: window.scrollY + rect.top - readingViewport(container).top,
      behavior: readingScrollBehavior(),
    });
  }
  showReadingLanding(element);
  setTimeout(
    requestReadingNavigationUpdate,
    readingPrefersReducedMotion() ? 0 : 350,
  );
}

function rebuildReadingLandmarks() {
  var container = readingNavigation.container;
  var rail = document.querySelector(".reading-structure-landmarks");
  if (!container || !rail) return;

  rail.replaceChildren();
  var metrics = readingAxisMetrics(container);
  var structureElements = readingStructureElements(container);
  var candidates = selectReadingStructureCandidates(structureElements);
  readingNavigation.landmarks = candidates.map(function (element) {
    var divider = element.matches("hr");
    var progress = readingElementProgress(container, element, metrics);
    var structureIndex = structureElements.indexOf(element);
    var label = readingLandmarkLabel(
      element,
      structureElements,
      structureIndex,
    );
    var percent = Math.round(progress * 100);
    var control = document.createElement("button");
    control.type = "button";
    control.className =
      "reading-structure-landmark " +
      (divider ? "is-divider" : "is-heading-" + element.tagName.substring(1));
    control.style.top = 2 + progress * 96 + "%";
    control.dataset.label = label + " · " + percent + "%";
    control.setAttribute(
      "aria-label",
      "Go to " + label + ", around " + percent + "%",
    );
    control.title = label + " · " + percent + "%";
    control.addEventListener("click", function () {
      scrollToReadingElement(element);
    });
    rail.appendChild(control);
    return {
      element: element,
      control: control,
      progress: progress,
      label: label,
    };
  });
  rail.hidden = readingNavigation.landmarks.length === 0;
}

function updateNearestReadingLandmark() {
  var container = readingNavigation.container;
  if (!container) return;

  var metrics = readingAxisMetrics(container);
  var readingFocus = clampReadingValue(
    (metrics.start + metrics.extent / 3) / metrics.total,
    0,
    1,
  );
  var nearestLandmark = null;
  var nearestDistance = Infinity;
  readingNavigation.landmarks.forEach(function (landmark) {
    var distance = Math.abs(landmark.progress - readingFocus);
    if (distance < nearestDistance) {
      nearestLandmark = landmark;
      nearestDistance = distance;
    }
  });
  readingNavigation.landmarks.forEach(function (landmark) {
    var current = landmark === nearestLandmark;
    landmark.control.classList.toggle("is-current", current);
    if (current) landmark.control.setAttribute("aria-current", "location");
    else landmark.control.removeAttribute("aria-current");
  });
}

function nextVerticalReadingTarget(container, blocks) {
  var viewport = readingViewport(container);
  for (var block of blocks) {
    for (var rect of readableClientRects(block)) {
      if (rect.bottom <= viewport.top + 2) continue;
      if (rect.top <= viewport.top + 2 && rect.bottom > viewport.bottom + 2) {
        return {
          axis: "vertical",
          block: null,
          rect: { top: viewport.bottom },
          beyondViewport: true,
          fallback: true,
        };
      }
      var startsBelow = rect.top >= viewport.bottom - 2;
      var crossesBottom =
        rect.top >= viewport.top + 2 &&
        rect.top < viewport.bottom - 2 &&
        rect.bottom > viewport.bottom + 2;
      if (startsBelow || crossesBottom) {
        return {
          axis: "vertical",
          block: block,
          rect: rect,
          beyondViewport: startsBelow,
        };
      }
    }
  }

  if (
    window.scrollY + window.innerHeight <
    document.documentElement.scrollHeight - 2
  ) {
    return {
      axis: "vertical",
      block: null,
      rect: { top: viewport.bottom },
      beyondViewport: true,
      fallback: true,
    };
  }
  return null;
}

function nextHorizontalReadingTarget(container, blocks) {
  var containerRect = container.getBoundingClientRect();
  for (var block of blocks) {
    for (var rect of readableClientRects(block)) {
      if (rect.right <= containerRect.left + 2) continue;
      var startsBeyond = rect.left >= containerRect.right - 2;
      var crossesEdge =
        rect.left >= containerRect.left + 2 &&
        rect.left < containerRect.right - 2 &&
        rect.right > containerRect.right + 2;
      if (startsBeyond || crossesEdge) {
        return {
          axis: "horizontal",
          block: block,
          rect: rect,
          beyondViewport: startsBeyond,
        };
      }
    }
  }

  if (
    container.scrollLeft <
    container.scrollWidth - container.clientWidth - 2
  ) {
    return {
      axis: "horizontal",
      block: null,
      rect: {
        left: containerRect.right,
        top: containerRect.top,
      },
      beyondViewport: true,
      fallback: true,
    };
  }
  return null;
}

function nextReadingTarget() {
  var container = readingNavigation.container;
  if (!container) return null;
  if (readingUsesHorizontalColumns(container)) {
    return nextHorizontalReadingTarget(container, readingNavigation.blocks);
  }
  return nextVerticalReadingTarget(container, readingNavigation.blocks);
}

function positionReadingStepMarker(marker, viewportY) {
  var rail = marker && marker.parentElement;
  var container = readingNavigation.container;
  if (!rail || !container) return;
  var viewport = readingViewport(container);
  var clampedY = Math.min(
    viewport.bottom - 14,
    Math.max(viewport.top + 14, viewportY),
  );
  marker.style.top = clampedY - rail.getBoundingClientRect().top + "px";
}

function updateReadingLandingMarker() {
  var marker = document.querySelector(".reading-step-landing");
  var block = readingNavigation.landingBlock;
  var container = readingNavigation.container;
  if (!marker || !container || !marker.classList.contains("is-visible")) {
    return;
  }
  if (!block) {
    positionReadingStepMarker(marker, readingViewport(container).top + 14);
    return;
  }
  var viewport = readingViewport(container);
  var containerRect = container.getBoundingClientRect();
  var horizontal = readingUsesHorizontalColumns(container);
  var rect = readableClientRects(block).find(function (candidate) {
    var verticallyVisible =
      candidate.bottom > viewport.top && candidate.top < viewport.bottom;
    var horizontallyVisible =
      !horizontal ||
      (candidate.right > containerRect.left &&
        candidate.left < containerRect.right);
    return verticallyVisible && horizontallyVisible;
  });
  positionReadingStepMarker(marker, rect ? rect.top : viewport.top + 14);
}

function updateReadingNavigation() {
  readingNavigation.frame = null;
  var marker = document.querySelector(".reading-step-next");
  if (!marker || !readingNavigation.container) return;

  var target = nextReadingTarget();
  readingNavigation.target = target;
  marker.classList.toggle("is-visible", !!target);
  marker.classList.toggle(
    "is-below",
    !!target && target.axis === "vertical" && target.beyondViewport,
  );
  marker.classList.toggle(
    "is-horizontal",
    !!target && target.axis === "horizontal",
  );
  if (target) positionReadingStepMarker(marker, target.rect.top);
  updateNearestReadingLandmark();
  updateReadingLandingMarker();
}

function requestReadingNavigationUpdate() {
  if (readingNavigation.frame !== null) return;
  readingNavigation.frame = window.requestAnimationFrame(
    updateReadingNavigation,
  );
}

function refreshReadingLandmarks() {
  if (!readingNavigation.container) return;
  rebuildReadingLandmarks();
  requestReadingNavigationUpdate();
}

function refreshReadingBlocks() {
  if (!readingNavigation.container) return;
  readingNavigation.blocks = getReadingBlocks(readingNavigation.container);
  refreshReadingLandmarks();
}

function showReadingLanding(block) {
  var marker = document.querySelector(".reading-step-landing");
  if (!marker) return;
  readingNavigation.landingBlock = block;
  marker.classList.add("is-visible");
  clearTimeout(readingNavigation.landingTimer);
  readingNavigation.landingTimer = setTimeout(function () {
    marker.classList.remove("is-visible");
    readingNavigation.landingBlock = null;
  }, 1500);
  requestReadingNavigationUpdate();
}

function advanceReadingBlock() {
  var container = readingNavigation.container;
  if (!container) return false;
  var target = nextReadingTarget();
  if (!target) return false;

  if (target.axis === "horizontal") {
    var containerRect = container.getBoundingClientRect();
    var horizontalDelta = target.fallback
      ? container.clientWidth * 0.9
      : target.rect.left - containerRect.left;
    container.scrollTo({
      left: Math.min(
        container.scrollWidth - container.clientWidth,
        container.scrollLeft + Math.max(1, horizontalDelta),
      ),
      behavior: readingScrollBehavior(),
    });
  } else {
    var viewport = readingViewport(container);
    var verticalDelta = target.fallback
      ? (viewport.bottom - viewport.top) * 0.85
      : target.rect.top - viewport.top;
    window.scrollTo({
      top: window.scrollY + Math.max(1, verticalDelta),
      behavior: readingScrollBehavior(),
    });
  }

  showReadingLanding(target.block);
  setTimeout(requestReadingNavigationUpdate, 350);
  return true;
}

$(function () {
  var container = document.getElementById("item-content-body");
  if (!container) return;
  readingNavigation.container = container;
  refreshReadingBlocks();
  $(window).on("scroll.reading-navigation", requestReadingNavigationUpdate);
  $(window).on("resize.reading-navigation", refreshReadingBlocks);
  $(window).on("load.reading-navigation", refreshReadingBlocks);
  $(container).on("scroll.reading-navigation", requestReadingNavigationUpdate);

  container.addEventListener("load", refreshReadingLandmarks, true);
  container.addEventListener("error", refreshReadingLandmarks, true);
  if ("ResizeObserver" in window) {
    readingNavigation.resizeObserver = new ResizeObserver(
      refreshReadingLandmarks,
    );
    readingNavigation.resizeObserver.observe(container);
  }
  if ("MutationObserver" in window) {
    readingNavigation.mutationObserver = new MutationObserver(function (
      mutations,
    ) {
      if (
        mutations.some(function (mutation) {
          return mutation.type === "childList";
        })
      ) {
        refreshReadingLandmarks();
      }
    });
    readingNavigation.mutationObserver.observe(container, {
      childList: true,
      subtree: true,
    });
  }
  if (document.fonts && document.fonts.ready) {
    document.fonts.ready.then(refreshReadingLandmarks);
  }
});

function keepCurrentItemUnread(onComplete) {
  var id = $('meta[name="llar-id"]').attr("content");
  if (!id) {
    onComplete();
    return;
  }
  requestItemState(id, "mark-unread").always(onComplete);
}

// Keyboard navigation
$("body").on("keydown", function (event) {
  if ($("body").hasClass("modal-open")) return;
  if (event.key === "Escape" && annotationModeActive) {
    event.preventDefault();
    toggleAnnotationMode();
    return;
  }
  if ($(event.target).is("input, textarea, select, [contenteditable]")) return;
  if (event.ctrlKey || event.metaKey || event.altKey) return;
  if (!readingNavigation.container) return;

  var nextUrl = null;
  if (event.key === "n") {
    nextUrl = $("#btn-next-item").attr("href");
    if (nextUrl) window.location.href = nextUrl;
  } else if (event.key === "p") {
    window.history.back();
  } else if (event.key === "N") {
    event.preventDefault();
    nextUrl = $("#btn-next-item").attr("href");
    if (nextUrl) {
      keepCurrentItemUnread(function () {
        window.location.href = nextUrl;
      });
    }
  } else if (event.key === "P") {
    event.preventDefault();
    keepCurrentItemUnread(function () {
      window.history.back();
    });
  } else if (event.key === "a") {
    event.preventDefault();
    toggleAnnotationMode();
  } else if (event.key === " " && !event.shiftKey) {
    event.preventDefault();
    advanceReadingBlock();
  }
});

// Swipe left uses the same forward movement as Space. Pointer Events cover
// touch and pen input without requiring a gesture library.
$(function () {
  var main = document.querySelector("main");
  if (!main) return;
  var swipeStart = null;

  main.addEventListener("pointerdown", function (event) {
    if (event.isPrimary && event.pointerType !== "mouse") {
      swipeStart = { x: event.clientX, y: event.clientY };
    }
  });

  main.addEventListener("pointercancel", function () {
    swipeStart = null;
  });

  main.addEventListener("pointerup", function (event) {
    if (!swipeStart || !event.isPrimary || event.pointerType === "mouse") {
      swipeStart = null;
      return;
    }

    var deltaX = event.clientX - swipeStart.x;
    var deltaY = event.clientY - swipeStart.y;
    swipeStart = null;
    if (
      deltaX <= -50 &&
      Math.abs(deltaX) > Math.abs(deltaY) &&
      !$("body").hasClass("modal-open")
    ) {
      advanceReadingBlock();
    }
  });
});

$(document).ready(function () {
  $(".reader-global-status-dismiss").on("click", dismissReaderStatus);

  var reservedBulkTags = new Set([
    "unread",
    "saved",
    "archive",
    "in-progress",
    "has-annotations",
  ]);

  function normalizeBulkTag(value) {
    var normalized = String(value || "")
      .trim()
      .toLocaleLowerCase()
      .replace(/\s+/g, "-")
      .replace(/[^\p{L}\p{N}-]/gu, "")
      .replace(/-{2,}/g, "-")
      .replace(/^-|-$/g, "");
    return normalized && !reservedBulkTags.has(normalized) ? normalized : "";
  }

  function selectedHeadlineRows() {
    return $(".reader-headline-checkbox:checked").closest(
      ".reader-headline-row",
    );
  }

  function rowItemTags(row) {
    var value = row.attr("data-item-tags") || "[]";
    try {
      return JSON.parse(value);
    } catch (_error) {
      return [];
    }
  }

  function syncHeadlineSelection() {
    var checkboxes = $(".reader-headline-checkbox");
    var selected = checkboxes.filter(":checked");
    var count = selected.length;
    $(".reader-headline-row").each(function () {
      $(this).toggleClass(
        "is-selected",
        $(this).find(".reader-headline-checkbox").prop("checked"),
      );
    });
    $(".reader-bulk-selection-count").text(count + " selected");
    $(".reader-bulk-edit-tags").prop("disabled", count === 0);
    $(".reader-bulk-select-all")
      .prop("checked", !!checkboxes.length && count === checkboxes.length)
      .prop("indeterminate", count > 0 && count < checkboxes.length);
  }

  function exitHeadlineSelection() {
    $("body").removeClass("reader-bulk-selection-active");
    $(".reader-bulk-selection-bar").prop("hidden", true);
    $(".reader-headline-checkbox").prop("checked", false);
    syncHeadlineSelection();
  }

  $(".btn-select-headlines").on("click", function (event) {
    event.preventDefault();
    if (!window.matchMedia("(min-width: 768px)").matches) return;
    $("body").addClass("reader-bulk-selection-active");
    $(".reader-bulk-selection-bar").prop("hidden", false);
    syncHeadlineSelection();
    $(".reader-headline-checkbox:first").trigger("focus");
  });

  $(".reader-bulk-cancel").on("click", exitHeadlineSelection);
  $(".reader-headline-checkbox").on("change", syncHeadlineSelection);
  $(".reader-bulk-select-all").on("change", function () {
    $(".reader-headline-checkbox").prop("checked", $(this).prop("checked"));
    syncHeadlineSelection();
  });

  function bulkTagsFor(operation) {
    return $(".reader-bulk-tag-chips[data-operation='" + operation + "']")
      .find(".reader-bulk-tag-chip")
      .map(function () {
        return $(this).data("tag");
      })
      .get();
  }

  function setBulkTagError(message) {
    $(".reader-bulk-tags-error")
      .text(message || "")
      .prop("hidden", !message);
  }

  function syncBulkTagSubmit() {
    $(".reader-bulk-tags-submit").prop(
      "disabled",
      bulkTagsFor("add").length + bulkTagsFor("remove").length === 0,
    );
  }

  function addBulkTagChip(operation, rawTag) {
    var tag = normalizeBulkTag(rawTag);
    if (!tag) {
      setBulkTagError("Enter a valid custom tag.");
      return;
    }
    var opposite = operation === "add" ? "remove" : "add";
    if (bulkTagsFor(opposite).includes(tag)) {
      setBulkTagError("A tag cannot be added and removed together.");
      return;
    }
    if (bulkTagsFor(operation).includes(tag)) return;
    setBulkTagError("");
    var chip = $("<span>")
      .addClass("reader-bulk-tag-chip")
      .attr("data-tag", tag)
      .data("tag", tag)
      .append(document.createTextNode(tag));
    $("<button>")
      .addClass("reader-bulk-tag-chip-remove")
      .attr({
        type: "button",
        title: "Remove " + tag,
        "aria-label": "Remove " + tag,
      })
      .text("×")
      .appendTo(chip);
    chip.appendTo(".reader-bulk-tag-chips[data-operation='" + operation + "']");
    syncBulkTagSubmit();
  }

  $(".reader-bulk-tag-chips").on(
    "click",
    ".reader-bulk-tag-chip-remove",
    function () {
      $(this).closest(".reader-bulk-tag-chip").remove();
      setBulkTagError("");
      syncBulkTagSubmit();
    },
  );

  $(".reader-bulk-tag-input")
    .on("keydown", function (event) {
      if (event.key !== "Enter" && event.key !== ",") return;
      event.preventDefault();
      addBulkTagChip($(this).data("operation"), $(this).val());
      $(this).val("");
    })
    .on("blur", function () {
      if (!$(this).val().trim()) return;
      addBulkTagChip($(this).data("operation"), $(this).val());
      $(this).val("");
    });

  $("#reader-bulk-tags-modal").on("show.bs.modal", function () {
    var rows = selectedHeadlineRows();
    var counts = new Map();
    rows.each(function () {
      rowItemTags($(this)).forEach(function (tag) {
        counts.set(tag, (counts.get(tag) || 0) + 1);
      });
    });
    $(".reader-bulk-tags-modal-count").text(rows.length);
    $(".reader-bulk-tag-chips").empty();
    $(".reader-bulk-tag-input").val("");
    setBulkTagError("");
    syncBulkTagSubmit();
    var context = $(".reader-bulk-tag-context").empty();
    Array.from(counts.keys())
      .sort()
      .forEach(function (tag) {
        var item = $("<span>").addClass("reader-bulk-tag-context-item");
        item.append(
          $("<span>").text(
            tag + " · " + counts.get(tag) + " of " + rows.length,
          ),
        );
        $("<button>")
          .addClass("btn btn-sm reader-bulk-tag-context-action")
          .attr({
            type: "button",
            title: "Add " + tag + " to all",
            "aria-label": "Add " + tag + " to all",
          })
          .text("+")
          .on("click", function () {
            addBulkTagChip("add", tag);
          })
          .appendTo(item);
        $("<button>")
          .addClass("btn btn-sm reader-bulk-tag-context-action")
          .attr({
            type: "button",
            title: "Remove " + tag + " from all",
            "aria-label": "Remove " + tag + " from all",
          })
          .text("−")
          .on("click", function () {
            addBulkTagChip("remove", tag);
          })
          .appendTo(item);
        item.appendTo(context);
      });
  });

  $("#reader-bulk-tags-form").on("submit", function (event) {
    event.preventDefault();
    var form = $(this);
    var submit = form.find(".reader-bulk-tags-submit");
    if (submit.data("state-request-pending")) return;
    var rows = selectedHeadlineRows();
    var addTags = bulkTagsFor("add");
    var removeTags = bulkTagsFor("remove");
    setBulkTagError("");
    setStateControlPending(submit, true);
    $.ajax({
      url: "/reader/items/tags",
      method: "POST",
      contentType: "application/json",
      data: JSON.stringify({
        item_ids: rows
          .map(function () {
            return $(this).data("id");
          })
          .get(),
        add_tags: addTags,
        remove_tags: removeTags,
      }),
    })
      .done(function (response) {
        (response.items || []).forEach(applyItemState);
        var modalElement = document.getElementById("reader-bulk-tags-modal");
        $(modalElement).one("hidden.bs.modal", function () {
          $(".btn-select-headlines:first").trigger("focus");
        });
        bootstrap.Modal.getOrCreateInstance(modalElement).hide();
        exitHeadlineSelection();
        showListLifecycleStatus(
          "Updated tags on " +
            rows.length +
            (rows.length === 1 ? " item." : " items."),
          "success",
        );
      })
      .fail(function () {
        setBulkTagError("Could not update tags. Nothing changed.");
      })
      .always(function () {
        setStateControlPending(submit, false);
        syncBulkTagSubmit();
      });
  });

  $(window).on("resize.reader-bulk-selection", function () {
    if (
      $("body").hasClass("reader-bulk-selection-active") &&
      !window.matchMedia("(min-width: 768px)").matches
    ) {
      var modalElement = document.getElementById("reader-bulk-tags-modal");
      if (modalElement && modalElement.classList.contains("show"))
        bootstrap.Modal.getOrCreateInstance(modalElement).hide();
      exitHeadlineSelection();
    }
  });

  function runItemStateBatch(ids, action, onComplete) {
    var remaining = ids.length;
    var successful = [];
    ids.forEach(function (id) {
      requestItemState(id, action)
        .done(function () {
          successful.push(id);
        })
        .always(function () {
          remaining -= 1;
          if (!remaining)
            onComplete(successful, ids.length - successful.length);
        });
    });
  }

  function setReadAllPending(pending) {
    $(".btn-mark-view-read")
      .data("read-all-pending", pending)
      .toggleClass("disabled", pending)
      .attr("aria-disabled", pending ? "true" : null);
  }

  function undoReadAll(ids) {
    setReadAllPending(true);
    showListLifecycleStatus(
      "Restoring the unread state for " + ids.length + " items…",
      "pending",
    );
    runItemStateBatch(ids, "mark-unread", function (successful, failed) {
      setReadAllPending(false);
      var message =
        "Restored " + successful.length + " items to unread in this snapshot.";
      if (failed) message += " " + failed + " could not be restored.";
      showListLifecycleStatus(message, failed ? "error" : "success");
    });
  }

  $(".btn-mark-view-read").on("click", function (event) {
    event.preventDefault();
    if ($(this).data("read-all-pending")) return;
    var ids = Array.from(
      new Set(
        $("main")
          .find('[data-id][data-unread="true"]')
          .map(function () {
            return $(this).data("id");
          })
          .get(),
      ),
    );
    if (!ids.length) {
      showListLifecycleStatus(
        "Every item in this snapshot is already read.",
        "info",
      );
      return;
    }

    setReadAllPending(true);
    showListLifecycleStatus(
      "Marking " + ids.length + " items read…",
      "pending",
    );
    runItemStateBatch(ids, "seen", function (successful, failed) {
      setReadAllPending(false);
      var message =
        "Marked " +
        successful.length +
        (successful.length === 1 ? " item" : " items") +
        " read. This snapshot stays in place.";
      if (failed) message += " " + failed + " could not be changed.";
      showListLifecycleStatus(
        message,
        failed ? "error" : "success",
        successful.length
          ? {
              label: "Undo",
              className: "btn-undo-mark-view-read",
              onClick: function () {
                undoReadAll(successful);
              },
            }
          : null,
      );
    });
  });
  // Bookmark capture is durable immediately; enrichment remains visible in the
  // quiet activity strip across Reader page loads.
  $("#add-thing").on("submit", function (event) {
    event.preventDefault();
    var button = $(this).find(".bookmark-submit");
    var input = $($(button.data("url-source")));
    setBookmarkSubmitPending(button, true);
    var request = $.post({
      url: "/reader/bookmark/add",
      data: { url: input.val(), type: button.data("type") },
      dataType: "json",
      success: (data) => {
        input.val("");
        var activity = Object.assign({}, data.activity || {});
        var readyItems = Array.isArray(activity["recent-ready"])
          ? activity["recent-ready"].slice()
          : [];
        if (
          data.result === "already-saved" &&
          data["item-id"] &&
          !readyItems.some(function (item) {
            return String(item["item-id"]) === String(data["item-id"]);
          })
        ) {
          readyItems.unshift({
            "item-id": data["item-id"],
            href: "/reader/item/by-id/" + data["item-id"],
            label: data.label || "Saved bookmark",
          });
          activity["recent-ready"] = readyItems.slice(0, 3);
        }
        renderBookmarkActivity(activity);
        scheduleBookmarkActivityRefresh();
      },
    });
    request.fail((data) => {
      var response = data && data.responseJSON;
      if (response && response.activity) {
        renderBookmarkActivity(response.activity);
      }
      showBookmarkActivityError(bookmark_capture_error_message(data));
    });
    request.always(function () {
      setBookmarkSubmitPending(button, false);
    });
  });

  scheduleBookmarkActivityRefresh();
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") {
      clearTimeout(bookmarkActivityRefreshTimer);
      bookmarkActivityRefreshTimer = null;
    } else if (bookmarkActivityIsActive()) {
      refreshBookmarkActivity();
    }
  });

  // Activate a privacy-friendly YouTube embed only after an explicit click.
  $(".lazy-youtube-trigger").on("click", function () {
    var vid = $(this).data("vid");
    var target = $(this).data("target");
    var embedUrl =
      "https://www.youtube-nocookie.com/embed/" +
      encodeURIComponent(vid) +
      "?controls=1&fs=1&playsinline=1";
    $("#" + target).html(
      `<iframe src="${embedUrl}" title="YouTube video player" ` +
        `referrerpolicy="strict-origin-when-cross-origin" ` +
        `allow="accelerometer; autoplay; clipboard-write; encrypted-media; fullscreen; gyroscope; picture-in-picture; web-share" ` +
        `allowfullscreen></iframe>`,
    );
  });

  // Main list: mark items read after their lower edge has remained visible.
  var markReadOnViewDwellMs = 1000;
  var markReadOnViewTimers = new Map();
  var markReadObserver = null;

  function markReadOnView(element) {
    var item = $(element);
    item.removeClass("option-mark-read-on-view");
    if (item.data("unread") !== true) {
      return;
    }

    item.data("unread", false).attr("data-unread", "false");
    requestItemState(item.data("id"), "seen").fail(function () {
      item.data("unread", true).attr("data-unread", "true");
    });
  }

  function cancelMarkReadOnView(target) {
    var timer = markReadOnViewTimers.get(target);
    if (timer) {
      clearTimeout(timer);
      markReadOnViewTimers.delete(target);
    }
  }

  function cancelAllMarkReadOnView() {
    markReadOnViewTimers.forEach(function (timer) {
      clearTimeout(timer);
    });
    markReadOnViewTimers.clear();
  }

  function scheduleMarkReadOnView(target, element, onComplete) {
    if (!element || !$(element).hasClass("option-mark-read-on-view")) {
      return;
    }
    if (markReadOnViewTimers.has(target)) {
      return;
    }

    var timer = setTimeout(function () {
      markReadOnViewTimers.delete(target);
      onComplete();
      markReadOnView(element);
    }, markReadOnViewDwellMs);
    markReadOnViewTimers.set(target, timer);
  }

  function elementIsInViewport(element) {
    var rect = element.getBoundingClientRect();
    return rect.bottom >= 0 && rect.top <= window.innerHeight;
  }

  function removeObservedBottomSentinel(sentinel) {
    if (markReadObserver) {
      markReadObserver.unobserve(sentinel);
    }
    sentinel.remove();
  }

  function scheduleBottomSentinelMarkRead(sentinel) {
    scheduleMarkReadOnView(sentinel, sentinel.parentElement, function () {
      removeObservedBottomSentinel(sentinel);
    });
  }

  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") {
      cancelAllMarkReadOnView();
    } else if (markReadObserver) {
      $(".mark-read-on-view-bottom").each(function () {
        if (elementIsInViewport(this)) {
          scheduleBottomSentinelMarkRead(this);
        }
      });
    }
  });

  if ("IntersectionObserver" in window) {
    markReadObserver = new IntersectionObserver(
      function (entries, observer) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            scheduleBottomSentinelMarkRead(entry.target);
          } else {
            cancelMarkReadOnView(entry.target);
          }
        });
      },
      { root: null, threshold: 0 },
    );

    $(".option-mark-read-on-view").each(function () {
      var bottomSentinel = document.createElement("span");
      bottomSentinel.className = "mark-read-on-view-bottom";
      bottomSentinel.setAttribute("aria-hidden", "true");
      bottomSentinel.style.cssText =
        "display:block;width:1px;height:1px;overflow:hidden;";
      this.appendChild(bottomSentinel);
      markReadObserver.observe(bottomSentinel);
    });
  } else {
    function markVisibleItemsRead() {
      $(".option-mark-read-on-view").each(function () {
        if (this.getBoundingClientRect().bottom <= window.innerHeight) {
          scheduleMarkReadOnView(this, this, function () {});
        } else {
          cancelMarkReadOnView(this);
        }
      });
    }

    markVisibleItemsRead();
    $(window).on("scroll resize", markVisibleItemsRead);
  }
  $(".btn-update-sources-in-view").on("click", function (event) {
    if ($(this).data("source-update-ready")) return;
    event.preventDefault();
    if ($(this).data("source-update-pending")) return;
    var target = $(this).data("target");
    var durationLabel = $(this).data("duration-label");
    var timingMessage = durationLabel
      ? " Recent source runs suggest about " + durationLabel + "."
      : " No timing information yet.";
    setSourceUpdatePending(true);
    showListLifecycleStatus(
      "Checking sources." +
        timingMessage +
        " This snapshot will stay in place while the check runs.",
      "pending",
      null,
      "dots",
    );
    $.post(target)
      .done(function () {
        window.setTimeout(update_sources_update_state, 1000, target);
      })
      .fail(function () {
        setSourceUpdatePending(false);
        showListLifecycleStatus(
          "The source check could not be started. This snapshot is unchanged.",
          "error",
        );
      });
  });

  // Item tags use the same state endpoint as semantic workflow actions.
  $(document).on("click", ".item-tag-toggle", function (event) {
    event.preventDefault();
    var button = $(this);
    if (button.data("state-request-pending")) return;
    var action = button.data("is-set") ? "remove-tag" : "add-tag";
    clearReaderStatus();
    setStateControlPending(button, true);
    requestItemState(button.data("id"), action, { tag: button.data("tag") })
      .fail(function (xhr) {
        showReaderError(
          "Could not update tags. Nothing changed.",
          "item-tag",
          xhr,
        );
      })
      .always(function () {
        setStateControlPending(button, false);
      });
  });

  $(".state-toggle").on("click", function (event) {
    event.preventDefault();
    var button = $(this);
    if (button.data("state-request-pending")) return;
    var action = button.data("is-set")
      ? button.data("action-unset")
      : button.data("action-set");
    clearReaderStatus();
    setStateControlPending(button, true);
    requestItemState(button.data("id"), action)
      .fail(function (xhr) {
        showReaderError(
          "Could not update this item. Nothing changed.",
          "item-state",
          xhr,
        );
      })
      .always(function () {
        setStateControlPending(button, false);
      });
  });

  $(".state-action").on("click", function (event) {
    event.preventDefault();
    var button = $(this);
    if (button.data("state-request-pending")) return;
    clearReaderStatus();
    setStateControlPending(button, true);
    requestItemState(button.data("id"), button.data("action"))
      .fail(function (xhr) {
        showReaderError(
          "Could not update this item. Nothing changed.",
          "item-state",
          xhr,
        );
      })
      .always(function () {
        setStateControlPending(button, false);
      });
  });

  $(".btn-save-checkpoint").on("click", saveReadingCheckpoint);
  $(".btn-clear-checkpoint").on("click", clearReadingCheckpoint);
  $(".btn-resume-checkpoint").on("click", resumeReadingCheckpoint);

  // Only ranked result cards produce impressions. Generic reader lists do not.
  var offeredResults = document.querySelectorAll(
    ".result-offer[data-offer-id]",
  );
  if (offeredResults.length && "IntersectionObserver" in window) {
    var offerTimers = new Map();
    var pendingOffers = new Set();
    var offerFlushTimer = null;
    function flushOffers() {
      offerFlushTimer = null;
      if (!pendingOffers.size) return;
      var ids = Array.from(pendingOffers);
      pendingOffers.clear();
      $.post("/reader/events/impression", { offer_ids: ids.join(",") });
    }
    function queueOffer(element) {
      pendingOffers.add(element.dataset.offerId);
      offerObserver.unobserve(element);
      offerTimers.delete(element);
      if (!offerFlushTimer) offerFlushTimer = setTimeout(flushOffers, 250);
    }
    var offerObserver = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          var enoughVisible =
            entry.isIntersecting &&
            (entry.intersectionRatio >= 0.5 ||
              entry.intersectionRect.height >=
                Math.min(300, entry.rootBounds.height * 0.5));
          if (enoughVisible && !offerTimers.has(entry.target)) {
            offerTimers.set(
              entry.target,
              setTimeout(queueOffer, 750, entry.target),
            );
          } else if (!enoughVisible && offerTimers.has(entry.target)) {
            clearTimeout(offerTimers.get(entry.target));
            offerTimers.delete(entry.target);
          }
        });
      },
      { threshold: [0, 0.5] },
    );
    offeredResults.forEach(function (element) {
      offerObserver.observe(element);
    });
  }

  // custom tag modal form
  $("form.add-custom-tag").on("submit", function (event) {
    event.preventDefault();
    var form = $(this);
    var input = form.find("input:first");
    var submit = form.find('[type="submit"]');
    var tag = input.val().trim();
    if (!tag || submit.data("state-request-pending")) return;
    clearReaderStatus();
    setStateControlPending(submit, true);
    requestItemState(form.data("id"), "add-tag", { tag: tag })
      .done(function () {
        input.val("");
      })
      .fail(function (xhr) {
        showReaderError(
          "Could not add the tag. Nothing changed.",
          "item-tag",
          xhr,
        );
      })
      .always(function () {
        setStateControlPending(submit, false);
      });
  });

  // annotation mode
  $(".btn-annotation-mode").on("click", function (event) {
    event.preventDefault();
    toggleAnnotationMode();
  });

  $(".btn-close-annotation-mode").on("click", function () {
    if (annotationModeActive) toggleAnnotationMode();
  });

  $("#btn-highlight-selection").on("click", function () {
    createHighlight();
  });

  $("#btn-add-item-note").on("click", function () {
    createItemNote();
  });

  $("#annotation-note-input").on("keydown", function (event) {
    if ((event.ctrlKey || event.metaKey) && event.key === "Enter") {
      event.preventDefault();
      createItemNote();
    }
  });

  // export buttons
  $("#btn-export-zotero").on("click", function (e) {
    e.preventDefault();
    exportToZotero();
  });
  $("#btn-export-url-handler").on("click", function (e) {
    e.preventDefault();
    exportToUrlHandler();
  });
});

// Mobile uses the sidebars as mutually exclusive destination drawers. The
// desktop context bar remains visible at md and above, so no action set is
// duplicated inside a collapsed navbar.
$(function () {
  var drawers = $("#groupnav, #sourcenav");
  if (!drawers.length) return;

  function drawerToggle(drawer) {
    return $('[aria-controls="' + drawer.id + '"]').first();
  }

  function drawerName(drawer) {
    return drawer.id === "sourcenav" ? "source filters" : "Reader navigation";
  }

  function syncDrawerToggle(drawer) {
    var expanded = $(drawer).hasClass("show");
    var label = (expanded ? "Close " : "Open ") + drawerName(drawer);
    drawerToggle(drawer)
      .attr("aria-expanded", expanded ? "true" : "false")
      .attr("aria-label", label)
      .attr("title", label);
  }

  function usesMobileNavigation() {
    return window.matchMedia("(max-width: 767.98px)").matches;
  }

  function syncMobileNavigationState() {
    drawers.each(function () {
      syncDrawerToggle(this);
    });
    if (!usesMobileNavigation()) {
      $("body").removeClass("reader-mobile-navigation-open");
      return;
    }
    $("body").toggleClass(
      "reader-mobile-navigation-open",
      drawers.filter(".show").length > 0,
    );
  }

  drawers.on("show.bs.collapse", function () {
    if (!usesMobileNavigation()) return;
    var opening = this;
    drawers.filter(".show").each(function () {
      if (this !== opening && window.bootstrap) {
        window.bootstrap.Collapse.getOrCreateInstance(this).hide();
      }
    });
  });
  drawers.on("shown.bs.collapse", function () {
    syncMobileNavigationState();
    if (!usesMobileNavigation()) return;
    $(this).find(".nav-link:visible").first().trigger("focus");
  });
  drawers.on("hidden.bs.collapse", syncMobileNavigationState);

  $(document).on("keydown.reader-mobile-navigation", function (event) {
    if (event.key !== "Escape" || !usesMobileNavigation()) return;
    event.preventDefault();
    drawers.filter(".show").each(function () {
      if (window.bootstrap) {
        var toggle = drawerToggle(this);
        $(this).one("hidden.bs.collapse", function () {
          toggle.trigger("focus");
        });
        window.bootstrap.Collapse.getOrCreateInstance(this).hide();
      }
    });
  });
  $(window).on("resize.reader-mobile-navigation", syncMobileNavigationState);
  syncMobileNavigationState();
});

//
// Annotation Mode
//

var annotationModeActive = false;
var annotations = [];
var pendingSelector = null;
var highlightRanges = new Map();

function annotationCounts() {
  return {
    highlights: annotations.filter(function (annotation) {
      return annotation.selector != null;
    }).length,
    notes: annotations.filter(function (annotation) {
      return annotation.selector == null && annotation.body != null;
    }).length,
  };
}

function setAnnotationStatus(message) {
  $("#annotation-mode-status").text(message);
}

function updateAnnotationStatus() {
  var counts = annotationCounts();
  var saved = [];
  if (counts.highlights) {
    saved.push(
      counts.highlights + " highlight" + (counts.highlights === 1 ? "" : "s"),
    );
  }
  if (counts.notes) {
    saved.push(counts.notes + " note" + (counts.notes === 1 ? "" : "s"));
  }
  setAnnotationStatus(
    "Select text to highlight or write a note" +
      (saved.length ? " · " + saved.join(" · ") : ""),
  );
}

function getItemId() {
  return $("#item-meta").data("id");
}

function toggleAnnotationMode() {
  if (!$("#item-content-body").length) return;
  annotationModeActive = !annotationModeActive;
  var btn = $(".btn-annotation-mode");

  if (annotationModeActive) {
    btn
      .addClass("active")
      .attr("aria-pressed", "true")
      .attr("title", "Close annotation mode (a)")
      .attr("aria-label", "Close annotation mode (a)");
    $("#annotation-bottom-bar").show();
    $("#item-content-body").addClass("annotation-mode-active");
    setAnnotationStatus("Loading annotations…");
    loadAnnotations();
    $("#item-content-body").on("mouseup.annotation", onTextSelected);
    $(document).on("selectionchange.annotation", onAnnotationSelectionChanged);
  } else {
    btn
      .removeClass("active")
      .attr("aria-pressed", "false")
      .attr("title", "Annotation Mode (a)")
      .attr("aria-label", "Annotation Mode (a)");
    $("#annotation-bottom-bar").hide();
    $("#annotation-item-notes").hide();
    $("#annotation-selection-actions").hide();
    $("#item-content-body").removeClass("annotation-mode-active");
    clearHighlights();
    $("#item-content-body").off("mouseup.annotation");
    $(document).off("selectionchange.annotation");
    pendingSelector = null;
  }
  $("body").toggleClass("reading-annotation-mode", annotationModeActive);
  setTimeout(refreshReadingBlocks, 0);
}

function openRequestedAnnotations() {
  var params = new URLSearchParams(window.location.search);
  if (params.get("annotations") !== "open" || annotationModeActive) return;
  toggleAnnotationMode();
}

$(openRequestedAnnotations);

function loadAnnotations() {
  var itemId = getItemId();
  if (!itemId) return;
  $.getJSON("/reader/annotation/" + itemId, function (data) {
    annotations = data.annotations || [];
    renderHighlights();
    renderNotes();
    updateAnnotationStatus();
  }).fail(function () {
    setAnnotationStatus("Could not load annotations");
  });
}

//
// Text Selection
//

function getTextOffset(container, node, offset) {
  var walker = document.createTreeWalker(
    container,
    NodeFilter.SHOW_TEXT,
    null,
    false,
  );
  var pos = 0;
  while (walker.nextNode()) {
    if (walker.currentNode === node) {
      return pos + offset;
    }
    pos += walker.currentNode.textContent.length;
  }
  return pos + offset;
}

function getContainerText(container) {
  var walker = document.createTreeWalker(
    container,
    NodeFilter.SHOW_TEXT,
    null,
    false,
  );
  var text = "";
  while (walker.nextNode()) {
    text += walker.currentNode.textContent;
  }
  return text;
}

function onAnnotationSelectionChanged() {
  var selection = window.getSelection();
  if (annotationModeActive && selection && !selection.isCollapsed) {
    onTextSelected();
  }
}

function clearPendingAnnotationSelection() {
  $("#annotation-selection-actions").hide();
  pendingSelector = null;
  if (annotationModeActive) updateAnnotationStatus();
}

function onTextSelected() {
  var sel = window.getSelection();
  if (!sel || sel.isCollapsed || sel.rangeCount === 0) {
    clearPendingAnnotationSelection();
    return;
  }

  var range = sel.getRangeAt(0);
  var container = document.getElementById("item-content-body");
  if (
    !container ||
    !container.contains(range.startContainer) ||
    !container.contains(range.endContainer)
  ) {
    clearPendingAnnotationSelection();
    return;
  }

  if (sel.toString().trim().length === 0) {
    clearPendingAnnotationSelection();
    return;
  }

  var fullText = getContainerText(container);
  var start = getTextOffset(container, range.startContainer, range.startOffset);
  var end = getTextOffset(container, range.endContainer, range.endOffset);
  // Derive exact from DOM text, not sel.toString() which normalizes whitespace
  var exact = fullText.substring(start, end);
  var prefixStart = Math.max(0, start - 32);
  var suffixEnd = Math.min(fullText.length, end + 32);

  pendingSelector = {
    position: { type: "TextPositionSelector", start: start, end: end },
    quote: {
      type: "TextQuoteSelector",
      exact: exact,
      prefix: fullText.substring(prefixStart, start),
      suffix: fullText.substring(end, suffixEnd),
    },
  };

  $("#annotation-selection-actions").show();
  setAnnotationStatus("Selection ready to highlight");
}

//
// CRUD
//

function createHighlight() {
  if (!pendingSelector) return;
  var itemId = getItemId();
  var button = $("#btn-highlight-selection").prop("disabled", true);
  $.post(
    "/reader/annotation/" + itemId,
    { selector: JSON.stringify(pendingSelector) },
    function (data) {
      annotations.push(data.annotation);
      window.getSelection().removeAllRanges();
      pendingSelector = null;
      $("#annotation-selection-actions").hide();
      renderHighlights();
      updateAnnotationStatus();
    },
  )
    .fail(function (xhr) {
      console.error("[annotations] highlight create failed:", xhr.status);
      setAnnotationStatus("Could not save highlight");
    })
    .always(function () {
      button.prop("disabled", false);
    });
}

function createItemNote() {
  var input = $("#annotation-note-input");
  var text = input.val();
  if (!text || text.trim().length === 0) return;
  var itemId = getItemId();
  var button = $("#btn-add-item-note").prop("disabled", true);
  $.post("/reader/annotation/" + itemId, { body: text }, function (data) {
    annotations.push(data.annotation);
    input.val("");
    renderNotes();
    updateAnnotationStatus();
  })
    .fail(function (xhr) {
      console.error("[annotations] note create failed:", xhr.status);
      setAnnotationStatus("Could not save note");
    })
    .always(function () {
      button.prop("disabled", false);
    });
}

function deleteAnnotation(id) {
  $.ajax({
    type: "DELETE",
    url: "/reader/annotation/" + id,
    success: function () {
      annotations = annotations.filter(function (a) {
        return a.id !== id;
      });
      renderHighlights();
      renderNotes();
      updateAnnotationStatus();
    },
    error: function (xhr) {
      console.error("[annotations] delete failed:", xhr.status);
      setAnnotationStatus("Could not delete annotation");
    },
  });
}

//
// Highlight Rendering
//

function clearHighlights() {
  if (window.CSS && CSS.highlights) {
    CSS.highlights.delete("llar-annotation");
  }
  highlightRanges.clear();
}

function createRangeFromOffsets(container, start, end) {
  var walker = document.createTreeWalker(
    container,
    NodeFilter.SHOW_TEXT,
    null,
    false,
  );
  var pos = 0;
  var startNode = null,
    startOffset = 0,
    endNode = null,
    endOffset = 0;

  while (walker.nextNode()) {
    var node = walker.currentNode;
    var len = node.textContent.length;
    if (!startNode && pos + len > start) {
      startNode = node;
      startOffset = start - pos;
    }
    if (pos + len >= end) {
      endNode = node;
      endOffset = end - pos;
      break;
    }
    pos += len;
  }

  if (!startNode || !endNode) return null;
  try {
    var range = document.createRange();
    range.setStart(startNode, startOffset);
    range.setEnd(endNode, endOffset);
    return range;
  } catch (e) {
    return null;
  }
}

function normalizeWS(s) {
  return s.replace(/\s+/g, " ");
}

// Build a regex from text that treats any whitespace in the needle as \s+
function textToFlexibleRegex(text) {
  var parts = text.split(/\s+/);
  var escaped = parts.map(function (p) {
    return p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  });
  return new RegExp(escaped.join("\\s+"));
}

function findByQuoteSelector(container, quote) {
  var fullText = getContainerText(container);

  // Prefer an exact occurrence whose surrounding text also matches. This
  // disambiguates repeated phrases when the DOM changed between devices.
  var idx = fullText.indexOf(quote.exact);
  var bestIdx = -1;
  var bestScore = -1;
  while (idx !== -1) {
    var prefix = quote.prefix || "";
    var suffix = quote.suffix || "";
    var actualPrefix = fullText.substring(
      Math.max(0, idx - prefix.length),
      idx,
    );
    var actualSuffix = fullText.substring(
      idx + quote.exact.length,
      idx + quote.exact.length + suffix.length,
    );
    var score =
      (prefix && actualPrefix === prefix ? 1 : 0) +
      (suffix && actualSuffix === suffix ? 1 : 0);
    if (score > bestScore) {
      bestIdx = idx;
      bestScore = score;
    }
    idx = fullText.indexOf(quote.exact, idx + 1);
  }
  if (bestIdx !== -1) {
    return createRangeFromOffsets(
      container,
      bestIdx,
      bestIdx + quote.exact.length,
    );
  }

  // Flexible whitespace match: turn the exact text into a regex
  // where whitespace runs match any whitespace
  var regex = textToFlexibleRegex(quote.exact);
  var match = regex.exec(fullText);
  if (match) {
    return createRangeFromOffsets(
      container,
      match.index,
      match.index + match[0].length,
    );
  }

  return null;
}

function firstVisibleTextOffset(container) {
  var viewport = readingViewport(container);
  var containerRect = container.getBoundingClientRect();
  var horizontal = readingUsesHorizontalColumns(container);
  var rectIsVisible = function (rect) {
    return (
      rect.bottom >= viewport.top &&
      rect.top <= viewport.bottom &&
      (!horizontal ||
        (rect.right >= containerRect.left && rect.left <= containerRect.right))
    );
  };
  var walker = document.createTreeWalker(
    container,
    NodeFilter.SHOW_TEXT,
    null,
    false,
  );
  var globalOffset = 0;

  while (walker.nextNode()) {
    var node = walker.currentNode;
    var text = node.textContent;
    if (text.trim().length > 0) {
      var nodeRange = document.createRange();
      nodeRange.selectNodeContents(node);
      if (Array.from(nodeRange.getClientRects()).some(rectIsVisible)) {
        if (horizontal) {
          for (var i = 0; i < text.length; i++) {
            if (/\s/.test(text.charAt(i))) continue;
            var visibleCharRange = document.createRange();
            visibleCharRange.setStart(node, i);
            visibleCharRange.setEnd(node, Math.min(i + 1, text.length));
            if (
              Array.from(visibleCharRange.getClientRects()).some(rectIsVisible)
            ) {
              return globalOffset + i;
            }
          }
          globalOffset += text.length;
          continue;
        }
        var low = 0;
        var high = text.length - 1;
        while (low < high) {
          var mid = Math.floor((low + high) / 2);
          var charRange = document.createRange();
          charRange.setStart(node, mid);
          charRange.setEnd(node, Math.min(mid + 1, text.length));
          if (charRange.getBoundingClientRect().bottom >= viewport.top) {
            high = mid;
          } else {
            low = mid + 1;
          }
        }
        while (low < text.length && /\s/.test(text.charAt(low))) low++;
        return Math.min(globalOffset + low, globalOffset + text.length - 1);
      }
    }
    globalOffset += text.length;
  }
  return Math.max(0, getContainerText(container).length - 1);
}

function readingCheckpointSelector(container) {
  var fullText = getContainerText(container);
  if (!fullText.length) return null;
  var start = firstVisibleTextOffset(container);
  var end = Math.min(fullText.length, start + 96);
  var prefixStart = Math.max(0, start - 32);
  var suffixEnd = Math.min(fullText.length, end + 32);
  return {
    selector: {
      position: { type: "TextPositionSelector", start: start, end: end },
      quote: {
        type: "TextQuoteSelector",
        exact: fullText.substring(start, end),
        prefix: fullText.substring(prefixStart, start),
        suffix: fullText.substring(end, suffixEnd),
      },
    },
    progress: start / fullText.length,
  };
}

function updateCheckpointControls(state) {
  var controls = $(".reading-checkpoint-tools").filter(function () {
    return String($(this).data("id")) === String(state.id);
  });
  if (!controls.length) return;

  var checkpoint = state.checkpoint;
  controls.find(".btn-resume-checkpoint,.btn-clear-checkpoint").remove();
  var save = controls.find(".btn-save-checkpoint");
  save
    .toggleClass("is-active", !!checkpoint)
    .attr("aria-pressed", checkpoint ? "true" : "false")
    .attr(
      "title",
      checkpoint ? "Update saved place" : "Save this reading position",
    )
    .attr(
      "aria-label",
      checkpoint ? "Update saved place" : "Save this reading position",
    );

  if (!checkpoint) return;
  if (checkpoint.selector) {
    var resume = $("<button>")
      .addClass(
        "reading-checkpoint-control reading-checkpoint-resume btn-resume-checkpoint",
      )
      .attr("type", "button")
      .attr("title", "Resume at " + Math.round(checkpoint.progress * 100) + "%")
      .attr("aria-label", "Scroll to the saved reading position")
      .data("selector", checkpoint.selector)
      .data("progress", checkpoint.progress)
      .html('<i class="fas fa-map-marker-alt" aria-hidden="true">\u2009</i>')
      .on("click", resumeReadingCheckpoint);
    controls.prepend(resume);
  }
  $("<button>")
    .addClass(
      "reading-checkpoint-control reading-checkpoint-clear btn-clear-checkpoint",
    )
    .attr("type", "button")
    .attr("title", "Clear the Continue Reading checkpoint")
    .attr("aria-label", "Clear saved place")
    .html('<i class="fas fa-times" aria-hidden="true">\u2009</i>')
    .on("click", clearReadingCheckpoint)
    .appendTo(controls);
}

function saveReadingCheckpoint(event) {
  event.preventDefault();
  var button = $(event.currentTarget);
  var container = document.getElementById("item-content-body");
  var checkpoint = container && readingCheckpointSelector(container);
  if (!checkpoint) return;
  var id = button.closest(".reading-checkpoint-tools").data("id");
  clearReaderStatus();
  setStateControlPending(button, true);
  requestItemState(id, "save-checkpoint", {
    selector: JSON.stringify(checkpoint.selector),
    progress: checkpoint.progress,
  })
    .done(function () {
      flashReadingLocation(container, checkpoint.selector);
    })
    .fail(function (xhr) {
      showReaderError(
        "Could not save your place. Your previous place is unchanged.",
        "reading-checkpoint",
        xhr,
      );
    })
    .always(function () {
      setStateControlPending(button, false);
    });
}

function checkpointRange(container, selector) {
  if (!container || !selector) return null;
  var range = null;
  if (selector.position) {
    range = createRangeFromOffsets(
      container,
      selector.position.start,
      selector.position.end,
    );
    if (
      range &&
      selector.quote &&
      normalizeWS(range.toString()) !== normalizeWS(selector.quote.exact)
    ) {
      range = null;
    }
  }
  if (!range && selector.quote) {
    range = findByQuoteSelector(container, selector.quote);
  }
  return range;
}

function flashReadingLocation(container, selector) {
  var range = checkpointRange(container, selector);
  flashReadingRange(range);
}

var checkpointFlashTimer = null;

function flashReadingRange(range) {
  if (!range) return;
  if (window.CSS && CSS.highlights && typeof Highlight !== "undefined") {
    CSS.highlights.set("llar-checkpoint-flash", new Highlight(range));
    window.clearTimeout(checkpointFlashTimer);
    checkpointFlashTimer = window.setTimeout(function () {
      CSS.highlights.delete("llar-checkpoint-flash");
    }, 1800);
    return;
  }
  var target = $(range.startContainer).closest("p,li,pre,blockquote,div");
  if (!target.length) target = $(range.startContainer.parentElement);
  target.addClass("checkpoint-resume-target");
  setTimeout(function () {
    target.removeClass("checkpoint-resume-target");
  }, 1800);
}

function clearReadingCheckpoint(event) {
  event.preventDefault();
  var button = $(event.currentTarget);
  var id = button.closest(".reading-checkpoint-tools").data("id");
  clearReaderStatus();
  setStateControlPending(button, true);
  requestItemState(id, "clear-checkpoint")
    .fail(function (xhr) {
      showReaderError(
        "Could not clear your saved place. Nothing changed.",
        "reading-checkpoint",
        xhr,
      );
    })
    .always(function () {
      setStateControlPending(button, false);
    });
}

function resumeReadingCheckpoint(event) {
  event.preventDefault();
  var button = $(event.currentTarget);
  var selector = button.data("selector");
  if (typeof selector === "string") selector = JSON.parse(selector);
  var container = document.getElementById("item-content-body");
  if (!container || !selector) return;

  var range = checkpointRange(container, selector);
  if (!range) {
    var fullText = getContainerText(container);
    var offset = Math.min(
      Math.max(0, Math.floor((button.data("progress") || 0) * fullText.length)),
      Math.max(0, fullText.length - 1),
    );
    range = createRangeFromOffsets(container, offset, offset + 1);
  }
  if (!range) return;

  var rect = range.getBoundingClientRect();
  if (readingUsesHorizontalColumns(container)) {
    var containerRect = container.getBoundingClientRect();
    container.scrollTo({
      left: Math.max(0, container.scrollLeft + rect.left - containerRect.left),
      behavior: readingScrollBehavior(),
    });
  } else {
    window.scrollTo({
      top: window.scrollY + rect.top - readingViewport(container).top,
      behavior: readingScrollBehavior(),
    });
  }
  flashReadingRange(range);
}

function resumeRequestedCheckpoint() {
  var params = new URLSearchParams(window.location.search);
  if (params.get("resume") !== "checkpoint") return;
  var button = $(".btn-resume-checkpoint").first();
  if (!button.length) return;

  var resume = function () {
    button.trigger("click");
  };
  if (document.fonts && document.fonts.ready) {
    document.fonts.ready.then(function () {
      window.requestAnimationFrame(resume);
    });
  } else {
    window.requestAnimationFrame(resume);
  }
}

$(window).on("load", resumeRequestedCheckpoint);

function renderHighlights() {
  clearHighlights();
  var container = document.getElementById("item-content-body");
  if (!container) return;

  var highlights = annotations.filter(function (a) {
    return a.selector != null;
  });

  var ranges = [];

  highlights.forEach(function (ann) {
    var sel = ann.selector;
    var range = null;

    // Primary: TextPositionSelector
    if (sel.position) {
      range = createRangeFromOffsets(
        container,
        sel.position.start,
        sel.position.end,
      );
      if (
        range &&
        sel.quote &&
        normalizeWS(range.toString()) !== normalizeWS(sel.quote.exact)
      ) {
        range = null;
      }
    }

    // Fallback: TextQuoteSelector
    if (!range && sel.quote) {
      range = findByQuoteSelector(container, sel.quote);
    }

    if (range) {
      highlightRanges.set(ann.id, range);
      ranges.push(range);
    }
  });

  if (
    ranges.length > 0 &&
    window.CSS &&
    CSS.highlights &&
    typeof Highlight !== "undefined"
  ) {
    var highlight = new Highlight(...ranges);
    CSS.highlights.set("llar-annotation", highlight);
  }
  renderHighlightLinks();
}

function renderHighlightLinks() {
  var list = $("#annotation-highlight-list");
  list.empty();
  var idx = 0;
  annotations.forEach(function (ann) {
    if (!ann.selector) return;
    idx++;
    var raw = ann.selector.quote ? ann.selector.quote.exact : "";
    var text = raw.replace(/\s+/g, " ").substring(0, 30);
    var item = $("<span>").addClass("reader-annotation-highlight-item");
    var range = highlightRanges.get(ann.id);
    var excerpt = text
      ? text + (text.length >= 30 ? "\u2026" : "")
      : "unavailable highlight";
    var label = idx + ": " + excerpt;
    var link;
    if (range) {
      link = $("<a>")
        .addClass("reader-annotation-jump")
        .attr("href", "#")
        .text(label)
        .on("click", function (e) {
          e.preventDefault();
          var rect = range.getBoundingClientRect();
          window.scrollTo({
            top: window.scrollY + rect.top - 80,
            behavior: readingScrollBehavior(),
          });
        });
    } else {
      item.addClass("is-unresolved");
      link = $("<span>")
        .addClass("reader-annotation-jump")
        .attr("title", "The saved text no longer matches this representation")
        .text(label);
    }
    var deleteButton = $("<button>")
      .addClass("btn reader-icon-button reader-annotation-delete")
      .attr("type", "button")
      .attr("title", "Delete highlight")
      .attr("aria-label", "Delete highlight " + idx)
      .html('<i class="fas fa-times" aria-hidden="true"></i>')
      .on("click", function () {
        deleteAnnotation(ann.id);
      });
    item.append(link).append(deleteButton);
    list.append(item);
  });
  if (idx > 0) list.show();
  else list.hide();
}

//
// Notes Panel
//

function renderNotes() {
  var notes = annotations.filter(function (a) {
    return a.selector == null && a.body != null;
  });
  var panel = $("#annotation-item-notes");
  var list = panel.find(".notes-list");
  list.empty();

  if (notes.length === 0) {
    $("#annotation-note-count").text("");
    panel.hide();
    return;
  }

  $("#annotation-note-count").text(
    notes.length + " note" + (notes.length === 1 ? "" : "s"),
  );

  notes.forEach(function (note) {
    var noteEl = $("<div>").addClass("reader-annotation-note");
    var textEl = $("<p>")
      .addClass("reader-annotation-note-text")
      .text(note.body);
    var delBtn = $("<button>")
      .addClass("btn reader-icon-button reader-annotation-delete")
      .attr("type", "button")
      .attr("title", "Delete note")
      .attr("aria-label", "Delete note")
      .html('<i class="fas fa-times" aria-hidden="true"></i>')
      .on("click", function () {
        deleteAnnotation(note.id);
      });
    noteEl.append(textEl).append(delBtn);
    list.append(noteEl);
  });

  if (annotationModeActive) {
    panel.show();
  }
}

//
// Export Functions
//

function exportFailureMessage(xhr) {
  try {
    return JSON.parse(xhr.responseText).error || "Export failed";
  } catch (e) {
    return "Export failed";
  }
}

function exportToZotero() {
  var itemId = getItemId();
  if (!itemId) return;
  var btn = $("#btn-export-zotero");
  clearReaderStatus();
  setStateControlPending(btn, true);
  $.post("/reader/export/" + itemId + "/zotero", function () {
    showExportFlash("success", "Exported to Zotero");
  })
    .fail(function (xhr) {
      showReaderError(exportFailureMessage(xhr), "item-export", xhr);
    })
    .always(function () {
      setStateControlPending(btn, false);
    });
}

function exportToUrlHandler() {
  var itemId = getItemId();
  if (!itemId) return;
  var btn = $("#btn-export-url-handler");
  clearReaderStatus();
  setStateControlPending(btn, true);
  $.getJSON("/reader/export/" + itemId + "/url-handler", function (data) {
    if (data.url) window.open(data.url, "_blank");
  })
    .fail(function (xhr) {
      showReaderError(exportFailureMessage(xhr), "item-export", xhr);
    })
    .always(function () {
      setStateControlPending(btn, false);
    });
}

function showExportFlash(type, message) {
  var flash = $("<div>")
    .addClass("alert alert-" + type + " position-fixed bottom-0 end-0 m-3")
    .css("z-index", 9999)
    .text(message)
    .appendTo("body");
  setTimeout(function () {
    flash.fadeOut(function () {
      flash.remove();
    });
  }, 3000);
}
