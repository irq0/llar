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

    var selector = image.getAttribute("data-error-remove");
    var target = selector ? image.closest(selector) : image;
    if (target) target.remove();
  },
  true,
);

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
    .attr("data-unread", state.unread ? "true" : "false");
  updateCheckpointControls(state);
}

function requestItemState(id, action, data) {
  return $.post(
    "/reader/item/by-id/" + id + "/state",
    Object.assign({ action: action }, data || {}),
  ).done(applyItemState);
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

function show_bookmark_add_result(title, message) {
  var popover_root = $("#add-thing");
  popover_root.data("result-title", title);
  popover_root.data(
    "result-message",
    `<div class="text-center">${message}</div>`,
  );
  popover_root.popover("show");
  console.log(message);
}

// Document-structure-aware reading navigation. Space and touch gestures share
// this model, while saving a cross-device checkpoint remains explicit.
var readingNavigation = {
  container: null,
  blocks: [],
  target: null,
  frame: null,
  landingBlock: null,
  landingTimer: null,
};

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

function readableClientRects(block) {
  return Array.from(block.getClientRects()).filter(function (rect) {
    return rect.width > 0 && rect.height > 0;
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
  updateReadingLandingMarker();
}

function requestReadingNavigationUpdate() {
  if (readingNavigation.frame !== null) return;
  readingNavigation.frame = window.requestAnimationFrame(
    updateReadingNavigation,
  );
}

function refreshReadingBlocks() {
  if (!readingNavigation.container) return;
  readingNavigation.blocks = getReadingBlocks(readingNavigation.container);
  requestReadingNavigationUpdate();
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
      behavior: "smooth",
    });
  } else {
    var viewport = readingViewport(container);
    var verticalDelta = target.fallback
      ? (viewport.bottom - viewport.top) * 0.85
      : target.rect.top - viewport.top;
    window.scrollTo({
      top: window.scrollY + Math.max(1, verticalDelta),
      behavior: "smooth",
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
  // bookmark / document add url
  $(".bookmark-submit").on("click", function () {
    var x = $(this);
    x.removeClass("btn-warning");
    x.removeClass("btn-secondary");
    x.addClass("btn-primary");
    $.post({
      url: "/reader/bookmark/add",
      data: { url: $(x.data("url-source")).val(), type: x.data("type") },
      dataType: "json",
      success: (data) => {
        x.removeClass("btn-warning");
        x.removeClass("btn-primary");
        $(x.data("url-source")).val("");
        x.addClass("btn-secondary");
        var itemId = data["item-id"];
        var detail = itemId
          ? `<a href="/reader/item/by-id/${itemId}">open saved item</a>`
          : "It will appear in the reading queue after processing.";
        show_bookmark_add_result(data["message"] || "Saved to Llar", detail);
        return false;
      },
    }).fail((data) => {
      x.addClass("btn-warning");
      x.removeClass("btn-secondary");
      x.removeClass("btn-primary");
      show_bookmark_add_result("Fail", data.responseText);
    });
    return false;
  });

  $("#add-thing").popover({
    placement: "right",
    container: "body",
    offset: [10, 20],
    boundary: $("#groupnav"),
    trigger: "manual",
    html: true,
    title: () => $("#add-thing").data("result-title"),
    content: () => $("#add-thing").data("result-message"),
  });

  // click on youtube preview image to start player
  $(".lazy-youtube").on("click", function () {
    var vid = $(this).data("vid");
    var target = $(this).data("target");
    var embedUrl =
      "https://www.youtube-nocookie.com/embed/" + encodeURIComponent(vid);
    $("#" + target).html(
      `<iframe src="${embedUrl}" title="YouTube video player" ` +
        `referrerpolicy="strict-origin-when-cross-origin" ` +
        `allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" ` +
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
    button.data("state-request-pending", true).addClass("disabled");
    requestItemState(button.data("id"), action, { tag: button.data("tag") })
      .fail(function (xhr) {
        console.error("[item-tag] transition failed:", xhr.status);
      })
      .always(function () {
        button.data("state-request-pending", false).removeClass("disabled");
      });
  });

  $(".state-toggle").on("click", function (event) {
    event.preventDefault();
    var button = $(this);
    if (button.data("state-request-pending")) return;
    var action = button.data("is-set")
      ? button.data("action-unset")
      : button.data("action-set");
    button.data("state-request-pending", true).addClass("disabled");
    requestItemState(button.data("id"), action)
      .fail(function (xhr) {
        console.error("[item-state] transition failed:", xhr.status);
      })
      .always(function () {
        button.data("state-request-pending", false).removeClass("disabled");
      });
  });

  $(".state-action").on("click", function (event) {
    event.preventDefault();
    var button = $(this);
    if (button.data("state-request-pending")) return;
    button.data("state-request-pending", true).addClass("disabled");
    requestItemState(button.data("id"), button.data("action"))
      .fail(function (xhr) {
        console.error("[item-state] action failed:", xhr.status);
      })
      .always(function () {
        button.data("state-request-pending", false).removeClass("disabled");
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
    var tag = input.val().trim();
    if (!tag) return;
    requestItemState(form.data("id"), "add-tag", { tag: tag })
      .done(function () {
        input.val("");
      })
      .fail(function (xhr) {
        console.error("[item-tag] add failed:", xhr.status);
      });
  });

  // annotation mode
  $(".btn-annotation-mode").on("click", function () {
    toggleAnnotationMode();
  });

  $("#btn-highlight-selection").on("click", function () {
    createHighlight();
  });

  $("#btn-add-item-note").on("click", function () {
    createItemNote();
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

//
// Annotation Mode
//

var annotationModeActive = false;
var annotations = [];
var pendingSelector = null;
var highlightRanges = new Map();

function getItemId() {
  return $("#item-meta").data("id");
}

function toggleAnnotationMode() {
  if (!$("#item-content-body").length) return;
  annotationModeActive = !annotationModeActive;
  var btn = $(".btn-annotation-mode");

  if (annotationModeActive) {
    btn.addClass("active");
    $("#annotation-bottom-bar").show();
    $("#item-content-body").addClass("annotation-mode-active");
    loadAnnotations();
    $("#item-content-body").on("mouseup.annotation", onTextSelected);
    $("#item-content-body").on("click.annotation-delete", onHighlightClick);
  } else {
    btn.removeClass("active");
    $("#annotation-bottom-bar").hide();
    $("#annotation-item-notes").hide();
    $("#annotation-selection-actions").hide();
    $("#item-content-body").removeClass("annotation-mode-active");
    clearHighlights();
    $("#item-content-body").off("mouseup.annotation");
    $("#item-content-body").off("click.annotation-delete");
    pendingSelector = null;
  }
  $("body").toggleClass("reading-annotation-mode", annotationModeActive);
  setTimeout(refreshReadingBlocks, 0);
}

function loadAnnotations() {
  var itemId = getItemId();
  if (!itemId) return;
  $.getJSON("/reader/annotation/" + itemId, function (data) {
    annotations = data.annotations || [];
    renderHighlights();
    renderNotes();
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

function onTextSelected() {
  var sel = window.getSelection();
  if (!sel || sel.isCollapsed || sel.rangeCount === 0) {
    $("#annotation-selection-actions").hide();
    pendingSelector = null;
    return;
  }

  var range = sel.getRangeAt(0);
  var container = document.getElementById("item-content-body");
  if (!container || !container.contains(range.startContainer)) {
    return;
  }

  if (sel.toString().trim().length === 0) {
    $("#annotation-selection-actions").hide();
    pendingSelector = null;
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
}

//
// CRUD
//

function createHighlight() {
  if (!pendingSelector) return;
  var itemId = getItemId();
  $.post(
    "/reader/annotation/" + itemId,
    { selector: JSON.stringify(pendingSelector) },
    function (data) {
      annotations.push(data.annotation);
      window.getSelection().removeAllRanges();
      pendingSelector = null;
      $("#annotation-selection-actions").hide();
      renderHighlights();
    },
  ).fail(function (xhr) {
    console.error("[annotations] highlight create failed:", xhr.status);
  });
}

function createItemNote() {
  var input = $("#annotation-note-input");
  var text = input.val();
  if (!text || text.trim().length === 0) return;
  var itemId = getItemId();
  $.post("/reader/annotation/" + itemId, { body: text }, function (data) {
    annotations.push(data.annotation);
    input.val("");
    renderNotes();
  }).fail(function (xhr) {
    console.error("[annotations] note create failed:", xhr.status);
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
    },
    error: function (xhr) {
      console.error("[annotations] delete failed:", xhr.status);
    },
  });
}

//
// Highlight Rendering
//

function clearHighlights() {
  CSS.highlights.delete("llar-annotation");
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
      .html('<i class="fas fa-map-marker-alt">\u2009</i>')
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
    .html('<i class="fas fa-times">\u2009</i>')
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
  button.prop("disabled", true);
  requestItemState(id, "save-checkpoint", {
    selector: JSON.stringify(checkpoint.selector),
    progress: checkpoint.progress,
  })
    .done(function () {
      flashReadingLocation(container, checkpoint.selector);
    })
    .fail(function (xhr) {
      console.error("[reading-checkpoint] save failed:", xhr.status);
    })
    .always(function () {
      button.prop("disabled", false);
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
  button.prop("disabled", true);
  requestItemState(id, "clear-checkpoint")
    .fail(function (xhr) {
      console.error("[reading-checkpoint] clear failed:", xhr.status);
    })
    .always(function () {
      button.prop("disabled", false);
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
      behavior: "smooth",
    });
  } else {
    window.scrollTo({
      top: window.scrollY + rect.top - readingViewport(container).top,
      behavior: "smooth",
    });
  }
  flashReadingRange(range);
}

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

  if (ranges.length > 0) {
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
    if (!ann.selector || !highlightRanges.has(ann.id)) return;
    idx++;
    var raw = ann.selector.quote ? ann.selector.quote.exact : "";
    var text = raw.replace(/\s+/g, " ").substring(0, 30);
    var link = $("<a>")
      .addClass("badge bg-warning text-dark me-1")
      .attr("href", "#")
      .text(idx + ": " + text + (text.length >= 30 ? "\u2026" : ""))
      .on("click", function (e) {
        e.preventDefault();
        var range = highlightRanges.get(ann.id);
        if (range) {
          var rect = range.getBoundingClientRect();
          window.scrollTo({
            top: window.scrollY + rect.top - 80,
            behavior: "smooth",
          });
        }
      });
    list.append(link);
  });
  if (idx > 0) list.show();
  else list.hide();
}

function onHighlightClick(event) {
  if (!window.getSelection().isCollapsed) return;

  var caretPos = document.caretPositionFromPoint
    ? document.caretPositionFromPoint(event.clientX, event.clientY)
    : document.caretRangeFromPoint(event.clientX, event.clientY);

  if (!caretPos) return;

  var clickNode = caretPos.offsetNode || caretPos.startContainer;
  var clickOffset = caretPos.offset || caretPos.startOffset;

  for (var [annId, range] of highlightRanges) {
    if (range.isPointInRange(clickNode, clickOffset)) {
      deleteAnnotation(annId);
      return;
    }
  }
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
    panel.hide();
    return;
  }

  notes.forEach(function (note) {
    var noteEl = $("<div>").addClass(
      "d-flex justify-content-between align-items-start mb-1",
    );
    var textEl = $("<span>").text(note.body);
    var delBtn = $("<button>")
      .addClass("btn btn-sm btn-outline-danger ms-2")
      .html('<i class="fas fa-times"></i>')
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

function exportToZotero() {
  var itemId = getItemId();
  if (!itemId) return;
  var btn = $("#btn-export-zotero");
  btn.addClass("disabled");
  $.post("/reader/export/" + itemId + "/zotero", function () {
    btn.removeClass("disabled");
    showExportFlash("success", "Exported to Zotero");
  }).fail(function (xhr) {
    btn.removeClass("disabled");
    var msg = "Export failed";
    try {
      msg = JSON.parse(xhr.responseText).error;
    } catch (e) {
      /* ignore parse error */
    }
    showExportFlash("danger", msg);
  });
}

function exportToUrlHandler() {
  var itemId = getItemId();
  if (!itemId) return;
  $.getJSON("/reader/export/" + itemId + "/url-handler", function (data) {
    if (data.url) window.open(data.url, "_blank");
  }).fail(function (xhr) {
    var msg = "Export failed";
    try {
      msg = JSON.parse(xhr.responseText).error;
    } catch (e) {
      /* ignore parse error */
    }
    showExportFlash("danger", msg);
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
