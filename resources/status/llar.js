//
// utilities
//

function get_scroll_to_items() {
  return $("#item-content-body").find(
    "h1, h2, h3, h4, h5, h6, h7, p, pre, li, blockquote, img, iframe, div:not(:has(*))",
  );
}

function stateValueForTag(state, tag) {
  if (tag === "archive") return !!state.archived;
  return !!state[tag];
}

function updateStateButton(button, isSet) {
  var icon = button.find("i");
  var label = isSet ? button.data("label-set") : button.data("label-unset");
  button.data("is-set", isSet);
  button.attr("data-is-set", String(isSet));
  button.attr("title", label);
  button.attr("aria-label", label);
  icon.attr(
    "class",
    isSet ? button.data("icon-set") : button.data("icon-unset"),
  );
}

function itemNoLongerMatchesView(state) {
  var body = $("body");
  var view = body.data("view");
  var groupName = body.data("group-name");
  var groupItem = body.data("group-item");
  var filter = body.data("filter");

  if (view === "saved-overview" && !state.queued) return true;
  if (view === "continue-reading" && !state.checkpoint) return true;
  if (filter === "unread" && !state.unread) return true;
  if (groupName === "item-tags") {
    if (groupItem === "saved" && !state.saved) return true;
    if (groupItem === "archive" && !state.archived) return true;
    if (groupItem === "unread" && !state.unread) return true;
  }
  return false;
}

function applyItemState(state) {
  var id = state.id;
  $(".state-toggle").each(function () {
    var button = $(this);
    if (String(button.data("id")) === String(id)) {
      updateStateButton(
        button,
        stateValueForTag(state, String(button.data("tag"))),
      );
    }
  });

  updateCheckpointControls(state);

  if (itemNoLongerMatchesView(state)) {
    $("[data-item-root][data-id]").each(function () {
      if (String($(this).data("id")) === String(id)) {
        $(this).fadeOut(150, function () {
          $(this).remove();
        });
      }
    });
  }
}

function requestItemState(id, action, data) {
  return $.post(
    "/reader/item/by-id/" + id + "/state",
    Object.assign({ action: action }, data || {}),
  ).done(applyItemState);
}

function show_update_sources_update_result(title, message) {
  var popover_root = $(".btn-update-sources-in-view");
  popover_root.data("result-title", "Update done");
  popover_root.data(
    "result-message",
    `<div class="text-center">${message}</div>`,
  );
  popover_root.popover("show");
}

function update_sources_update_state(target) {
  $.getJSON(target, (result) => {
    if (result["done"]) {
      var items_url = $(".btn-update-sources-in-view").data("items");
      show_update_sources_update_result(
        "Update done",
        `<a href="${items_url}">refresh</a>`,
      );
    } else {
      setTimeout(update_sources_update_state, 5000, target);
    }
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

// document structure aware page forward scrolling
$(".item-content-body").ready(function () {
  //    var main_top = $("main").offset().top;
  if ($("#item-content-body").length) {
    var main_top = $("#item-content-body").offset().top;
    var main_bottom = window.innerHeight;
    var items = get_scroll_to_items();
    items.each(function () {
      var this_top = $(this)[0].getBoundingClientRect().top;
      var this_bottom = this_top + $(this).height();
      if (this_top >= main_top && this_bottom < main_bottom) {
        $(this).attr("view", "full");
      } else if (this_top < main_top && this_bottom < main_bottom) {
        $(this).attr("view", "partial-top");
      } else if (
        this_top >= main_top &&
        this_bottom >= main_bottom &&
        this_top + 20 < main_bottom
      ) {
        $(this).attr("view", "partial-bottom");
      } else {
        $(this).attr("view", "out");
      }
    });
    items.each(function () {
      $(this).removeClass("viewport-bottom");
    });
    var scroll_to = items.last();
    var candidate = items.filter('[view="out"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    candidate = items.filter('[view="partial-bottom"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    scroll_to.addClass("viewport-bottom");
  }
});

$(window).scroll(function () {
  var content_body = $("#item-content-body");
  if (content_body.length) {
    var main_top = content_body.offset().top;
    // var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;
    var items = get_scroll_to_items();
    items.each(function () {
      var this_top = $(this)[0].getBoundingClientRect().top + 10;
      var this_bottom = this_top + $(this).height();
      if (this_top >= main_top && this_bottom < main_bottom) {
        $(this).attr("view", "full");
      } else if (this_top < main_top && this_bottom < main_bottom) {
        $(this).attr("view", "partial-top");
      } else if (
        this_top >= main_top &&
        this_bottom >= main_bottom &&
        this_top + 20 < main_bottom
      ) {
        $(this).attr("view", "partial-bottom");
      } else {
        $(this).attr("view", "out");
      }
    });
    items.each(function () {
      $(this).removeClass("viewport-bottom");
    });
    var scroll_to = items.last();
    var candidate = items.filter('[view="out"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    candidate = items.filter('[view="partial-bottom"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    scroll_to.addClass("viewport-bottom");
  }
});

// keyboard navigation
$("body").keypress(function (event) {
  if ($("body").hasClass("modal-open")) {
    return;
  }
  if ($(event.target).is("input, textarea, select, [contenteditable]")) {
    return;
  }
  if ($("#item-content-body").length > 0) {
    var main_top = $("#item-content-body").offset().top;
    var main_bottom = window.innerHeight;
    var next_url = null;
    var scroll_to = null;
    var candidate = null;
    if (event.key == "n") {
      next_url = $("#btn-next-item").attr("href");
      if (next_url) {
        window.location.href = next_url;
      }
    } else if (event.key == "p") {
      window.history.back();
    } else if (event.key == "N") {
      $("#btn-tag-unread").trigger("click");
      next_url = $("#btn-next-item").attr("href");
      if (next_url) {
        window.location.href = next_url;
      }
    } else if (event.key == "P") {
      $("#btn-tag-unread").trigger("click");
      window.history.back();
    } else if (event.key == "a") {
      event.preventDefault();
      toggleAnnotationMode();
    } else if (event.which == 32) {
      // space
      event.preventDefault();
      var items = get_scroll_to_items();
      items.each(function () {
        var this_top = $(this)[0].getBoundingClientRect().top;
        var this_bottom = this_top + $(this).height();
        if (this_top >= main_top && this_bottom < main_bottom) {
          $(this).attr("view", "full");
        } else if (this_top < main_top && this_bottom < main_bottom) {
          $(this).attr("view", "partial-top");
        } else if (
          this_top >= main_top &&
          this_bottom >= main_bottom &&
          this_top < main_bottom
        ) {
          $(this).attr("view", "partial-bottom");
        } else {
          $(this).attr("view", "out");
        }
      });
      scroll_to = items.last();
      candidate = items.filter('[view="out"]');
      if (candidate.length > 0) {
        scroll_to = candidate.first();
      }
      candidate = items.filter('[view="partial-bottom"]');
      if (candidate.length > 0) {
        scroll_to = candidate.first();
      }
      event.preventDefault();
      scroll_to.addClass("viewport-pivot");
      $("body,html").animate({
        scrollTop: scroll_to.offset().top - main_top - 5,
      });
    }
  }
});

// Swipe left to advance through long content. Pointer Events cover touch and pen
// input without requiring a gesture library.
var main = document.querySelector("main");
if (main) {
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

    if (deltaX > -50 || Math.abs(deltaX) <= Math.abs(deltaY)) {
      return;
    }

    var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;

    if ($("body").hasClass("modal-open")) {
      return;
    }

    var items = get_scroll_to_items();
    items.each(function () {
      var this_top = $(this)[0].getBoundingClientRect().top;
      var this_bottom = this_top + $(this).height();
      if (this_top >= main_top && this_bottom < main_bottom) {
        $(this).attr("view", "full");
      } else if (this_top < main_top && this_bottom < main_bottom) {
        $(this).attr("view", "partial-top");
      } else if (
        this_top >= main_top &&
        this_bottom >= main_bottom &&
        this_top < main_bottom
      ) {
        $(this).attr("view", "partial-bottom");
      } else {
        $(this).attr("view", "out");
      }
    });
    var scroll_to = items.last();
    var candidate = items.filter('[view="out"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    candidate = items.filter('[view="partial-bottom"]');
    if (candidate.length > 0) {
      scroll_to = candidate.first();
    }
    scroll_to.addClass("viewport-pivot");
    $("body,html").animate({
      scrollTop: scroll_to.offset().top - main_top - 5,
    });
  });
}

$(document).ready(function () {
  $(".btn-mark-view-read").on("click", function () {
    var ids = Array.from(
      new Set(
        $("main")
          .find("[data-id]")
          .map(function () {
            return $(this).data("id");
          })
          .get(),
      ),
    );
    console.log(ids);
    for (var id of ids) {
      requestItemState(id, "seen");
    }
  });

  $(".btn-update-sources-in-view").popover({
    placement: "bottom",
    container: "body",
    offset: [10, 20],
    trigger: "manual",
    html: true,
    title: () => $(".btn-update-sources-in-view").data("result-title"),
    content: () => $(".btn-update-sources-in-view").data("result-message"),
  });
  // bookmark / document add url
  $(".bookmark-submit").on("click", function () {
    var x = $(this);
    x.removeClass("btn-warning");
    x.removeClass("btn-secondary");
    x.addClass("btn-info");
    $.post({
      url: "/reader/bookmark/add",
      data: { url: $(x.data("url-source")).val(), type: x.data("type") },
      dataType: "json",
      success: (data) => {
        x.removeClass("btn-warning");
        x.removeClass("btn-info");
        $(x.data("url-source")).val("");
        x.addClass("btn-secondary");
        var item = data["item"];
        var item_url =
          "/reader/group/type/bookmark/source" +
          "/" +
          item["meta"]["source-key"] +
          "/item/by-id" +
          "/" +
          item["id"];
        var source_list_url =
          "/reader/group/type/bookmark/source" +
          "/" +
          item["meta"]["source-key"] +
          "/items";
        show_bookmark_add_result(
          "Added: " + item["title"],
          `<a href="${item_url}">go</a>&nbsp;<a href="${source_list_url}">others</a>`,
        );
        return false;
      },
    }).fail((data) => {
      x.addClass("btn-warning");
      x.removeClass("btn-secondary");
      x.removeClass("btn-info");
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

  // main list: mark on view, toggle read
  var markReadOnViewDwellMs = 1000;
  var markReadOnViewTimers = new Map();
  var markReadObserver = null;

  function markReadOnView(element) {
    $(element).removeClass("option-mark-read-on-view");

    var x = $("#" + element.id + " .direct-tag-buttons .btn-tag-unread");
    if (!x.length || x.data("is-set") === false) {
      return;
    }

    requestItemState(x.data("id"), "seen");
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
  $(".btn-update-sources-in-view").on("click", function () {
    var target = $(this).data("target");
    $(this).find("i").addClass("icon-is-set");
    $.post(target, (data, status) => {
      if (status == "success") {
        setTimeout(update_sources_update_state, 5000, target);
      }
    });
  });

  // tag toggle buttons
  $(".ajax-toggle").on("click", function () {
    var x = $(this);
    var showing_list = $("#item-content-body").length == 0;
    var action = "set";
    if (x.data("is-set")) {
      action = "del";
    }
    var id = x.data("id");
    var tag = x.data("tag");
    $.post("/reader/item/by-id/" + id, { action: action, tag: tag }, () => {
      var icon = x.find("i");
      if (x.data("is-set")) {
        x.data("is-set", false);
        icon.attr("class", x.data("icon-unset"));
      } else {
        x.data("is-set", true);
        icon.attr("class", x.data("icon-set"));
      }
      if (!showing_list) {
        window.location.replace(window.location.href.replace(/mark=read/, ""));
      }
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
    var action = "set";
    var tag = $(this).find("input:first").val();
    var id = $(this).data("id");
    $.post(
      "/reader/item/by-id/" + id,
      {
        action: action,
        tag: tag,
      },
      () => location.reload(),
    );
  });

  // annotation mode
  $("#btn-annotation-mode").on("click", function () {
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
  var btn = $("#btn-annotation-mode");

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
  var topNav = document.getElementById("top-nav");
  var topBoundary = Math.max(
    0,
    container.getBoundingClientRect().top,
    topNav ? topNav.getBoundingClientRect().bottom : 0,
  );
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
      var rect = nodeRange.getBoundingClientRect();
      if (rect.bottom >= topBoundary && rect.top <= window.innerHeight) {
        var low = 0;
        var high = text.length - 1;
        while (low < high) {
          var mid = Math.floor((low + high) / 2);
          var charRange = document.createRange();
          charRange.setStart(node, mid);
          charRange.setEnd(node, Math.min(mid + 1, text.length));
          if (charRange.getBoundingClientRect().bottom >= topBoundary) {
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
    .toggleClass("btn-warning", !!checkpoint)
    .toggleClass("btn-outline-warning", !checkpoint)
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
      .addClass("btn btn-warning btn-resume-checkpoint")
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
    .addClass("btn btn-outline-secondary btn-clear-checkpoint")
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
    .fail(function (xhr) {
      console.error("[reading-checkpoint] save failed:", xhr.status);
    })
    .always(function () {
      button.prop("disabled", false);
    });
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
  window.scrollTo({
    top: window.scrollY + rect.top - 80,
    behavior: "smooth",
  });
  var target = $(range.startContainer).closest("p,li,pre,blockquote,div");
  target.addClass("checkpoint-resume-target");
  setTimeout(function () {
    target.removeClass("checkpoint-resume-target");
  }, 1800);
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
