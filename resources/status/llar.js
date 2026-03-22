//
// utilities
//

function get_scroll_to_items() {
  return $("#item-content-body").find(
    "h1, h2, h3, h4, h5, h6, h7, p, pre, li, blockquote, img, iframe, div:not(:has(*))",
  );
}

function tag_item_by_id(id, tag, icon_elem, action = "toggle") {
  var orig_action = action;
  if (action == "toggle") {
    action = "set";
    if (icon_elem.data("is-set")) {
      action = "del";
    }
  }
  $.post(
    "/reader/item/by-id/" + id,
    {
      action: action,
      tag: tag,
    },
    () => {
      var icon = icon_elem.find("i");
      if (orig_action == "toggle") {
        if (icon_elem.data("is-set")) {
          icon_elem.data("is-set", false);
          icon.attr("class", icon_elem.data("icon-unset"));
        } else {
          icon_elem.data("is-set", true);
          icon.attr("class", icon_elem.data("icon-set"));
        }
      } else if (orig_action == "del") {
        icon_elem.data("is-set", false);
        icon.attr("class", icon_elem.data("icon-unset"));
      } else if (orig_action == "set") {
        icon_elem.data("is-set", true);
        icon.attr("class", icon_elem.data("icon-set"));
      }
    },
  );
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

// gestures

function is_touch_device() {
  try {
    document.createEvent("TouchEvent");
    return true;
  } catch (e) {
    return false;
  }
}

if (is_touch_device()) {
  /* eslint no-undef: "off" */
  var main_swipe = new Hammer($("main")[0]);
  main_swipe.on("swipeleft", function (ev) {
    console.log(ev);
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
    event.preventDefault();
    scroll_to.addClass("viewport-pivot");
    $("body,html").animate({
      scrollTop: scroll_to.offset().top - main_top - 5,
    });
  });
}

$(document).ready(function () {
  $(".btn-mark-view-read").on("click", function () {
    var ids = jQuery.unique(
      $("main")
        .find("[data-id]")
        .map(function () {
          return $(this).data("id");
        }),
    );
    console.log(ids);
    for (var id of ids) {
      var icon_elem = $("[data-id=" + id + "].ajax-toggle+.btn-tag-unread");
      tag_item_by_id(id, "unread", icon_elem, "del");
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
    $("#" + target).html(`<div class="ratio ratio-16x9">
<iframe class="img-fluid" src="https://www.youtube.com/embed/${vid}" frameborder="0" allowfullscreen="true"></iframe></div>`);
  });

  // main list: mark on view, toggle read
  $(".option-mark-read-on-view").waypoint({
    offset: "bottom-in-view",
    handler: function () {
      var x = $("#" + this.element.id + " .direct-tag-buttons .btn-tag-unread");
      $.post(
        "/reader/item/by-id/" + x.data("id"),
        {
          action: "del",
          tag: x.data("tag"),
        },
        () => {
          var icon = x.find("i");
          x.data("is-set", false);
          icon.attr("class", x.data("icon-unset"));
        },
      );
    },
  });
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

  // First try exact match
  var idx = fullText.indexOf(quote.exact);
  if (idx !== -1) {
    return createRangeFromOffsets(container, idx, idx + quote.exact.length);
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
