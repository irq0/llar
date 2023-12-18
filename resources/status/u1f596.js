//
// utilities
//

function get_scroll_to_items() {
  return $(".item-content-body").find(
    "h1, h2, h3, h4, h5, h6, h7, p, pre, li, blockquote, img, iframe, div:not(:has(*))",
  );
}

function tag_item_by_id(id, tag, icon_elem) {
  var action = "set";
  if (icon_elem.data("is-set")) {
    action = "del";
  }
  $.post(
    "/reader/item/by-id/" + id,
    {
      action: action,
      tag: tag,
    },
    function (data) {
      var icon = icon_elem.find("i");
      if (icon_elem.data("is-set")) {
        icon_elem.data("is-set", false);
        icon.attr("class", icon_elem.data("icon-unset"));
      } else {
        icon_elem.data("is-set", true);
        icon.attr("class", icon_elem.data("icon-set"));
      }
    },
  );
}

//
// group action buttons
//

$(".btn-mark-view-read").click(function () {
  var ids = jQuery.unique(
    $("main")
      .find("[data-id]")
      .map(function () {
        return $(this).data("id");
      }),
  );

  for (id of ids) {
    var icon_elem = $("[data-id=" + id + "].ajax-toggle+.btn-tag-unread");
    tag_item_by_id(id, "unread", icon_elem);
  }
});

$(".btn-update-sources-in-view").popover({
  placement: "bottom",
  container: "body",
  offset: 10,
  trigger: "manual",
  html: true,
  title: function () {
    return $(".btn-update-sources-in-view").data("result-title");
  },
  content: function () {
    return $(".btn-update-sources-in-view").data("result-message");
  },
});

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
  $.getJSON(target, function (result) {
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

$(".btn-update-sources-in-view").click(function () {
  var target = $(this).data("target");
  $.post(target, function (data, status) {
    if (status == "success") {
      setTimeout(update_sources_update_state, 5000, target);
    }
  });
});

//
// tagging
//

// tag toggle buttons
$(".ajax-toggle").click(function () {
  var action = "set";
  if ($(this).data("is-set")) {
    action = "del";
  }
  var x = $(this);
  $.post(
    "/reader/item/by-id/" + x.data("id"),
    { action: action, tag: x.data("tag") },
    function (data) {
      var icon = x.find("i");
      if (x.data("is-set")) {
        x.data("is-set", false);
        icon.attr("class", x.data("icon-unset"));
      } else {
        x.data("is-set", true);
        icon.attr("class", x.data("icon-set"));
      }
    },
  );
});

// custom tag modal form
$("form.add-custom-tag").submit(function (event) {
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
    function () {
      location.reload();
    },
  );
});

function show_bookmark_add_result(title, message) {
  var popover_root = $("#add-thing");
  popover_root.data("result-title", title);
  popover_root.data(
    "result-message",
    `<div class="text-center">${message}</div>`,
  );
  popover_root.popover("show");
}

// bookmark / document add url
$(".bookmark-submit").click(function () {
  var x = $(this);
  x.removeClass("btn-warning");
  x.removeClass("btn-secondary");
  x.addClass("btn-info");
  $.post({
    url: "/reader/bookmark/add",
    data: { url: $(x.data("url-source")).val(), type: x.data("type") },
    dataType: "json",
    success: function (data) {
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
  }).fail(function (data) {
    x.addClass("btn-warning");
    x.removeClass("btn-secondary");
    x.removeClass("btn-info");
    show_bookmark_add_result("Fail", data);
  });
  return false;
});

$("#add-thing").popover({
  placement: "right",
  container: "body",
  offset: 10,
  boundary: $("#groupnav"),
  trigger: "manual",
  html: true,
  title: function () {
    return $("#add-thing").data("result-title");
  },
  content: function () {
    return $("#add-thing").data("result-message");
  },
});

// click on youtube preview image to start player
$(".lazy-youtube").click(function () {
  var vid = $(this).data("vid");
  var target = $(this).data("target");
  $("#" + target).html(
    '<iframe class="embed-responsive-item"' +
      ' src="https://www.youtube.com/embed/' +
      vid +
      '" allowfullscreen="true">',
  );
});

// main list: mark on view, toggle read
$(".option-mark-read-on-view").waypoint({
  offset: "bottom-in-view",
  handler: function (direction) {
    var x = $("#" + this.element.id + " .direct-tag-buttons .btn-tag-unread");
    console.log(x);
    $.post(
      "/reader/item/by-id/" + x.data("id"),
      {
        action: "del",
        tag: x.data("tag"),
      },
      function (data) {
        var icon = x.find("i");
        x.data("is-set", false);
        icon.attr("class", x.data("icon-unset"));
      },
    );
  },
});

// document structure aware page forward scrolling
$("#item-content-body").ready(function () {
  //    var main_top = $("main").offset().top;
  if ($("#item-content-body").length) {
    var main_top = $("#item-content-body").offset().top;
    var main_bottom = window.innerHeight;
    var items = get_scroll_to_items();
    items.each(function (index) {
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
    items.each(function (index) {
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
    items.each(function (index) {
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
    items.each(function (index) {
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
  var main_top = $("#item-content-body").offset().top;
  // var main_top = $("main").offset().top;
  var main_bottom = window.innerHeight;

  if ($("body").hasClass("modal-open")) {
    return;
  }

  // main list view
  if ($(".feed-item").length > 0) {
    content = $(".feed-item");
    if (event.which == 32) {
      $(".feed-item").each(function (index) {
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
      // debugging: highlight view attribute with color
      // $(".feed-item").each(function (index) {
      //    $(this).css("background-color", "white");
      //    if ($(this).attr("view") == "out")  {
      //      $(this).css("background-color", "red");
      //    }
      //    if ($(this).attr("view") == "partial-top")  {
      //      $(this).css("background-color", "blue");
      //    }
      //    if ($(this).attr("view") == "partial-bottom")  {
      //      $(this).css("background-color", "yellow");
      //    }
      //    if ($(this).attr("view") == "full")  {
      //      $(this).css("background-color", "green");
      //    }
      // });
      var scroll_to = $(".feed-item").last();
      var candidate = $('.feed-item[view="out"]');
      if (candidate.length > 0) {
        scroll_to = candidate.first();
      }
      candidate = $('.feed-item[view="partial-bottom"]');
      if (candidate.length > 0) {
        scroll_to = candidate.first();
      }
      event.preventDefault();
      scroll_to.addClass("viewport-pivot");
      $("body").animate({ scrollTop: scroll_to.offset().top - main_top - 5 });
    }
    // item content view
  } else if ($("#item-content-body").length > 0) {
    content = $("#item-content-body");
    if (event.key == "n") {
      var next_url = $("#btn-next-item").attr("href");
      if (next_url) {
        window.location.href = next_url;
      }
    } else if (event.key == "p") {
      window.history.back();
    } else if (event.key == "N") {
      $("#btn-tag-unread").trigger("click");
      var next_url = $("#btn-next-item").attr("href");
      if (next_url) {
        window.location.href = next_url;
      }
    } else if (event.key == "P") {
      $("#btn-tag-unread").trigger("click");
      window.history.back();
    } else if (
      event.which == 32 &&
      content.css("columns") != "auto auto" &&
      content.css("column-width") != "auto"
    ) {
      event.preventDefault();
      content.animate({ scrollLeft: content.scrollLeft() + content.width() });
    } else if (event.which == 32) {
      // space
      event.preventDefault();
      var items = get_scroll_to_items();
      items.each(function (index) {
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
      // debugging: highlight view attribute with color
      // items.each(function (index) {
      // 	$(this).css("background-color", "white");
      // 	if ($(this).attr("view") == "out")  {
      // 	    $(this).css("background-color", "red");
      // 	}
      // 	if ($(this).attr("view") == "partial-top")  {
      // 	    $(this).css("background-color", "blue");
      // 	}
      // 	if ($(this).attr("view") == "partial-bottom")  {
      // 	    $(this).css("background-color", "yellow");
      // 	}
      // 	if ($(this).attr("view") == "full")  {
      // 	    $(this).css("background-color", "green");
      // 	}
      // });
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
      $("body").animate({ scrollTop: scroll_to.offset().top - main_top - 5 });
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
  var main_swipe = new Hammer($("main")[0]);
  main_swipe.on("swipeleft", function (ev) {
    console.log(ev);
    var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;

    if ($("body").hasClass("modal-open")) {
      return;
    }

    var items = get_scroll_to_items();
    items.each(function (index) {
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
    $("body").animate({ scrollTop: scroll_to.offset().top - main_top - 5 });
  });
}
