// annotator

var anno = new annotator.App();
anno.include(annotator.ui.main);
anno.include(annotator.storage.http, {
    prefix: "/ans"
});

anno.include(function () {
    return {
	beforeAnnotationCreated: function(a) {
            a.item_id = $("#item-meta").data("id");
	}
    };
});

$(".item_content").ready(function () {
anno
    .start()
    .then(function() {
	anno.annotations.load({item_id: $("#item-meta").data("id")});
    });
});

//
// utilities
//

function get_scroll_to_items() {
    return $(".item-content-body").find(
	"h1, h2, h3, h4, h5, h6, h7, p, section," +
	    " pre, li, blockquote, img, iframe, div:not(:has(*))");
}


function tag_item_by_id(id, tag, action, icon_elem) {
    $.post("/reader/item/by-id/" + id,
	   {
	       "action": action,
	       "tag": tag
	   },
	   function(data) {
	       var icon = icon_elem.find('i');
	       if (icon_elem.data("is-set")) {
		   icon_elem.data("is-set", false);
		   icon.attr("class", icon_elem.data("icon-unset"));
	       } else {
		   icon_elem.data("is-set", true);
		   icon.attr("class", icon_elem.data("icon-set"));
	       }
	   });
}

//
// group action buttons
//
$("#btn-mark-view-read").click(function () {
    var ids = jQuery.unique($("main").find("[data-id]").map(function () {
	return $(this).data("id");
    }));

    for (id of ids) {
	var icon_elem = $("[data-id=" + id + "] .ajax-toggle+.btn-tag-unread");
	tag_item_by_id(id, "unread", "del", icon_elem);
    }
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
    $.post("/reader/item/by-id/" + x.data("id"),
	   {"action": action, "tag": x.data("tag")},
	   function(data) {
	       var icon = x.find('i');
	       if (x.data("is-set")) {
		   x.data("is-set", false);
		   icon.attr("class", x.data("icon-unset"));
	       } else {
		   x.data("is-set", true);
		   icon.attr("class", x.data("icon-set"));
	       }
	   });
});


// custom tag modal form
$("form.add-custom-tag").submit(function (event) {
    var action = "set";
    var tag = $(this).find('input:first').val();
    var id = $(this).data("id");
    $.post("/reader/item/by-id/" + id, {
	"action": action,
	"tag": tag});
});


// bookmark / document add url
$(".bookmark-submit").click(function () {
    var x = $(this);
    x.removeClass("btn-warning");
    x.removeClass("btn-secondary");
    x.addClass("btn-info");
    $.post({url: "/reader/bookmark/add",
            data: {"url": $(x.data("url-source")).val(),
                   "type": x.data("type")},
            success: function(data) {
		x.removeClass("btn-warning");
		x.removeClass("btn-info");
		$(x.data("url-source")).val("");
		x.addClass("btn-secondary");
		return false;
            }}).fail(function(data) {
		x.addClass("btn-warning");
		x.removeClass("btn-secondary");
		x.removeClass("btn-info");
            });
    return false;
});


// click on youtube preview image to start player
$(".lazy-youtube").click(function() {
    var vid = $(this).data("vid");
    var target = $(this).data("target");
    $("#" + target).html(
	"<iframe class=\"embed-responsive-item\"" +
	    " src=\"https://www.youtube.com/embed/" +
	    vid +
	    "\" allowfullscreen=\"true\">");
});


// main list: mark on view, toggle read
$(".option-mark-read-on-view").waypoint({
    offset: 'bottom-in-view',
    handler: function(direction) {
	var x = $('#' + this.element.id + " .direct-tag-buttons .btn-tag-unread");
	$.post("/reader/item/by-id/" + x.data("id"), {
            "action": "del",
            "tag": x.data("tag")},
               function(data) {
		   var icon = x.find('i');
		   x.data("is-set", false);
		   icon.attr("class", x.data("icon-unset"));
	       });
    }
});


// document structure aware page forward scrolling
$(".item-content-body").ready(function () {
    var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;
    var items = get_scroll_to_items();
    items.each(function (index) {
	var this_top = $(this)[0].getBoundingClientRect().top;
	var this_bottom = this_top + $(this).height();
	if (this_top >= main_top && this_bottom < main_bottom) {
	    $(this).attr("view", "full");
	} else if (this_top < main_top && this_bottom < main_bottom) {
	    $(this).attr("view", "partial-top");
	} else if (this_top >= main_top && this_bottom >= main_bottom && this_top + 20 < main_bottom) {
	    $(this).attr("view", "partial-bottom");
	} else {
	    $(this).attr("view", "out");
	}
    });
    items.each(function (index) {
	$(this).removeClass("viewport-bottom");
    });
    var scroll_to = items.last();
    var candidate = items.filter("[view=\"out\"]");
    if (candidate.length > 0) {
	scroll_to = candidate.first();
    }
    candidate = items.filter("[view=\"partial-bottom\"]");
    if (candidate.length > 0) {
	scroll_to = candidate.first();
    }
    scroll_to.addClass("viewport-bottom");
});

$(window).scroll(function () {
    var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;
    var items = get_scroll_to_items();
    items.each(function (index) {
	var this_top = $(this)[0].getBoundingClientRect().top + 10;
	var this_bottom = this_top + $(this).height();
	if (this_top >= main_top && this_bottom < main_bottom) {
	    $(this).attr("view", "full");
	} else if (this_top < main_top && this_bottom < main_bottom) {
	    $(this).attr("view", "partial-top");
	} else if (this_top >= main_top && this_bottom >= main_bottom && this_top + 20 < main_bottom) {
	    $(this).attr("view", "partial-bottom");
	} else {
	    $(this).attr("view", "out");
	}
    });
    items.each(function (index) {
	$(this).removeClass("viewport-bottom");
    });
    var scroll_to = items.last();
    var candidate = items.filter("[view=\"out\"]");
    if (candidate.length > 0) {
	scroll_to = candidate.first();
    }
    candidate = items.filter("[view=\"partial-bottom\"]");
    if (candidate.length > 0) {
	scroll_to = candidate.first();
    }
    scroll_to.addClass("viewport-bottom");
});


// keyboard navigation
$("body").keypress(function(event) {
    var main_top = $("main").offset().top;
    var main_bottom = window.innerHeight;
    // main list view
    if ($(".feed-item").length > 0) {
	content = $(".feed-item");
	if (event.which == 32 ) {
	    $(".feed-item").each(function (index) {
		var this_top = $(this)[0].getBoundingClientRect().top;
		var this_bottom = this_top + $(this).height();
		if (this_top >= main_top && this_bottom < main_bottom) {
		    $(this).attr("view", "full");
		} else if (this_top < main_top && this_bottom < main_bottom) {
		    $(this).attr("view", "partial-top");
		} else if (this_top >= main_top && this_bottom >= main_bottom && this_top < main_bottom) {
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
	    var candidate = $(".feed-item[view=\"out\"]");
	    if (candidate.length > 0) {
		scroll_to = candidate.first();
	    }
	    candidate = $(".feed-item[view=\"partial-bottom\"]");
	    if (candidate.length > 0) {
		scroll_to = candidate.first();
	    }
	    event.preventDefault();
	    scroll_to.addClass("viewport-pivot");
	    $("body").animate({scrollTop: scroll_to.offset().top - main_top - 5});
	}
    // item content view
    } else if ($(".item-content-body").length > 0 && ! $(".annotator-widget textarea").is(":focus")) {
	content = $(".item-content-body");
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
	} else if (event.which == 32 && content.css("columns") != "auto auto" && content.css("column-width") != "auto") {
	    event.preventDefault();
	    content.animate({scrollLeft: content.scrollLeft() + content.width()});
	} else if (event.which == 32) { // space
	    event.preventDefault();
	    var items = get_scroll_to_items();
	    items.each(function (index) {
		var this_top = $(this)[0].getBoundingClientRect().top;
		var this_bottom = this_top + $(this).height();
		if (this_top >= main_top && this_bottom < main_bottom) {
		    $(this).attr("view", "full");
		} else if (this_top < main_top && this_bottom < main_bottom) {
		    $(this).attr("view", "partial-top");
		} else if (this_top >= main_top && this_bottom >= main_bottom && this_top < main_bottom) {
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
	    var candidate = items.filter("[view=\"out\"]");
	    if (candidate.length > 0) {
		scroll_to = candidate.first();
	    }
	    candidate = items.filter("[view=\"partial-bottom\"]");
	    if (candidate.length > 0) {
		scroll_to = candidate.first();
	    }
	    event.preventDefault();
	    scroll_to.addClass("viewport-pivot");
	    $("body").animate({scrollTop: scroll_to.offset().top - main_top - 5});
	}
    }
});