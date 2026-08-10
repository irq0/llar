function set_row_color_by_status(row, status) {
  if (status === "ok") {
    $(row).addClass("table-success");
  } else if (status != null && status.startsWith("temp-fail")) {
    $(row).addClass("table-warning");
  } else if (status === "updating") {
    $(row).addClass("table-info");
  } else if (status === "perm-fail") {
    $(row).addClass("table-danger");
  } else if (status === "bug") {
    $(row).addClass("table-dark");
  } else if (status === "new") {
    $(row).addClass("table-primary");
  }
}

function update_source_row(url, source_key, row, retries) {
  if (retries === undefined) retries = 0;
  if (retries > 120) return;
  $.getJSON(url, (result) => {
    var new_row = result["row"];
    var actions_html = sources_row_actions_html(source_key);
    var msg = "Updating";
    set_row_color_by_status(row.node(), result["update-status"]["result"]);
    if (result["update-status"]["done"]) {
      msg = "<p>Update: " + result["update-status"]["result"] + "</p>";
    } else {
      setTimeout(update_source_row, 1000, url, source_key, row, retries + 1);
    }
    new_row.push(actions_html + msg);
    row.data(new_row).draw();
  }).fail(function () {
    console.error("Failed to poll update status for " + source_key);
  });
}

function sources_row_actions_html(source_key) {
  return `
<a data-source-key="${source_key}" data-overwrite="false" class="btn-update-source">
<i title="Update" class="fas fa-angle-down"></i></a>
<a data-source-key="${source_key}" data-overwrite="true" class="btn-update-source">
<i title="Update, overwrite existing" class="fas fa-angle-double-down"></i></a>
<a data-source-key="${source_key}" class="btn-source-details">
<i title="Show state details" class="fas fa-info-circle"></i></a>`;
}

function initialize_datatables(container) {
  var sources_table = $(container).find("#sources-datatable");
  if (sources_table.length && !$.fn.DataTable.isDataTable(sources_table[0])) {
    sources_table.DataTable({
      paging: true,
      pageLength: 100,
      deferRender: true,
      searching: true,
      ajax: {
        url: "/api/sources",
        dataSrc: function (json) {
          var rows = json.data;
          for (var row = 0, row_count = rows.length; row < row_count; row++) {
            var source_key = rows[row][0];
            rows[row].push(sources_row_actions_html(source_key));
          }
          return rows;
        },
      },
      rowCallback: function (row, data) {
        set_row_color_by_status(row, data[1]);
      },
      columnDefs: [{ width: "40ch", targets: 2 }],
    });
  }

  $(container)
    .find(".datatable")
    .each(function () {
      if (!$.fn.DataTable.isDataTable(this)) {
        $(this).DataTable({
          paging: true,
          pageLength: 100,
          deferRender: true,
        });
      }
    });

  var threads_table = $(container).find("#threads-datatable");
  if (threads_table.length && !$.fn.DataTable.isDataTable(threads_table[0])) {
    threads_table.DataTable({
      paging: true,
      pageLength: 100,
      deferRender: true,
      searching: true,
    });
  }
}

var config_lab_state = {
  session_id: null,
  source_form: null,
  snapshots: {},
  snapshot_labels: {},
  fetch_result: null,
  selector_items: {},
  active_item: null,
  processed_result: null,
  running: false,
};

function config_lab_processors() {
  return {
    pre: $("#config-lab-pre").val(),
    filter: $("#config-lab-filter").val(),
    post: $("#config-lab-post").val(),
  };
}

function config_lab_input() {
  return {
    "source-form": $("#config-lab-source-form").val(),
    processors: config_lab_processors(),
  };
}

function config_lab_set_status(kind, message) {
  $("#config-lab-status")
    .removeClass(
      "alert-danger alert-secondary alert-success alert-info alert-warning",
    )
    .addClass("alert-" + kind)
    .text(message);
}

function config_lab_activate_tab(name) {
  var element = document.getElementById("config-lab-" + name + "-tab");
  if (element && window.bootstrap) new bootstrap.Tab(element).show();
}

function config_lab_store_snapshot(key, label, value) {
  config_lab_state.snapshots[key] = value;
  config_lab_state.snapshot_labels[key] = label;
  var select = $("#config-lab-data-root");
  if (!select.find('option[value="' + key + '"]').length) {
    select.append($("<option>").val(key).text(label));
  }
  select.val(key);
  config_lab_render_data();
}

function config_lab_error(response) {
  return response
    .json()
    .catch(function () {
      return { message: response.statusText };
    })
    .then(function (body) {
      $("#config-lab-status")
        .removeClass("alert-success alert-secondary alert-info")
        .addClass("alert-danger")
        .text(body.message || body.error || "Config Lab request failed");
      config_lab_store_snapshot("error", "Last error", body);
      throw body;
    });
}

function config_lab_request(path, options) {
  options = options || {};
  options.headers = Object.assign(
    { "Content-Type": "application/json", "X-LLAR-Config-Lab": "1" },
    options.headers || {},
  );
  return fetch(path, options).then(function (response) {
    if (!response.ok) return config_lab_error(response);
    return response.json();
  });
}

function config_lab_create_session() {
  if (config_lab_state.session_id) {
    return Promise.resolve(config_lab_state.session_id);
  }
  return config_lab_request("/api/config-lab/sessions", {
    method: "POST",
    body: JSON.stringify(config_lab_input()),
  }).then(function (session) {
    config_lab_state.session_id = session.id;
    return session.id;
  });
}

function config_lab_copy_text(value, message) {
  return navigator.clipboard.writeText(value).then(function () {
    config_lab_set_status("success", message);
  });
}

function config_lab_plain_object(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function config_lab_edn_key(key) {
  return /^[A-Za-z*+!_?.<>=][A-Za-z0-9*+!_?.<>=/-]*$/.test(key)
    ? ":" + key
    : JSON.stringify(key);
}

function config_lab_edn(value) {
  if (value === null || value === undefined) return "nil";
  if (typeof value === "string") return JSON.stringify(value);
  if (typeof value === "number" || typeof value === "boolean")
    return String(value);
  if (Array.isArray(value))
    return "[" + value.map(config_lab_edn).join(" ") + "]";
  if (config_lab_plain_object(value)) {
    return (
      "{" +
      Object.keys(value)
        .map(function (key) {
          return config_lab_edn_key(key) + " " + config_lab_edn(value[key]);
        })
        .join(" ") +
      "}"
    );
  }
  return JSON.stringify(String(value));
}

function config_lab_collection_summary(value) {
  var count = Array.isArray(value) ? value.length : Object.keys(value).length;
  var shape = Array.isArray(value)
    ? count
      ? "[…]"
      : "[]"
    : count
      ? "{…}"
      : "{}";
  return shape + "  " + count + (count === 1 ? " entry" : " entries");
}

function config_lab_scalar_summary(value, limit) {
  if (value === null || value === undefined) return "nil";
  if (typeof value !== "string") return String(value);
  if (value.length <= limit) return JSON.stringify(value);
  return JSON.stringify(value.slice(0, limit) + "…");
}

function config_lab_edn_summary(value, limit) {
  var edn = config_lab_edn(value);
  return edn.length <= limit ? edn : edn.slice(0, limit) + "…";
}

function config_lab_tree_key(key, parentIsVector) {
  if (key === null || key === undefined)
    return $("<span>").addClass("config-lab-tree-key");
  return $("<code>")
    .addClass("config-lab-tree-key")
    .text(parentIsVector ? "[" + key + "]" : config_lab_edn_key(String(key)));
}

function config_lab_tree_line(key, parentIsVector, valueNode) {
  return $("<span>")
    .addClass("config-lab-tree-line")
    .append($("<span>").addClass("config-lab-tree-marker"))
    .append(config_lab_tree_key(key, parentIsVector))
    .append(valueNode.addClass("config-lab-tree-value"));
}

function config_lab_tree_node(value, depth, key, parentIsVector) {
  var root = key === null || key === undefined;
  if (value === null || typeof value !== "object") {
    var kind = value === null ? "nil" : typeof value;
    if (typeof value === "string" && value.length > 180) {
      var longValue = $("<details>")
        .addClass("config-lab-tree-entry config-lab-tree-long-value")
        .append(
          $("<summary>").append(
            config_lab_tree_line(
              key,
              parentIsVector,
              $("<span>")
                .append(
                  $("<code>")
                    .addClass("config-lab-tree-string")
                    .text(config_lab_scalar_summary(value, 120)),
                )
                .append(
                  $("<span>")
                    .addClass("config-lab-tree-size ms-2")
                    .text(value.length.toLocaleString() + " chars"),
                ),
            ),
          ),
        );
      longValue.one("toggle", function () {
        if (this.open)
          longValue.append(
            $("<pre>").addClass("config-lab-tree-full-value").text(value),
          );
      });
      return longValue;
    }
    return $("<div>")
      .addClass(
        "config-lab-tree-entry config-lab-tree-leaf config-lab-tree-" + kind,
      )
      .append(
        config_lab_tree_line(
          key,
          parentIsVector,
          $("<code>").text(config_lab_scalar_summary(value, 180)),
        ),
      );
  }

  var details = $("<details>")
    .addClass("config-lab-tree-entry config-lab-tree-collection")
    .toggleClass("config-lab-tree-root", root)
    .prop("open", depth < 2);
  details.append(
    $("<summary>").append(
      config_lab_tree_line(
        key,
        parentIsVector,
        $("<span>")
          .addClass("config-lab-tree-shape")
          .text(config_lab_collection_summary(value)),
      ),
    ),
  );
  var children = $("<div>").addClass("config-lab-tree-children");
  details.append(children);
  var rendered = false;
  function renderChildren() {
    if (rendered) return;
    rendered = true;
    var entries = Array.isArray(value)
      ? value.map(function (entry, index) {
          return [index, entry];
        })
      : Object.keys(value).map(function (key) {
          return [key, value[key]];
        });
    entries.forEach(function (entry) {
      children.append(
        config_lab_tree_node(
          entry[1],
          depth + 1,
          entry[0],
          Array.isArray(value),
        ),
      );
    });
  }
  details.on("toggle", function () {
    if (this.open) renderChildren();
  });
  if (depth < 2) renderChildren();
  return details;
}

function config_lab_render_data() {
  var key = $("#config-lab-data-root").val();
  var value = config_lab_state.snapshots[key];
  var tree = $("#config-lab-data-tree").empty();
  if (value === undefined) {
    tree.append(
      $("<div>").addClass("text-muted").text("No data captured yet."),
    );
    return;
  }
  tree.append(config_lab_tree_node(value, 0, null, false));
}

function config_lab_rewrite_blob_urls(html) {
  if (!config_lab_state.session_id) return html || "";
  var base =
    "/api/config-lab/sessions/" + config_lab_state.session_id + "/blobs/";
  return (html || "").replace(/\/blob\/([0-9a-f]{64})/g, base + "$1");
}

function config_lab_preview_document(html, plainText) {
  var content = html || "";
  if (!content && plainText) {
    content = $("<div>").text(plainText).html().replace(/\n/g, "<br>");
  }
  content = config_lab_rewrite_blob_urls(content);
  var parsed = new DOMParser().parseFromString(content, "text/html");
  parsed
    .querySelectorAll(
      "script,meta,base,form,input,button,textarea,select,iframe,object,embed,link,source",
    )
    .forEach(function (element) {
      element.remove();
    });
  parsed.querySelectorAll("a").forEach(function (element) {
    element.removeAttribute("href");
    element.removeAttribute("target");
  });
  var blobPrefix =
    "/api/config-lab/sessions/" + config_lab_state.session_id + "/blobs/";
  parsed.querySelectorAll("img").forEach(function (element) {
    var src = element.getAttribute("src") || "";
    if (!src.startsWith(blobPrefix) && !src.startsWith("data:image/")) {
      element.removeAttribute("src");
    }
    element.removeAttribute("srcset");
  });
  content = parsed.body.innerHTML;
  return (
    '<!doctype html><html><head><meta charset="utf-8">' +
    "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; img-src 'self' data:; style-src 'unsafe-inline'; font-src 'none'; media-src 'self'; object-src 'none'; base-uri 'none'; form-action 'none'\">" +
    "<style>body{font-family:Georgia,serif;line-height:1.65;max-width:70ch;margin:1.5rem auto;padding:0 1rem;color:#212529}img{max-width:100%;height:auto}pre{white-space:pre-wrap;overflow-wrap:anywhere}a{color:#495057}iframe,video{max-width:100%}</style>" +
    "</head><body><article>" +
    content +
    "</article></body></html>"
  );
}

function config_lab_render_preview(item) {
  config_lab_state.active_item = item || null;
  if (!item) {
    $("#config-lab-preview-empty").removeClass("d-none");
    $("#config-lab-preview").addClass("d-none");
    $("#config-lab-open-original").addClass("d-none");
    return;
  }
  var title = item.title || item.url || "Untitled item";
  var authors = Array.isArray(item.authors)
    ? item.authors.join(", ")
    : item.authors;
  var metadata = [authors, item.timestamp].filter(Boolean).join(" · ");
  var contents = item.contents || {};
  $("#config-lab-result-title").text(title);
  $("#config-lab-preview-meta").text(metadata);
  $("#config-lab-preview-frame").attr(
    "srcdoc",
    config_lab_preview_document(contents["text/html"], contents["text/plain"]),
  );
  $("#config-lab-preview-empty").addClass("d-none");
  $("#config-lab-preview").removeClass("d-none");
  var externalUrl = null;
  try {
    externalUrl = new URL(item.url);
  } catch (_error) {}
  if (
    externalUrl &&
    (externalUrl.protocol === "http:" || externalUrl.protocol === "https:")
  ) {
    $("#config-lab-open-original")
      .attr("href", externalUrl.href)
      .removeClass("d-none");
  } else {
    $("#config-lab-open-original").removeAttr("href").addClass("d-none");
  }
  if (!item.url) {
    $("#config-lab-open-original").addClass("d-none");
  }
}

function config_lab_article_items() {
  var fetched = config_lab_state.fetch_result;
  if (!fetched) return [];
  if (fetched["url-selector"]) {
    return fetched["url-selector"].urls.map(function (url, index) {
      var cached = config_lab_state.selector_items[index];
      return {
        index: index,
        url: url,
        title: cached && cached.item && cached.item.title,
      };
    });
  }
  return (fetched.items || []).map(function (item, index) {
    return { index: index, url: item.url, title: item.title, item: item };
  });
}

function config_lab_render_articles() {
  var container = $("#config-lab-articles").empty();
  var items = config_lab_article_items();
  $("#config-lab-result-summary").text(
    items.length === 1
      ? "Found 1 article."
      : "Found " + items.length + " articles.",
  );
  if (!items.length) {
    container.append(
      $("<div>")
        .addClass("text-muted p-3")
        .text("No articles matched this source."),
    );
    return;
  }
  items.forEach(function (entry) {
    var button = $("<button>")
      .attr("type", "button")
      .addClass("list-group-item list-group-item-action config-lab-article")
      .attr("data-index", entry.index)
      .append(
        $("<div>")
          .addClass("fw-semibold text-break")
          .text(entry.title || entry.url || "Untitled item"),
      );
    if (entry.title && entry.url) {
      button.append(
        $("<div>").addClass("small text-muted text-break").text(entry.url),
      );
    }
    if (
      config_lab_state.active_item &&
      entry.url === config_lab_state.active_item.url
    ) {
      button.addClass("active");
    }
    container.append(button);
  });
}

function config_lab_trace_section(label, value) {
  if (!value) return $();
  return $("<details>")
    .addClass("mb-2")
    .append($("<summary>").addClass("fw-semibold").text(label))
    .append(
      $("<pre>")
        .addClass("border rounded bg-light p-2 mt-2 config-lab-code")
        .text(value),
    );
}

function config_lab_render_http() {
  var container = $("#config-lab-http").empty().removeClass("text-muted");
  var stages = [];
  if (config_lab_state.fetch_result)
    stages.push(["Source/index response", config_lab_state.fetch_result]);
  if (config_lab_state.active_selector_result)
    stages.push([
      "Selected article response",
      config_lab_state.active_selector_result,
    ]);
  if (!stages.length) {
    container.addClass("text-muted").text("HTTP traces appear after fetching.");
    return;
  }
  stages.forEach(function (stage) {
    var card = $("<section>").addClass("mb-4");
    card.append($("<h6>").text(stage[0]));
    if (stage[1].response)
      card.append(config_lab_tree_node(stage[1].response, 0, null, false));
    var trace = stage[1].trace || {};
    card
      .append(config_lab_trace_section("Raw response HTML", trace["raw-html"]))
      .append(
        config_lab_trace_section("After DOMPurify", trace["dompurify-html"]),
      )
      .append(config_lab_trace_section("Final LLAR HTML", trace["final-html"]));
    container.append(card);
  });
}

function config_lab_selector_data(diagnostic, field, open) {
  var nodes = diagnostic && diagnostic["selected-hickory"];
  if (!Array.isArray(nodes)) return null;
  var total = Number(diagnostic["match-count"] || 0);
  var truncated = diagnostic["selected-hickory-truncated?"] === true;
  var label = field === ":content" ? "Selected Hickory" : "Extractor input";
  var countLabel = truncated
    ? "first " + nodes.length + " of " + total + " nodes"
    : nodes.length + (nodes.length === 1 ? " node" : " nodes");
  var details = $("<details>")
    .addClass("config-lab-selector-data border rounded p-2 mb-2")
    .prop("open", open === true);
  details.append(
    $("<summary>")
      .addClass("fw-semibold")
      .append(label + " for ")
      .append($("<code>").text(field))
      .append(" — " + countLabel),
  );
  details.append(
    $("<p>")
      .addClass("small text-muted mt-2 mb-1")
      .text(
        field === ":content"
          ? "These are the Hickory nodes produced by the content selector."
          : "This vector is passed as the sole argument to the field extractor.",
      ),
  );
  if (truncated) {
    details.append(
      $("<p>")
        .addClass("small text-warning mb-1")
        .text(
          "The preview is truncated; the extractor receives all " +
            total +
            " nodes.",
        ),
    );
  }
  details.append(
    $("<div>")
      .addClass("config-lab-data-tree config-lab-selector-data-tree")
      .append(config_lab_tree_node(nodes, 0, null, false)),
  );
  return details;
}

function config_lab_match_previews(diagnostic) {
  if (!diagnostic.matches || !diagnostic.matches.length) return null;
  var details = $("<details>")
    .addClass("mt-2")
    .append($("<summary>").text("Rendered match previews"));
  diagnostic.matches.forEach(function (match) {
    details.append(
      $("<pre>")
        .addClass("border rounded bg-light p-2 mt-2 config-lab-code")
        .text(match),
    );
  });
  return details;
}

function config_lab_field_default(field) {
  return {
    title: "HTTP title, then the first h1/h2, then the article URL",
    author: "nil",
    ts: "the HTTP response timestamp",
    description: "nil",
    content: "the article body",
  }[field];
}

function config_lab_field_card(field, diagnostic) {
  var matches = Number(diagnostic["match-count"] || 0);
  var invalid = diagnostic["valid?"] === false;
  var card = $("<div>").addClass("col-12");
  var shell = $("<div>").addClass("card h-100 config-lab-field-card");
  var badges = $("<span>").addClass("d-flex gap-1");
  if (invalid)
    badges.append(
      $("<span>").addClass("badge text-bg-danger").text("Invalid output"),
    );
  badges.append(
    $("<span>")
      .addClass("badge text-bg-" + (matches ? "success" : "secondary"))
      .text(matches + (matches === 1 ? " match" : " matches")),
  );
  shell.append(
    $("<div>")
      .addClass(
        "card-header bg-white d-flex justify-content-between align-items-center",
      )
      .append(
        $("<code>")
          .addClass("fw-semibold")
          .text(":" + field),
      )
      .append(badges),
  );
  var body = $("<div>").addClass("card-body");
  body.append($("<div>").addClass("small fw-semibold mb-1").text("Selector"));
  if (diagnostic.selector) {
    body.append(
      $("<code>").addClass("d-block text-break mb-3").text(diagnostic.selector),
    );
  } else {
    body.append(
      $("<div>")
        .addClass("small text-muted mb-3")
        .append("Not configured — currently using ")
        .append(
          $("<span>").text(config_lab_field_default(field) || "the default"),
        )
        .append("."),
    );
  }
  if (invalid) {
    body.append(
      $("<div>")
        .addClass("alert alert-warning py-2 small")
        .append(
          $("<div>")
            .addClass("fw-semibold")
            .text(diagnostic["validation-error"] || "Invalid extractor output"),
        )
        .append(
          $("<div>").text(
            "Expected " +
              (diagnostic.expected || "a valid value") +
              "; received " +
              (diagnostic["actual-type"] || "an unknown type") +
              ".",
          ),
        ),
    );
  }
  body.append(
    $("<div>").addClass("small fw-semibold mb-1").text("Extracted value"),
  );
  body.append(
    $("<pre>")
      .addClass("config-lab-field-output border rounded bg-light p-2")
      .text(
        config_lab_edn_summary(
          diagnostic.value,
          field === "content" ? 1200 : 400,
        ),
      ),
  );
  if (diagnostic.selector || field === "content") {
    body.append(config_lab_selector_data(diagnostic, ":" + field, false));
  } else {
    body.append(
      $("<p>")
        .addClass("small text-muted mb-0")
        .append("Add ")
        .append($("<code>").text(":" + field))
        .append(" to the selectors map and run again to inspect its matches."),
    );
  }
  var previews = config_lab_match_previews(diagnostic);
  if (previews) body.append(previews);
  card.append(shell.append(body));
  return card;
}

function config_lab_url_selector_panel(urlSelector, open) {
  var panel = $("<details>")
    .addClass("config-lab-index-selector border rounded p-2")
    .prop("open", open === true)
    .append(
      $("<summary>")
        .addClass("fw-semibold")
        .text(
          "Index URL selection — " +
            urlSelector["match-count"] +
            " matches, " +
            urlSelector.urls.length +
            " URLs",
        ),
    );
  var body = $("<div>").addClass("pt-2");
  body.append(
    $("<code>").addClass("d-block text-break mb-2").text(urlSelector.selector),
  );
  body.append(config_lab_selector_data(urlSelector, ":urls", true));
  var previews = config_lab_match_previews(urlSelector);
  if (previews) body.append(previews);
  return panel.append(body);
}

function config_lab_render_selectors() {
  var container = $("#config-lab-selectors").empty().removeClass("text-muted");
  var fetched = config_lab_state.fetch_result;
  var urlSelector = fetched && fetched["url-selector"];
  if (!urlSelector) {
    container
      .addClass("text-muted")
      .text("This source does not use SelectorFeed diagnostics.");
    return;
  }
  var selected = config_lab_state.active_selector_result;
  if (!selected || !selected.fields) {
    container
      .append($("<h5>").text("Choose an article to design its extraction"))
      .append(
        $("<p>")
          .addClass("text-muted")
          .text(
            "The index selector works. Select one of its URLs in Articles to inspect title, author, timestamp, description, and content.",
          ),
      )
      .append(config_lab_url_selector_panel(urlSelector, true));
    return;
  }

  var item = selected.item || {};
  var heading = $("<div>").addClass(
    "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-3",
  );
  heading.append(
    $("<div>")
      .append($("<h5>").addClass("mb-1").text("Article extraction"))
      .append(
        $("<div>")
          .addClass("fw-semibold text-break")
          .text(item.title || selected.url || "Untitled article"),
      )
      .append(
        $("<div>")
          .addClass("small text-muted text-break")
          .text(selected.url || item.url || ""),
      ),
  );
  heading.append(
    $("<div>")
      .addClass("btn-group btn-group-sm")
      .append(
        $("<button>")
          .attr("type", "button")
          .addClass("btn btn-outline-secondary")
          .prop("disabled", !selected.item)
          .text("Preview article")
          .on("click", function () {
            config_lab_activate_tab("preview");
          }),
      )
      .append(
        $("<button>")
          .attr("type", "button")
          .addClass("btn btn-outline-secondary")
          .text("HTTP details")
          .on("click", function () {
            config_lab_activate_tab("http");
          }),
      ),
  );
  container.append(heading);
  if (selected["valid?"] === false) {
    container.append(
      $("<div>")
        .addClass("alert alert-warning")
        .append(
          $("<div>")
            .addClass("fw-semibold")
            .text("This article cannot become an item yet."),
        )
        .append(
          $("<div>").text(
            "The HTTP fetch and selectors succeeded. Fix the highlighted extractor output; the selected Hickory remains available below.",
          ),
        ),
    );
  }
  container.append(
    $("<p>")
      .addClass("small text-muted")
      .text(
        "Each card shows the selector, its output for this article, and the selected Hickory vector available to its extractor.",
      ),
  );
  var fields = selected.fields;
  var cards = $("<div>").addClass("row g-3 mb-4");
  ["title", "author", "ts", "description", "content"].forEach(function (field) {
    cards.append(config_lab_field_card(field, fields[field] || {}));
  });
  container.append(cards);
  container.append(config_lab_url_selector_panel(urlSelector, false));
}

function config_lab_render_item_panel(item, label) {
  var panel = $("<div>").addClass("col-lg-6");
  panel.append($("<h6>").text(label));
  if (!item) {
    return panel.append(
      $("<div>")
        .addClass("alert alert-warning")
        .text("Item removed by the filter."),
    );
  }
  panel.append(
    $("<div>")
      .addClass("small fw-semibold text-break mb-1")
      .text(item.title || item.url || "Untitled item"),
  );
  var contents = item.contents || {};
  panel.append(
    $("<iframe>")
      .addClass("config-lab-preview-frame config-lab-preview-frame-small")
      .attr("title", label + " item preview")
      .attr("sandbox", "allow-same-origin")
      .attr(
        "srcdoc",
        config_lab_preview_document(
          contents["text/html"],
          contents["text/plain"],
        ),
      ),
  );
  return panel;
}

function config_lab_render_transform() {
  var container = $("#config-lab-transform-result").empty();
  var result = config_lab_state.processed_result;
  if (!result || !result.items || !result.items.length) return;
  var activeUrl =
    config_lab_state.active_item && config_lab_state.active_item.url;
  var transformed = result.items.find(function (entry) {
    return entry.before && entry.before.url === activeUrl;
  });
  if (!transformed) {
    container.append(
      $("<div>")
        .addClass("text-muted")
        .text("Apply processors to the selected item to compare it here."),
    );
    return;
  }
  if (transformed.removed) {
    container.append(
      $("<div>")
        .addClass("alert alert-warning")
        .text("The filter removes this item."),
    );
  }
  container.append(
    $("<div>")
      .addClass("row g-3")
      .append(config_lab_render_item_panel(transformed.before, "Before"))
      .append(config_lab_render_item_panel(transformed.after, "After")),
  );
  if (transformed.after) config_lab_render_preview(transformed.after);
}

function config_lab_select_item(index, targetTab) {
  var fetched = config_lab_state.fetch_result;
  if (!fetched) return Promise.resolve();
  targetTab = targetTab || "preview";
  if (!fetched["url-selector"]) {
    var item = (fetched.items || [])[index];
    config_lab_render_preview(item);
    config_lab_render_articles();
    config_lab_activate_tab(targetTab);
    config_lab_render_transform();
    return Promise.resolve(item);
  }
  if (config_lab_state.selector_items[index]) {
    var cached = config_lab_state.selector_items[index];
    config_lab_state.active_selector_result = cached;
    config_lab_render_preview(cached.item);
    config_lab_render_articles();
    config_lab_render_selectors();
    config_lab_render_http();
    config_lab_render_transform();
    $("#config-lab-process").prop("disabled", !cached.item);
    if (!cached.item) {
      targetTab = "selectors";
      config_lab_set_status(
        "warning",
        "Article fetched; extraction needs fixes.",
      );
    }
    config_lab_activate_tab(targetTab);
    return Promise.resolve(cached);
  }
  config_lab_set_status(
    "info",
    "Fetching and extracting the selected article…",
  );
  return config_lab_request(
    "/api/config-lab/sessions/" +
      config_lab_state.session_id +
      "/selector-item",
    { method: "POST", body: JSON.stringify({ "item-index": Number(index) }) },
  ).then(function (result) {
    config_lab_state.selector_items[index] = result;
    config_lab_state.active_selector_result = result;
    config_lab_store_snapshot("item-" + index, "Article " + index, result);
    config_lab_render_preview(result.item);
    config_lab_render_articles();
    config_lab_render_selectors();
    config_lab_render_http();
    config_lab_render_transform();
    $("#config-lab-process").prop("disabled", !result.item);
    if (result.item) {
      config_lab_set_status("success", "Article fetched and extracted.");
    } else {
      targetTab = "selectors";
      config_lab_set_status(
        "warning",
        "Article fetched; extraction needs fixes.",
      );
    }
    config_lab_activate_tab(targetTab);
    return result;
  });
}

function config_lab_set_running(running) {
  config_lab_state.running = running;
  $("#config-lab-run").prop("disabled", running);
  $("#config-lab-refetch").prop(
    "disabled",
    running || !config_lab_state.fetch_result,
  );
  $("#config-lab-run-progress").text(running ? "Running…" : "");
}

function config_lab_run() {
  if (config_lab_state.running) return;
  config_lab_set_running(true);
  config_lab_set_status("info", "Compiling source…");
  config_lab_state.source_form = $("#config-lab-source-form").val();
  config_lab_create_session()
    .then(function (id) {
      return config_lab_request("/api/config-lab/sessions/" + id + "/compile", {
        method: "POST",
        body: JSON.stringify(config_lab_input()),
      });
    })
    .then(function (compiled) {
      config_lab_store_snapshot("compiled", "Compiled source", compiled);
      var keyInput = $("#config-lab-source-key");
      if (
        keyInput.attr("data-auto") === "true" &&
        compiled["suggested-source-key"]
      ) {
        keyInput.val(compiled["suggested-source-key"]);
      }
      $("#config-lab-copy, #config-lab-download").prop("disabled", false);
      config_lab_set_status("info", "Fetching source snapshot…");
      return config_lab_request(
        "/api/config-lab/sessions/" + config_lab_state.session_id + "/fetch",
        { method: "POST", body: "{}" },
      );
    })
    .then(function (fetched) {
      config_lab_state.fetch_result = fetched;
      config_lab_state.selector_items = {};
      config_lab_state.active_selector_result = null;
      config_lab_state.processed_result = null;
      $("#config-lab-process").prop("disabled", true);
      config_lab_store_snapshot("fetched", "Fetched source", fetched);
      config_lab_render_articles();
      config_lab_render_selectors();
      config_lab_render_http();
      $("#config-lab-refetch").prop("disabled", false);
      $("#config-lab-dirty")
        .removeClass("text-bg-warning text-bg-secondary")
        .addClass("text-bg-success")
        .text("Current");
      var items = config_lab_article_items();
      if (!items.length) {
        config_lab_render_preview(null);
        config_lab_set_status(
          "success",
          "Run completed, but no articles matched.",
        );
        config_lab_activate_tab(
          fetched["url-selector"] ? "selectors" : "articles",
        );
        return null;
      }
      return config_lab_select_item(0);
    })
    .then(function () {
      if (config_lab_state.active_item) {
        $("#config-lab-process").prop("disabled", false);
        config_lab_set_status(
          "success",
          "Run completed. Previewing the first article.",
        );
      }
    })
    .catch(function () {})
    .finally(function () {
      config_lab_set_running(false);
      if (config_lab_state.fetch_result)
        $("#config-lab-refetch").prop("disabled", false);
    });
}

function config_lab_export_request() {
  return fetch(
    "/api/config-lab/sessions/" + config_lab_state.session_id + "/export",
    {
      method: "POST",
      headers: { "Content-Type": "application/json", "X-LLAR-Config-Lab": "1" },
      body: JSON.stringify({
        "source-key": $("#config-lab-source-key").val(),
        "tags-form": $("#config-lab-tags").val(),
        "options-form": $("#config-lab-options").val(),
        processors: config_lab_processors(),
      }),
    },
  ).then(function (response) {
    if (!response.ok) return config_lab_error(response);
    return response.text();
  });
}

function initialize_config_lab(container) {
  var app = $(container).find("#config-lab-app");
  if (!app.length || app.data("initialized")) return;
  app.data("initialized", true);
  if (config_lab_state.source_form)
    $("#config-lab-source-form").val(config_lab_state.source_form);

  config_lab_request("/api/config-lab/status")
    .then(function (result) {
      $("#config-lab-login").prop("hidden", true);
      $("#config-lab-workbench").prop("hidden", false);
      config_lab_set_status("secondary", "Config Lab unlocked.");
      if (config_lab_state.fetch_result) {
        config_lab_render_articles();
        config_lab_render_preview(config_lab_state.active_item);
        config_lab_render_selectors();
        config_lab_render_http();
        Object.keys(config_lab_state.snapshots).forEach(function (key) {
          var select = $("#config-lab-data-root");
          select.append(
            $("<option>").val(key).text(config_lab_state.snapshot_labels[key]),
          );
        });
        config_lab_render_data();
      }
    })
    .catch(function () {
      $("#config-lab-status")
        .removeClass("alert-danger")
        .addClass("alert-secondary");
    });

  $("#config-lab-login-button").on("click", function () {
    fetch("/api/config-lab/login", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ token: $("#config-lab-token").val() }),
    }).then(function (response) {
      if (!response.ok) return config_lab_error(response);
      return response.json().then(function (result) {
        $("#config-lab-token").val("");
        $("#config-lab-login").prop("hidden", true);
        $("#config-lab-workbench").prop("hidden", false);
        config_lab_set_status("success", "Config Lab unlocked.");
      });
    });
  });

  $("#config-lab-source-form").on("input", function () {
    config_lab_state.source_form = $(this).val();
    $("#config-lab-dirty")
      .removeClass("text-bg-success text-bg-secondary")
      .addClass("text-bg-warning")
      .text("Changed — run again");
  });

  $("#config-lab-source-key").on("input", function () {
    $(this).attr("data-auto", "false");
  });

  $("#config-lab-run, #config-lab-refetch").on("click", config_lab_run);

  $("#config-lab-articles").on("click", ".config-lab-article", function () {
    var target = config_lab_state.fetch_result["url-selector"]
      ? "selectors"
      : "preview";
    config_lab_select_item(Number($(this).attr("data-index")), target).catch(
      function () {},
    );
  });

  $("#config-lab-process").on("click", function () {
    var id = config_lab_state.session_id;
    config_lab_request("/api/config-lab/sessions/" + id + "/process", {
      method: "POST",
      body: JSON.stringify({ processors: config_lab_processors() }),
    }).then(function (result) {
      config_lab_state.processed_result = result;
      config_lab_store_snapshot("processed", "Processed items", result);
      config_lab_render_transform();
      config_lab_set_status("success", "Processors applied in the sandbox.");
    });
  });

  $("#config-lab-data-root").on("change", config_lab_render_data);

  $("#config-lab-copy").on("click", function () {
    config_lab_export_request().then(function (form) {
      return config_lab_copy_text(form, "Copied .llar form to the clipboard.");
    });
  });

  $("#config-lab-download").on("click", function () {
    config_lab_export_request().then(function (form) {
      var link = document.createElement("a");
      link.href = URL.createObjectURL(
        new Blob([form], { type: "application/edn" }),
      );
      link.download = "config-lab.llar";
      link.click();
      URL.revokeObjectURL(link.href);
      config_lab_set_status("success", "Downloaded config-lab.llar.");
    });
  });
}

function load_dashboard_tab(target_selector) {
  var pane = $(target_selector);
  var tab_name = pane.data("tab-name");
  var placeholder = pane.find(".dashboard-tab-placeholder");

  if (!tab_name || tab_name === "overview" || pane.data("tab-loaded")) {
    return;
  }

  pane.data("tab-loaded", true);
  placeholder.text("Loading...");
  $.ajax({
    url: "/tab/" + tab_name,
  })
    .done(function (html) {
      pane.html(html);
      initialize_datatables(pane);
      initialize_config_lab(pane);
    })
    .fail(function () {
      pane.data("tab-loaded", false);
      placeholder
        .removeClass("text-muted")
        .addClass("text-danger")
        .text("Failed to load tab.");
    });
}

function reload_dashboard_tab(target_selector) {
  var pane = $(target_selector);
  var tab_name = pane.data("tab-name");

  if (!tab_name) {
    return;
  }

  if (
    tab_name === "sources" &&
    $("#sources-datatable").length &&
    $.fn.DataTable.isDataTable($("#sources-datatable")[0])
  ) {
    $("#sources-datatable").DataTable().ajax.reload();
    return;
  }

  pane.data("tab-loaded", false);
  pane.html(
    '<div class="dashboard-tab-placeholder text-muted">Loading...</div>',
  );
  $.ajax({
    url: "/tab/" + tab_name,
  })
    .done(function (html) {
      pane.data("tab-loaded", true);
      pane.html(html);
      initialize_datatables(pane);
      initialize_config_lab(pane);
    })
    .fail(function () {
      pane.data("tab-loaded", false);
      pane
        .find(".dashboard-tab-placeholder")
        .removeClass("text-muted")
        .addClass("text-danger")
        .text("Failed to reload tab.");
    });
}

$(document).ready(function () {
  initialize_datatables(document);

  $('a[data-bs-toggle="tab"]').on("click", function () {
    load_dashboard_tab($(this).attr("href"));
  });

  $('a[data-bs-toggle="tab"]').on("shown.bs.tab", function (event) {
    load_dashboard_tab($(event.target).attr("href"));
  });

  $("#dashboard-reload-tab").on("click", function () {
    reload_dashboard_tab($(".tab-pane.active").first());
  });

  $(document).on(
    "click",
    "#sources-datatable .btn-source-details",
    function () {
      var k = $(this).data("source-key");
      var tr = $(this).closest("tr");
      var sources_datatable = $("#sources-datatable").DataTable();
      var row = sources_datatable.row(tr);
      if (row.child.isShown()) {
        row.child.hide();
      } else {
        $.ajax({
          url: "/source-details/" + k,
        })
          .done(function (msg) {
            row.child(msg).show();
          })
          .fail(function () {
            row.child("<em>Failed to load details</em>").show();
          });
      }
    },
  );

  $(document).on("click", "#sources-datatable tr", function () {
    var tr = $(this);
    var sources_datatable = $("#sources-datatable").DataTable();
    var row = sources_datatable.row(tr);
    row.child.hide();
  });

  $(document).on("click", "#sources-datatable .btn-update-source", function () {
    var k = $(this).data("source-key");
    var overwrite = $(this).data("overwrite");
    var tr = $(this).closest("tr");
    var sources_datatable = $("#sources-datatable").DataTable();
    var row = sources_datatable.row(tr);
    var status_url = "/api/source/" + k;

    $.post("/api/update/" + k, { overwrite: overwrite })
      .done(function () {
        setTimeout(update_source_row, 1000, status_url, k, row);
      })
      .fail(function () {
        console.error("Failed to trigger update for " + k);
      });
  });

  $(document).on("click", ".btn-run-schedule", function () {
    var k = $(this).data("schedule-key");
    $.post("/api/schedule/" + k + "/run")
      .done(function () {
        reload_dashboard_tab($(".tab-pane.active").first());
      })
      .fail(function () {
        console.error("Failed to trigger schedule " + k);
      });
  });

  $(document).on("click", "#threads-datatable td.details-control", function () {
    console.log("Clicked");
    var tr = $(this).closest("tr");
    var threads_datatable = $("#threads-datatable").DataTable();
    var row = threads_datatable.row(tr);
    if (row.child.isShown()) {
      row.child.hide();
      tr.removeClass("shown");
    } else {
      tr.addClass("shown");
      row.child(tr.data("stacktrace")).show();
    }
  });
});
