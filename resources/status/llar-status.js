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
      dom: "Bfrtip",
      rowCallback: function (row, data) {
        set_row_color_by_status(row, data[1]);
      },
      columnDefs: [{ width: "40ch", targets: 2 }],
      buttons: [
        {
          text: '<i title="Refresh table" class="fas fa-refresh">&thinsp;</i>',
          action: function (e, dt) {
            console.log("Reloading");
            dt.ajax.reload();
          },
        },
      ],
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
