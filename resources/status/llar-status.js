function set_row_color_by_status(row, status) {
  if (status == "ok") {
    $(row).addClass("table-success");
  } else if (status != null && status.startsWith("temp-fail")) {
    $(row).addClass("table-warning");
  } else if (status == "updating") {
    $(row).addClass("table-info");
  } else if (status == "perm-fail") {
    $(row).addClass("table-danger");
  } else if (status == "bug") {
    $(row).addClass("table-dark");
  } else if (status == "new") {
    $(row).addClass("table-primary");
  }
}

function update_source_row(url, source_key, row) {
  $.getJSON(url, (result) => {
    var new_row = result["row"];
    var actions_html = sources_row_actions_html(source_key);
    var msg = "Updating";
    set_row_color_by_status(row.node(), result["update-status"]["result"]);
    if (result["update-status"]["done"]) {
      msg = "<p>Update: " + result["update-status"]["result"] + "</p>";
    } else {
      setTimeout(update_source_row, 1000, url, source_key, row);
    }
    new_row.push(actions_html + msg);
    row.data(new_row).draw();
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

$(document).ready(function () {
  $(".datatable").DataTable({
    paging: false,
    searching: false,
  });
  var sources_datatable = $("#sources-datatable").DataTable({
    paging: false,
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
  var threads_datatable = $("#threads-datatable").DataTable({
    paging: false,
    searching: false,
  });

  $("#sources-datatable").on("click", ".btn-source-details", function () {
    var k = $(this).data("source-key");
    var tr = $(this).closest("tr");
    var row = sources_datatable.row(tr);
    if (row.child.isShown()) {
      row.child.hide();
    } else {
      $.ajax({
        url: "/source-details/" + k,
      }).done(function (msg) {
        row.child(msg).show();
      });
    }
  });

  $("#sources-datatable").on("click", "tr", function () {
    var tr = $(this);
    var row = sources_datatable.row(tr);
    row.child.hide();
  });

  $("#sources-datatable").on("click", ".btn-update-source", function () {
    var k = $(this).data("source-key");
    var overwrite = $(this).data("overwrite");
    var tr = $(this).closest("tr");
    var row = sources_datatable.row(tr);
    var status_url = "/api/source/" + k;

    $.post("/api/update/" + k, { overwrite: overwrite }, (data, status) => {
      if (status == "success") {
        setTimeout(update_source_row, 1000, status_url, k, row);
      }
    });
  });

  $("#threads-datatable").on("click", "td.details-control", function () {
    console.log("Clicked");
    var tr = $(this).closest("tr");
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
