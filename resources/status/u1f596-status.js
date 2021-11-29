$(document).ready(function() {
    $('.datatable').DataTable({
	"paging":   false,
	"searching":   false,
    });
    var sources_datatable = $('#sources-datatable').DataTable({
	"paging":   false,
	"searching":   false,
    });
    var threads_datatable = $('#threads-datatable').DataTable({
	"paging":   false,
	"searching":   false,
    });

    $('#sources-datatable').on('click', 'td.details-control', function () {
        var tr = $(this).closest('tr');
        var row = sources_datatable.row(tr);
        var k = tr.children()[1].innerText;
        if (row.child.isShown()) {
            row.child.hide();
            tr.removeClass('shown');
        } else {
        	tr.addClass('shown');
            $.ajax({
                url: "/source-details/" + k
            })
              .done(function( msg ) {
              	row.child(msg).show();
            });
        }
    });
    $('#threads-datatable').on('click', 'td.details-control', function () {
        var tr = $(this).closest('tr');
        var row = threads_datatable.row(tr);
        if (row.child.isShown()) {
            row.child.hide();
            tr.removeClass('shown');
        } else {
            tr.addClass('shown');
	    row.child(tr.data("stacktrace")).show();
        }
    });
});
